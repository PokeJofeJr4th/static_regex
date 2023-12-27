#![warn(clippy::pedantic, clippy::nursery)]
use std::{
    collections::hash_map::DefaultHasher,
    fmt::{Debug, Display},
    hash::{Hash, Hasher},
};

use proc_macro::TokenStream;
use proc_macro2::{Span as Span2, TokenStream as TokenStream2};
use quote::quote;
use syn::{
    parse::{Parse, ParseStream},
    parse_macro_input, Expr, ExprLit, Lit, LitByteStr,
};

fn get_hash(h: &impl Hash) -> u64 {
    let mut state = DefaultHasher::new();
    h.hash(&mut state);
    state.finish()
}

fn vecu8_to_tokens(v: &[u8], span: Span2) -> TokenStream2 {
    let str_version = LitByteStr::new(v, span);
    quote! {#str_version}
}

/// # Panics
///
#[proc_macro]
#[allow(clippy::too_many_lines)]
pub fn static_regex(input: TokenStream) -> TokenStream {
    let regex = parse_macro_input!(input as RegexRepr);
    println!("{regex:?}");
    let hash_state = get_hash(&regex);
    let RegexRepr {
        mut body,
        pattern,
        span,
    } = regex;
    let mut fn_body = quote! {
        let mut idx: usize = 0;
    };
    let mut end_len = 0;
    if body.len() >= 2 {
        if let (Some(Segment::Str(end)), Some(Segment::End)) =
            (body.get(body.len() - 2), body.last())
        {
            // check if the string ends with the given string
            let end_lit = String::from_utf8_lossy(end);
            fn_body.extend(quote! {
                if !src.ends_with(#end_lit) {
                    return false;
                }
            });
            end_len = end.len();
            body.pop();
            body.pop();
            body.push(Segment::End);
        } else if let (Some(Segment::Char(Character::Char(end))), Some(Segment::End)) =
            (body.get(body.len() - 2), body.last())
        {
            // check if the string ends with the given character
            let end_lit = char::from_u32(u32::from(*end)).unwrap();
            fn_body.extend(quote! {
                if !src.ends_with(#end_lit) {
                    return false;
                }
            });
            body.pop();
            body.pop();
            body.push(Segment::End);
            end_len = 1;
        }
    }
    if body.get(0) == Some(&Segment::Start) {
        body.remove(0);
        if let Some(Segment::Str(start)) = body.get(0) {
            // check if the string starts with the given string
            let str_len = start.len();
            let start_lit = String::from_utf8_lossy(start);
            fn_body.extend(quote! {
                if !src.starts_with(#start_lit) {
                    return false;
                }
                idx += #str_len;
            });
            body.remove(0);
        } else if let Some(Segment::Char(Character::Char(start))) = body.get(0) {
            // check if the string starts with the given character
            let start_lit = char::from_u32(u32::from(*start)).unwrap();
            fn_body.extend(quote! {
                if !src.starts_with(#start_lit) {
                    return false;
                }
                idx += 1;
            });
            body.remove(0);
        }
        if body.is_empty() {
            fn_body.extend(quote! {true});
        } else {
            let start = body.remove(0);
            fn_body.extend(start.compile(
                hash_state,
                quote! {return false;},
                quote! {return true;},
                regex.span,
                end_len,
                body,
            ));
        }
    } else if body.is_empty() {
        fn_body.extend(quote! {true});
    } else {
        let main_loop_label = syn::Lifetime::new(&format!("'_main_loop_{hash_state:x}"), span);
        let start = body.remove(0);
        let loop_body = start.compile(
            hash_state,
            quote! {continue #main_loop_label;},
            quote! {return true;},
            regex.span,
            end_len,
            body,
        );
        fn_body.extend(quote! {
            #main_loop_label: for i in 0..src.len() {
                idx = i;
                #loop_body
            }
            return false;
        });
    }
    quote! {unsafe {
        use ::static_regex::CharacterClasses;
        #[allow(dead_code, unused_labels)]
        ::static_regex::Regex::new(
            #pattern,
            |src| {#fn_body}
        )
    }}
    .into()
}

#[derive(Debug)]
struct RegexRepr {
    body: Vec<Segment>,
    pattern: String,
    span: Span2,
}

impl Hash for RegexRepr {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.body.hash(state);
    }
}

#[derive(PartialEq, Eq, Clone, Copy)]
enum RegexErr {
    UnexpectedEof,
    UnclosedGroup,
    BadEscape(char),
}

impl Display for RegexErr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::UnexpectedEof => write!(f, "unexpected end of regular expression"),
            Self::UnclosedGroup => write!(f, "unclosed capture group"),
            Self::BadEscape(c) => write!(f, "bad escape sequence \"\\{c}\""),
        }
    }
}

impl Parse for RegexRepr {
    fn parse(input: ParseStream) -> Result<Self, syn::Error> {
        // println!("{input:?}");
        let Expr::Lit(ExprLit {
            attrs: _,
            lit: Lit::Str(lit_str),
        }) = input.parse()? else {
            return Err(syn::Error::new(input.span(), "Expected a string literal"));
        };

        let pattern = lit_str.value();
        let span = lit_str.span();

        let mut idx = 0;
        let mut body = Vec::new();
        while idx < pattern.len() {
            let segment = get_regex_section(&pattern, &mut idx)
                .map_err(|err| syn::Error::new(span, err.to_string()))?;
            if let (Some(Segment::Str(lhs)), &Segment::Char(Character::Char(rhs))) =
                (body.last_mut(), &segment)
            {
                // if the next segment is a regular character and the previous is a string, append them
                lhs.push(rhs);
                continue;
            }
            if let (
                Some(&Segment::Char(Character::Char(lhs))),
                &Segment::Char(Character::Char(rhs)),
            ) = (body.last(), &segment)
            {
                // if both the current and next segments are chars, make a new string
                body.pop();
                body.push(Segment::Str([lhs, rhs].into()));
                continue;
            }
            if let (
                Some(Segment::Str(lhs)),
                Segment::Quantified(
                    rhs,
                    Quantity {
                        min: min @ 1..,
                        max,
                    },
                ),
            ) = (body.last_mut(), &segment)
            {
                if let &Segment::Char(Character::Char(rhs_c)) = &**rhs {
                    // `ab+` => `abb*`
                    for _ in 0..*min {
                        lhs.push(rhs_c);
                    }
                    body.push(Segment::Quantified(
                        rhs.clone(),
                        Quantity {
                            min: 0,
                            max: max - min,
                        },
                    ));
                    continue;
                }
            }

            if let (
                Some(Segment::Quantified(
                    lhs,
                    Quantity {
                        min: min @ 1..,
                        max,
                    },
                )),
                Segment::Str(_),
            ) = (body.last_mut(), &segment)
            {
                // `a+bc` => `a*abc`
                if let Segment::Str(lhs) = &**lhs {
                    let Segment::Str(mut rhs) = segment else { unreachable!() };
                    *max -= *min;
                    for _ in 0..*min {
                        rhs.extend(lhs);
                    }
                    *min = 0;
                    body.push(Segment::Str(rhs));
                    continue;
                }
            }

            body.push(segment);
        }
        Ok(Self {
            body,
            pattern,
            span,
        })
    }
}

fn get_initial_section(s: &str, idx: &mut usize) -> Result<Segment, RegexErr> {
    let mut chars_iter = s.chars();
    let v = match chars_iter.nth(*idx) {
        Some('\\') => {
            *idx += 1;
            match chars_iter.next() {
                Some('s') => Ok(Segment::Char(Character::Space)),
                Some('S') => Ok(Segment::Char(Character::NonSpace)),
                Some('w') => Ok(Segment::Char(Character::Word)),
                Some('W') => Ok(Segment::Char(Character::NonWord)),
                Some('d') => Ok(Segment::Char(Character::Digit)),
                Some('D') => Ok(Segment::Char(Character::NonDigit)),
                Some('b') => Ok(Segment::Boundary),
                Some('B') => Ok(Segment::NonBoundary),
                Some('n') => Ok(Segment::Str((*b"\n").into())),
                Some('t') => Ok(Segment::Str((*b"\t").into())),
                Some('r') => Ok(Segment::Str((*b"\r").into())),
                Some(
                    c @ ('\\' | '(' | ')' | '{' | '}' | '[' | ']' | '+' | '*' | '?' | '.' | '^'
                    | '$'),
                ) => Ok(Segment::Char(Character::Char(c.try_into().unwrap()))),
                Some(c) => Err(RegexErr::BadEscape(c)),
                None => Err(RegexErr::UnexpectedEof),
            }
        }
        Some('^') => Ok(Segment::Start),
        Some('$') => Ok(Segment::End),
        Some('.') => Ok(Segment::Char(Character::Any)),
        Some('(') => {
            let mut sections = Vec::new();
            *idx += 1;
            loop {
                if s.chars().nth(*idx) == Some(')') {
                    break;
                }
                sections.push(get_regex_section(s, idx).map_err(|err| {
                    if err == RegexErr::UnexpectedEof {
                        RegexErr::UnclosedGroup
                    } else {
                        err
                    }
                })?);
            }
            Ok(Segment::Group(sections))
        }
        Some('[') => {
            let mut characters = Vec::new();
            let mut s_char_iter = s.chars().skip(*idx + 1);
            while let Some(c) = s_char_iter.next() {
                *idx += 1;
                match c {
                    ']' if !characters.is_empty() => {
                        break;
                    }
                    '-' if !characters.is_empty() => {
                        let Some(Character::Char(c)) = characters.pop() else {
                            return Err(RegexErr::BadEscape('-'));
                        };
                        let Some(nxt) = s_char_iter.next() else {
                            return Err(RegexErr::UnexpectedEof);
                        };
                        *idx += 1;
                        let nxt = nxt as u8;
                        characters.push(Character::Range(c, nxt));
                    }
                    c => characters.push(Character::Char(c as u8)),
                }
            }
            Ok(Segment::Bracket(characters))
        }
        Some(c @ (')' | '{' | '}' | ']' | '+' | '*' | '?')) => Err(RegexErr::BadEscape(c)),
        Some(c) => Ok(Segment::Char(Character::Char(c.try_into().unwrap()))),
        None => Err(RegexErr::UnexpectedEof),
    }?;
    *idx += 1;
    Ok(v)
}

fn get_regex_section(s: &str, idx: &mut usize) -> Result<Segment, RegexErr> {
    let v = get_initial_section(s, idx)?;
    let mut chars_iter = s.chars().skip(*idx);
    match chars_iter.next() {
        Some('+') => {
            *idx += 1;
            Ok(Segment::Quantified(
                Box::new(v),
                Quantity {
                    min: 1,
                    max: usize::MAX,
                },
            ))
        }
        Some('?') => {
            *idx += 1;
            Ok(Segment::Quantified(
                Box::new(v),
                Quantity { min: 0, max: 1 },
            ))
        }
        Some('*') => {
            *idx += 1;
            Ok(Segment::Quantified(
                Box::new(v),
                Quantity {
                    min: 0,
                    max: usize::MAX,
                },
            ))
        }
        Some('{') => {
            *idx += 1;
            let mut min = None;
            let mut int_buf = 0;
            loop {
                *idx += 1;
                match chars_iter.next() {
                    Some(',') => {
                        if min.is_some() {
                            return Err(RegexErr::BadEscape(','));
                        }
                        min = Some(int_buf);
                        int_buf = 0;
                    }
                    Some('}') => break,
                    Some(c) if c.is_ascii_digit() => {
                        int_buf *= 10;
                        int_buf += c.to_string().parse::<usize>().unwrap();
                    }
                    Some(c) => return Err(RegexErr::BadEscape(c)),
                    None => return Err(RegexErr::UnexpectedEof),
                }
            }
            match min {
                Some(min) => Ok(Segment::Quantified(
                    Box::new(v),
                    Quantity { min, max: int_buf },
                )),
                None => Ok(Segment::Quantified(
                    Box::new(v),
                    Quantity {
                        min: int_buf,
                        max: int_buf,
                    },
                )),
            }
        }
        _ => Ok(v),
    }
}

#[derive(Debug, Hash, PartialEq, Clone)]
enum Segment {
    Str(Vec<u8>),
    Char(Character),
    Quantified(Box<Segment>, Quantity),
    Group(Vec<Segment>),
    Bracket(Vec<Character>),
    Start,
    End,
    Boundary,
    NonBoundary,
}

impl Segment {
    #[allow(clippy::too_many_lines)]
    pub fn compile(
        &self,
        hash_state: u64,
        on_fail: TokenStream2,
        on_success: TokenStream2,
        span: Span2,
        end_length: usize,
        mut next: Vec<Self>,
    ) -> TokenStream2 {
        let hash_state = get_hash(&(hash_state ^ get_hash(self)));
        match self {
            Self::Char(c) => {
                let next = if next.is_empty() {
                    on_success
                } else {
                    let next_segment = next.remove(0);
                    next_segment.compile(
                        hash_state,
                        on_fail.clone(),
                        on_success,
                        span,
                        end_length,
                        next,
                    )
                };
                let c_comp = c.compile();
                quote! {
                    if !(#c_comp) {
                        #on_fail
                    }
                    idx += 1;
                    #next
                }
            }
            Self::Group(g) => {
                let next = if next.is_empty() {
                    on_success
                } else {
                    let next_segment = next.remove(0);
                    next_segment.compile(
                        hash_state,
                        on_fail.clone(),
                        on_success,
                        span,
                        end_length,
                        next,
                    )
                };
                if g.is_empty() {
                    next
                } else {
                    let mut group = g.clone();
                    let start = group.remove(0);
                    start.compile(hash_state, on_fail, next, span, end_length, group)
                }
            }
            Self::Quantified(s, q) => {
                let Quantity { min, max } = q;
                let max = max - min;
                let loop_label = syn::Lifetime::new(&format!("'loop_{hash_state:x}"), span);
                let min_iters = if *min == 0 {
                    quote! {}
                } else {
                    let compile_ret = s.compile(
                        hash_state,
                        on_fail.clone(),
                        quote! {continue #loop_label;},
                        span,
                        end_length,
                        Vec::new(),
                    );
                    quote! {
                        #loop_label: for _ in 0..#min {
                            #compile_ret
                        }
                    }
                };
                let idx_ident = syn::Ident::new(&format!("matches_{hash_state:x}"), span);
                let compile_break = s.compile(
                    hash_state,
                    quote! {break #loop_label;},
                    quote! {continue #loop_label;},
                    span,
                    end_length,
                    Vec::new(),
                );
                if max == 0 {
                    let next = if next.is_empty() {
                        on_success
                    } else {
                        let next_segment = next.remove(0);
                        next_segment
                            .compile(hash_state, on_fail, on_success, span, end_length, next)
                    };
                    quote! {
                        #min_iters
                        #next
                    }
                } else {
                    let next = if next.is_empty() {
                        on_success
                    } else {
                        let next_segment = next.remove(0);
                        if next_segment == Self::End {
                            next_segment.compile(
                                hash_state,
                                on_fail.clone(),
                                on_success,
                                span,
                                end_length,
                                next,
                            )
                        } else {
                            next_segment.compile(
                                hash_state,
                                quote! {continue #loop_label;},
                                on_success,
                                span,
                                end_length,
                                next,
                            )
                        }
                    };
                    quote! {
                        #min_iters
                        let mut #idx_ident: Vec<usize> = Vec::new();
                        #loop_label: for _ in 0..#max {
                            #idx_ident.push(idx);
                            #compile_break
                        }
                        #loop_label: for i in #idx_ident.into_iter().rev() {
                            idx = i;
                            #next
                        }
                        #on_fail
                    }
                }
            }
            Self::Str(s) => {
                let strlen = s.len();
                let c_ident = syn::Ident::new(&format!("c_{hash_state:x}"), span);
                let char_iter_ident = syn::Ident::new(&format!("src_iter_{hash_state:x}"), span);
                let next = if next.is_empty() {
                    on_success
                } else {
                    let next_segment = next.remove(0);
                    next_segment.compile(
                        hash_state,
                        on_fail.clone(),
                        on_success,
                        span,
                        end_length,
                        next,
                    )
                };
                let s_bytes = vecu8_to_tokens(s, span);
                quote! {
                    let mut #char_iter_ident = src.bytes().skip(idx);
                    for &#c_ident in #s_bytes {
                        if #char_iter_ident.next() != Some(#c_ident) {
                            #on_fail
                        }
                    }
                    idx += #strlen;
                    #next
                }
            }
            Self::Start => {
                let next = if next.is_empty() {
                    on_success
                } else {
                    let next_segment = next.remove(0);
                    next_segment.compile(
                        hash_state,
                        on_fail.clone(),
                        on_success,
                        span,
                        end_length,
                        next,
                    )
                };
                quote! {
                    if idx != 0 {
                        #on_fail
                    }
                    #next
                }
            }
            Self::End => {
                let next = if next.is_empty() {
                    on_success
                } else {
                    let next_segment = next.remove(0);
                    next_segment.compile(
                        hash_state,
                        on_fail.clone(),
                        on_success,
                        span,
                        end_length,
                        next,
                    )
                };
                quote! {
                    if idx + #end_length < src.len() {
                        #on_fail
                    }
                    #next
                }
            }
            Self::Boundary => {
                let next = if next.is_empty() {
                    on_success
                } else {
                    let next_segment = next.remove(0);
                    next_segment.compile(
                        hash_state,
                        on_fail.clone(),
                        on_success,
                        span,
                        end_length,
                        next,
                    )
                };
                quote! {
                    if (idx != 0 && src.bytes().nth(idx - 1).unwrap().is_word()) == (src.bytes().nth(idx).unwrap_or(0).is_word()) {
                        #on_fail
                    }
                    #next
                }
            }
            Self::NonBoundary => {
                let next = if next.is_empty() {
                    on_success
                } else {
                    let next_segment = next.remove(0);
                    next_segment.compile(
                        hash_state,
                        on_fail.clone(),
                        on_success,
                        span,
                        end_length,
                        next,
                    )
                };
                quote! {
                    if (idx != 0 && src.bytes().nth(idx - 1).unwrap().is_word()) ^ (src.bytes().nth(idx).unwrap_or(0).is_word()) {
                        #on_fail
                    }
                    #next
                }
            }
            Self::Bracket(options) => {
                let next = if next.is_empty() {
                    on_success
                } else {
                    let next_segment = next.remove(0);
                    next_segment.compile(
                        hash_state,
                        on_fail.clone(),
                        on_success,
                        span,
                        end_length,
                        next,
                    )
                };
                let mut c_comp = quote! {};
                for c in options {
                    if !c_comp.is_empty() {
                        c_comp.extend(quote! {||});
                    }
                    c_comp.extend(c.compile());
                }
                quote! {
                    if !(#c_comp) {
                        #on_fail
                    }
                    idx += 1;
                    #next
                }
            }
        }
    }
}

#[derive(Debug, Hash, PartialEq, Eq, Clone, Copy)]
struct Quantity {
    min: usize,
    max: usize,
}

#[derive(Debug, Hash, PartialEq, Eq, Clone, Copy)]
enum Character {
    Char(u8),
    Any,
    Range(u8, u8),
    Space,
    Word,
    Digit,
    NonSpace,
    NonWord,
    NonDigit,
}

impl Character {
    pub fn compile(self) -> TokenStream2 {
        let src_nth = quote! {src.bytes().nth(idx)};
        match self {
            Self::Char(c) => quote! {#src_nth == Some(#c)},
            Self::Any => quote! {true},
            Self::Range(l, r) => {
                quote! {#src_nth.is_some_and(|c| (#l..=#r).contains(&c))}
            }
            Self::Space => quote! {#src_nth.is_some_and(CharacterClasses::is_space)},
            Self::NonSpace => quote! {#src_nth.is_some_and(CharacterClasses::is_not_space)},
            Self::Word => {
                quote! {#src_nth.is_some_and(CharacterClasses::is_word)}
            }
            Self::NonWord => {
                quote! {#src_nth.is_some_and(CharacterClasses::is_not_word)}
            }
            Self::Digit => quote! {#src_nth.is_some_and(CharacterClasses::is_digit)},
            Self::NonDigit => {
                quote! {#src_nth.is_some_and(CharacterClasses::is_not_digit)}
            }
        }
    }
}
