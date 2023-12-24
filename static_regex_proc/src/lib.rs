use std::{
    collections::hash_map::DefaultHasher,
    hash::{Hash, Hasher},
    str::FromStr,
};

use proc_macro::{Literal, Span, TokenStream, TokenTree};
use proc_macro2::{Span as Span2, TokenStream as TokenStream2};
use quote::{quote, quote_spanned, TokenStreamExt};
use syn::{
    parse::{Parse, ParseStream},
    parse_macro_input, Expr, ExprLit, Ident, Lit,
};

fn get_hash(h: &impl Hash) -> u64 {
    let mut state = DefaultHasher::new();
    h.hash(&mut state);
    state.finish()
}

#[proc_macro]
pub fn static_regex(input: TokenStream) -> TokenStream {
    let mut regex = parse_macro_input!(input as RegexRepr);
    println!("{regex:?}");
    let hash_state = get_hash(&regex);
    let mut fn_body = quote! {
        let mut idx: usize = 0;
    };
    if regex.body.len() >= 2 {
        if let (Some(Segment::Str(end)), Some(Segment::End)) =
            (regex.body.get(regex.body.len() - 2), regex.body.last())
        {
            // check if the string ends with the given string
            fn_body.extend(quote! {
                if !src.ends_with(#end) {
                    return false;
                }
            });
            regex.body.pop();
            regex.body.pop();
        } else if let (Some(Segment::Char(Character::Char(end))), Some(Segment::End)) =
            (regex.body.get(regex.body.len() - 2), regex.body.last())
        {
            // check if the string ends with the given character
            fn_body.extend(quote! {
                if !src.ends_with(#end) {
                    return false;
                }
            });
            regex.body.pop();
            regex.body.pop();
        }
    }
    if regex.body.get(0) == Some(&Segment::Start) {
        regex.body.remove(0);
        if let Some(Segment::Str(start)) = regex.body.get(0) {
            // check if the string starts with the given string
            let str_len = start.len();
            fn_body.extend(quote! {
                if !src.starts_with(#start) {
                    return false;
                }
                idx += #str_len;
            });
            regex.body.remove(0);
        } else if let Some(Segment::Char(Character::Char(start))) = regex.body.get(0) {
            // check if the string starts with the given character
            fn_body.extend(quote! {
                if !src.starts_with(#start) {
                    return false;
                }
                idx += 1;
            });
            regex.body.remove(0);
        }
    } else {
        todo!()
    }
    fn_body.extend(if regex.body.is_empty() {
        quote! {}
    } else {
        let start = regex.body.remove(0);
        start.compile(hash_state, quote! {return false;}, regex.span, regex.body)
    });
    quote! {::static_regex::Regex::from_fn(
        |src| {#fn_body true}
    )}
    .into()
}

#[derive(Debug)]
struct RegexRepr {
    body: Vec<Segment>,
    span: Span2,
}

impl Hash for RegexRepr {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.body.hash(state);
    }
}

#[derive(Debug)]
enum RegexErr {
    UnexpectedEof,
    BadEscape(char),
}

impl Parse for RegexRepr {
    fn parse(input: ParseStream) -> Result<Self, syn::Error> {
        // println!("{input:?}");
        let Expr::Lit(ExprLit {
            attrs: _,
            lit: Lit::Str(lit_str),
        }) = input.parse()? else {
            panic!()
        };

        let s = lit_str.value();
        let span = lit_str.span();

        let mut idx = 0;
        let mut body = Vec::new();
        while idx < s.len() {
            let segment = get_regex_section(&s, &mut idx).unwrap();
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
                body.push(Segment::Str(format!("{lhs}{rhs}")));
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
                        rhs.push_str(lhs);
                    }
                    *min = 0;
                    body.push(Segment::Str(rhs));
                    continue;
                }
            }

            body.push(segment);
        }
        Ok(Self { body, span })
    }
}

fn get_regex_section(s: &str, idx: &mut usize) -> Result<Segment, RegexErr> {
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
                Some('n') => Ok(Segment::Char(Character::Char('\n'))),
                Some('t') => Ok(Segment::Char(Character::Char('\t'))),
                Some(c @ ('\\' | '(' | ')' | '{' | '}' | '[' | ']' | '+' | '*' | '?' | '.')) => {
                    Ok(Segment::Char(Character::Char(c)))
                }
                Some(c) => Err(RegexErr::BadEscape(c)),
                None => Err(RegexErr::UnexpectedEof),
            }
        }
        Some('^') => Ok(Segment::Start),
        Some('$') => Ok(Segment::End),
        Some('.') => Ok(Segment::Char(Character::Any)),
        Some(c) => Ok(Segment::Char(Character::Char(c))),
        None => Err(RegexErr::UnexpectedEof),
    }?;
    *idx += 1;
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
                        } else {
                            min = Some(int_buf);
                            int_buf = 0;
                        }
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
    Str(String),
    Char(Character),
    Quantified(Box<Segment>, Quantity),
    Group(Vec<Segment>),
    Start,
    End,
}

impl Segment {
    pub fn compile(
        &self,
        hash_state: u64,
        on_else: TokenStream2,
        span: Span2,
        mut next: Vec<Self>,
    ) -> TokenStream2 {
        let hash_state = get_hash(&(hash_state ^ get_hash(self)));
        match self {
            Self::Char(c) => {
                let next = if next.is_empty() {
                    quote! {}
                } else {
                    let nxt = next.remove(0);
                    nxt.compile(hash_state, on_else.clone(), span, next)
                };
                let c_comp = c.compile();
                quote! {
                    if !(#c_comp) {
                        #on_else
                    }
                    idx += 1;
                    #next
                }
            }
            Self::Group(g) => todo!(),
            Self::Quantified(s, q) => {
                let Quantity { min, max } = q;
                let max = max - min;
                let min_iters = if *min == 0 {
                    quote! {}
                } else {
                    let compile_ret = s.compile(hash_state, on_else.clone(), span, Vec::new());
                    quote! {
                        for _ in 0..#min {
                            #compile_ret
                        }
                    }
                };
                let idx_ident = syn::Ident::new(&format!("matches_{hash_state:x}"), span);
                let loop_label = syn::Lifetime::new(&format!("'loop_{hash_state:x}"), span);
                let compile_break =
                    s.compile(hash_state, quote! {break #loop_label;}, span, Vec::new());
                let next = if next.is_empty() {
                    quote! {}
                } else {
                    let nxt = next.remove(0);
                    nxt.compile(hash_state, quote! {continue #loop_label;}, span, next)
                };
                if max == 0 {
                    min_iters
                } else {
                    quote! {
                        #min_iters
                        let mut #idx_ident: Vec<usize> = Vec::new();
                        #loop_label: for _ in 0..#max {
                            #idx_ident.push(idx);
                            #compile_break
                        }
                        if #idx_ident.last() != Some(&idx) {
                            #idx_ident.push(idx);
                        }
                        #loop_label: for i in #idx_ident.into_iter().rev() {
                            idx = i;
                            #next
                            return true;
                        }
                        #on_else
                    }
                }
            }
            Self::Str(s) => {
                let strlen = s.len();
                let c_ident = syn::Ident::new(&format!("c_{hash_state:x}"), span);
                let char_iter_ident = syn::Ident::new(&format!("src_iter_{hash_state:x}"), span);
                let next = if next.is_empty() {
                    quote! {}
                } else {
                    let nxt = next.remove(0);
                    nxt.compile(hash_state, on_else.clone(), span, next)
                };
                quote! {
                    let mut #char_iter_ident = src.chars().skip(idx);
                    for #c_ident in #s.chars() {
                        if #char_iter_ident.next() != Some(#c_ident) {
                            #on_else
                        }
                    }
                    idx += #strlen;
                    #next
                }
            }
            Self::Start => {
                let next = if next.is_empty() {
                    quote! {}
                } else {
                    let nxt = next.remove(0);
                    nxt.compile(hash_state, on_else.clone(), span, next)
                };
                quote! {
                    if idx != 0 {
                        #on_else
                    }
                    #next
                }
            }
            Self::End => {
                let next = if next.is_empty() {
                    quote! {}
                } else {
                    let nxt = next.remove(0);
                    nxt.compile(hash_state, on_else.clone(), span, next)
                };
                quote! {
                    if idx < src.len() {
                        #on_else
                    }
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
    Char(char),
    Any,
    Range(char, char),
    Space,
    Word,
    Digit,
    NonSpace,
    NonWord,
    NonDigit,
}

impl Character {
    pub fn compile(&self) -> TokenStream2 {
        let src_nth = quote! {src.chars().nth(idx)};
        match self {
            Self::Char(c) => quote! {#src_nth == Some(#c)},
            Self::Any => quote! {true},
            Self::Range(l, r) => quote! {#src_nth.is_some_and(|c| #l <= c && #r >= c)},
            Self::Space => quote! {#src_nth.is_some_and(char::is_whitespace)},
            Self::NonSpace => quote! {#src_nth.is_some_and(|c| !c.is_whitespace())},
            Self::Word => quote! {#src_nth.is_some_and(char::is_alphanumeric)},
            Self::NonWord => quote! {#src_nth.is_some_and(|c| !c.is_alphanumeric())},
            Self::Digit => quote! {#src_nth.is_some_and(char::is_ascii_digit)},
            Self::NonDigit => quote! {#src_nth.is_some_and(|c| !c.is_ascii_digit())},
        }
    }
}
