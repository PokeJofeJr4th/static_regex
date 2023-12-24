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
    let regex = parse_macro_input!(input as RegexRepr);
    println!("{regex:?}");
    let mut hash_state = get_hash(&regex);
    let mut fn_body = quote! {
        let mut idx: usize = 0;
    };
    for segment in regex.body {
        hash_state = get_hash(&(hash_state ^ get_hash(&segment)));
        fn_body.extend(segment.compile(hash_state, quote! {return false;}, regex.span));
    }
    fn_body.extend(quote! {true});
    quote! {::static_regex::Regex::from_fn(
        |src| {#fn_body}
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
                lhs.push(rhs);
            } else if let (
                Some(&Segment::Char(Character::Char(lhs))),
                &Segment::Char(Character::Char(rhs)),
            ) = (body.last(), &segment)
            {
                body.pop();
                body.push(Segment::Str(format!("{lhs}{rhs}")));
            } else {
                body.push(segment);
            }
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
        _ => Ok(v),
    }
}

#[derive(Debug, Hash)]
enum Segment {
    Str(String),
    Char(Character),
    Quantified(Box<Segment>, Quantity),
    Group(Vec<Segment>),
    Start,
    End,
}

impl Segment {
    pub fn compile(&self, hash_state: u64, on_else: TokenStream2, span: Span2) -> TokenStream2 {
        match self {
            Self::Char(c) => {
                let c_comp = c.compile(hash_state);
                quote! {
                    if !(#c_comp) {
                        #on_else
                    }
                    idx += 1;
                }
            }
            Self::Group(g) => todo!(),
            Self::Quantified(s, q) => {
                let Quantity { min, max } = q;
                let max = max - min;
                let min_iters = if *min == 0 {
                    quote! {}
                } else {
                    let compile_ret = s.compile(hash_state, on_else, span);
                    quote! {
                        for _ in 0..#min {
                            #compile_ret
                        }
                    }
                };
                let idx_ident = syn::Ident::new(&format!("idx_{hash_state:x}"), span);
                let loop_label = syn::Lifetime::new(&format!("'loop_{hash_state:x}"), span);
                let compile_break = s.compile(hash_state, quote! {break #loop_label;}, span);
                quote! {
                    #min_iters
                    let mut #idx_ident: usize = 0;
                    #loop_label: for _ in 0..#max {
                        #idx_ident = idx;
                        #compile_break
                    }
                    idx = #idx_ident;
                }
            }
            Self::Str(s) => {
                let strlen = s.len();
                let c_ident = syn::Ident::new(&format!("c_{hash_state:x}"), span);
                let char_iter_ident = syn::Ident::new(&format!("src_iter_{hash_state:x}"), span);
                quote! {
                    let mut #char_iter_ident = src.chars().skip(idx);
                    for #c_ident in #s.chars() {
                        if #char_iter_ident.next() != Some(#c_ident) {
                            #on_else
                        }
                    }
                    idx += #strlen;
                }
            }
            Self::Start => {
                quote! {
                    if idx != 0 {
                        return false;
                    }
                }
            }
            Self::End => {
                quote! {
                    if idx < src.len() {
                        return false;
                    }
                }
            }
        }
    }
}

#[derive(Debug, Hash)]
struct Quantity {
    min: usize,
    max: usize,
}

#[derive(Debug, Hash)]
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
    pub fn compile(&self, hash_state: u64) -> TokenStream2 {
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
