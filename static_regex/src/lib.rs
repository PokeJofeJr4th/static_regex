#![warn(clippy::pedantic, clippy::nursery)]
use std::fmt::{Debug, Display};

#[cfg(test)]
mod tests;

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Regex {
    match_fn: fn(&str) -> bool,
    pattern: &'static str,
}

impl Regex {
    /// This function should only be used by the `static_regex_proc` crate.
    /// If used outside of this crate, the resulting regex will be invalid
    pub const unsafe fn new(pattern: &'static str, match_fn: fn(&str) -> bool) -> Self {
        Self { pattern, match_fn }
    }

    pub const fn pattern(self) -> &'static str {
        self.pattern
    }

    pub fn is_match(&self, src: &str) -> bool {
        (self.match_fn)(src)
    }
}

impl Debug for Regex {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.pattern)
    }
}

impl Display for Regex {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.pattern)
    }
}

pub trait CharacterClasses: Sized {
    fn is_word(self) -> bool;
    fn is_digit(self) -> bool;
    fn is_space(self) -> bool;

    fn is_not_word(self) -> bool {
        !self.is_word()
    }
    fn is_not_digit(self) -> bool {
        !self.is_digit()
    }
    fn is_not_space(self) -> bool {
        !self.is_space()
    }
}

impl CharacterClasses for char {
    fn is_digit(self) -> bool {
        self.is_ascii_digit()
    }

    fn is_space(self) -> bool {
        self.is_whitespace()
    }

    fn is_word(self) -> bool {
        self == '_' || self.is_alphanumeric()
    }
}

impl CharacterClasses for u8 {
    fn is_digit(self) -> bool {
        (b'0'..=b'9').contains(&self)
    }

    fn is_space(self) -> bool {
        b" \n\r\t".contains(&self)
    }

    fn is_word(self) -> bool {
        self == b'_' || (b'a'..=b'z').contains(&self) || (b'A'..=b'Z').contains(&self)
    }
}
