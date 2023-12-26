#![warn(clippy::pedantic, clippy::nursery)]
use std::fmt::Debug;

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

impl ToString for Regex {
    fn to_string(&self) -> String {
        self.pattern.to_string()
    }
}
