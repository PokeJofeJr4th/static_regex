pub struct Regex {
    match_fn: fn(&str) -> bool
}

impl Regex {
    pub const fn from_fn(match_fn: fn(&str) -> bool) -> Self {
        Self { match_fn }
    }

    pub fn matches(&self, src: &str) -> bool {
        (self.match_fn)(src)
    }
}
