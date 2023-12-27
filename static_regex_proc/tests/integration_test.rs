#[test]
fn is_match() {
    let re = static_regex_proc::static_regex!(r"\d{3}-\d{2}-\d{4}");
    assert!(re.is_match("123-45-6789"));
    assert!(!re.is_match("123-456-789"));
}

#[test]
fn is_match_start_hello() {
    let re = static_regex_proc::static_regex!(r"^Hello");
    assert!(re.is_match("Hello, World!"));
    assert!(!re.is_match("Why, Hello!"));
}

#[test]
fn is_match_empty() {
    let re = static_regex_proc::static_regex!("^$");
    assert!(re.is_match(""));
    assert!(!re.is_match("invalid input"));
}

#[test]
fn test_quantifier() {
    let re = static_regex_proc::static_regex!("^(1+0)+$");
    assert!(re.is_match("10"));
    assert!(re.is_match("111010101010"));
    assert!(!re.is_match("100"));
}

#[test]
fn test_word_boundary() {
    let re = static_regex_proc::static_regex!(r"\bapple\b");
    assert!(re.is_match("I want an apple"));
    assert!(re.is_match("apple"));
    assert!(!re.is_match("Welcome to applebee's!"));
}

#[test]
fn test_non_word_boundary() {
    let re = static_regex_proc::static_regex!(r"\Bball\b");
    assert!(re.is_match("Basketball"));
    assert!(!re.is_match("Basketballs"));
    assert!(!re.is_match("ball"))
}

#[test]
fn test_bracket_group() {
    let re = static_regex_proc::static_regex!(r"[Ww]ow");
    assert!(re.is_match("wow"));
    assert!(re.is_match("Wow"));
    assert!(!re.is_match("WWW"));
}

#[test]
fn test_bracket_range() {
    let re = static_regex_proc::static_regex!(r"^[1-9]\d*$");
    assert!(re.is_match("1"));
    assert!(re.is_match("100"));
    assert!(!re.is_match("001"));
    assert!(re.is_match("9"));
    assert!(re.is_match("900"));
    assert!(!re.is_match("009"));
}
