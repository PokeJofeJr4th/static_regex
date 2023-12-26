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
