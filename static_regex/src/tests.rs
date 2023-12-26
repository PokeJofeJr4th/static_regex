use super::CharacterClasses;

#[test]
fn test_char_class_digit() {
    assert!(CharacterClasses::is_digit('0'));
    assert!(CharacterClasses::is_digit('9'));
    assert!(CharacterClasses::is_digit(b'0'));
    assert!(CharacterClasses::is_digit(b'9'));
}
