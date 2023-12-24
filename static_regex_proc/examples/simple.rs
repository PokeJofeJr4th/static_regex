use static_regex::Regex;
use static_regex_proc::static_regex;

const X: Regex = static_regex!("^tra+ins*$");

fn main() {
    for s in [
        "train",
        "trainsss",
        "trinsss",
        "traaaains",
        "traaaainssssssssss",
        "train!",
    ] {
        println!("{s}: {}", X.matches(s))
    }
}
