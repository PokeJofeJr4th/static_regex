use static_regex::Regex;
use static_regex_proc::static_regex;

const TRAINS: Regex = static_regex!("^tra+ins*$");
const WOLOLO: Regex = static_regex!("^w(ol)+o$");
const HELLO: Regex = static_regex!("^Hello$");

fn main() {
    for s in [
        "train",
        "trainsss",
        "trinsss",
        "traaaains",
        "traaaainssssssssss",
        "train!",
        "wololo",
        "wolo",
        "wololololololololo",
    ] {
        println!("{s}: {} {}", TRAINS.matches(s), WOLOLO.matches(s));
    }
}
