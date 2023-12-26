use static_regex::Regex;
use static_regex_proc::static_regex;

const TRAINS: Regex = static_regex!(r"^tra+ins*$");
const WOLOLO: Regex = static_regex!("^w(ol)+o$");
const HELLO: Regex = static_regex!("^Hello$");
const LOL: Regex = static_regex!("lol");

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
        println!(
            "{s}:\n\t{TRAINS}: {}\n\t{WOLOLO}: {}\n\t{LOL}: {}",
            TRAINS.is_match(s),
            WOLOLO.is_match(s),
            LOL.is_match(s)
        );
    }
}
