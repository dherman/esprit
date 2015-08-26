use std::process::Command;

fn main() {
    assert!(Command::new("make")
        .args(&["-C", "tests"])
        .status()
        .unwrap()
        .success());
}
