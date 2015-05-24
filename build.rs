use std::process::Command;

fn main() {
    assert!(Command::new("make")
        .args(&["-C", "tests/parser"])
        .status()
        .unwrap()
        .success());
}
