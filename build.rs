use std::process::Command;

fn main() {
    if cfg!(not(windows)) {
        assert!(Command::new("make")
            .args(&["-C", "tests"])
            .status()
            .unwrap()
            .success());
    }
}
