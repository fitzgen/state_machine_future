use std::fs::File;
use std::io::Read;
use std::process::Command;

#[test]
fn cargo_readme_up_to_date() {
    println!("Checking that `cargo readme > README.md` is up to date...");

    let output = Command::new("cargo")
        .arg("readme")
        .current_dir(env!("CARGO_MANIFEST_DIR"))
        .output()
        .expect("should run `cargo readme` OK");

    assert!(
        output.status.success(),
        "Check if you have `cargo-readme` in $PATH"
    );
    let expected = String::from_utf8_lossy(&output.stdout);

    let actual = {
        let mut file = File::open(concat!(env!("CARGO_MANIFEST_DIR"), "/README.md"))
            .expect("should open README.md file");
        let mut s = String::new();
        file.read_to_string(&mut s)
            .expect("should read contents of file to string");
        s
    };

    if actual != expected {
        panic!("Run `cargo readme > README.md` to update README.md");
    }
}
