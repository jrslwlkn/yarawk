use assert_cmd::prelude::*;
use assert_fs::prelude::*;
use predicates::prelude::*;
use std::process::Command;

#[test]
fn hello_world() -> Result<(), Box<dyn std::error::Error>> {
    let source = assert_fs::NamedTempFile::new("source.awk")?;
    source.write_str("BEGIN { print 'hello', ',' 'world' }")?;

    let mut cmd = Command::cargo_bin("yarawk")?;
    cmd.arg("-f").arg(source.path());
    cmd.assert()
        .success()
        .stdout(predicate::str::contains("hello , world\n"));

    Ok(())
}

#[test]
fn actions() -> Result<(), Box<dyn std::error::Error>> {
    let source = assert_fs::NamedTempFile::new("source.awk")?;
    source.write_str(
        r#"
        { print }
        END {
            print FNR, NR
        }
    "#,
    )?;

    let in1 = assert_fs::NamedTempFile::new("in1.awk")?;
    in1.write_str("hello\nworld\n");
    let in2 = assert_fs::NamedTempFile::new("in2.awk")?;
    in2.write_str("hello\nworld\n");

    let mut cmd = Command::cargo_bin("yarawk")?;
    cmd.arg("-f")
        .arg(source.path())
        .arg(in1.path())
        .arg(in2.path());
    cmd.assert().success().stdout(predicate::str::contains(
        "hello\nworld\nhello\nworld\n2 4\n",
    ));

    Ok(())
}

#[test]
fn fields() -> Result<(), Box<dyn std::error::Error>> {
    let source = assert_fs::NamedTempFile::new("source.awk")?;
    source.write_str(
        r#"
        1 { print $1 "-" $2 }
    "#,
    )?;

    let in1 = assert_fs::NamedTempFile::new("in1.awk")?;
    in1.write_str("hello   world   \n");

    let mut cmd = Command::cargo_bin("yarawk")?;
    cmd.arg("-f").arg(source.path()).arg(in1.path());
    cmd.assert()
        .success()
        .stdout(predicate::str::contains("hello - world\n"));

    Ok(())
}
