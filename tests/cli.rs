use assert_cmd::prelude::*;
use assert_fs::prelude::*;
use predicates::prelude::*;
use std::process::Command;

#[test]
fn hello_world() -> Result<(), Box<dyn std::error::Error>> {
    let source = assert_fs::NamedTempFile::new("source.awk")?;
    source.write_str("BEGIN { print 'hello', ',', 'world' }")?;

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
    in1.write_str("hello\nworld\n")?;
    let in2 = assert_fs::NamedTempFile::new("in2.awk")?;
    in2.write_str("hello\nworld\n")?;

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
    in1.write_str("hello   world   \n")?;

    let mut cmd = Command::cargo_bin("yarawk")?;
    cmd.arg("-f").arg(source.path()).arg(in1.path());
    cmd.assert()
        .success()
        .stdout(predicate::str::contains("hello-world\n"));

    Ok(())
}

#[test]
fn pattern() -> Result<(), Box<dyn std::error::Error>> {
    let source = assert_fs::NamedTempFile::new("source.awk")?;
    source.write_str(
        r#"
        /a/ {
            print "match", NR
        }
    "#,
    )?;

    let in1 = assert_fs::NamedTempFile::new("in1.awk")?;
    in1.write_str(
        r#"a
    b
    a c b
    hello

    a b
    "#,
    )?;

    let mut cmd = Command::cargo_bin("yarawk")?;
    cmd.arg("-f").arg(source.path()).arg(in1.path());
    cmd.assert()
        .success()
        .stdout(predicate::str::contains("match 1\nmatch 3\nmatch 6\n"));

    Ok(())
}

#[test]
fn range_pattern() -> Result<(), Box<dyn std::error::Error>> {
    let source = assert_fs::NamedTempFile::new("source.awk")?;
    source.write_str(
        r#"
        /a/, /b/ {
            print "match", NR
        }
    "#,
    )?;

    let in1 = assert_fs::NamedTempFile::new("in1.awk")?;
    in1.write_str(
        r#"a
    b
    a c b
    hello

    a b
    "#,
    )?;

    let mut cmd = Command::cargo_bin("yarawk")?;
    cmd.arg("-f").arg(source.path()).arg(in1.path());
    cmd.assert().success().stdout(predicate::str::contains(
        "match 1\nmatch 2\nmatch 3\nmatch 6\n",
    ));

    Ok(())
}

#[test]
fn expression_pattern() -> Result<(), Box<dyn std::error::Error>> {
    let source = assert_fs::NamedTempFile::new("source.awk")?;
    source.write_str(
        r#"
        "" {
            print "not match", NR
        }
        42 {
            print "match", NR
        }
    "#,
    )?;

    let in1 = assert_fs::NamedTempFile::new("in1.awk")?;
    in1.write_str("a\nb")?;

    let mut cmd = Command::cargo_bin("yarawk")?;
    cmd.arg("-f").arg(source.path()).arg(in1.path());
    cmd.assert()
        .success()
        .stdout(predicate::str::contains("match 1\nmatch 2\n"));

    Ok(())
}

#[test]
fn unary_expression() -> Result<(), Box<dyn std::error::Error>> {
    let source = assert_fs::NamedTempFile::new("source.awk")?;
    source.write_str(
        r#"
        BEGIN {
            a[0] = "hello"
            v = 42
            print ++a[0], ++v, ++$0
            print --a[0], --v, --$0
            print a[0]++, v++, $0++
            print a[0]--, v--, $0--
            print a[0], v, $0
        }
    "#,
    )?;

    let in1 = assert_fs::NamedTempFile::new("in1.awk")?;
    in1.write_str("a\nb")?;

    let mut cmd = Command::cargo_bin("yarawk")?;
    cmd.arg("-f").arg(source.path()).arg(in1.path());
    cmd.assert().success().stdout(predicate::str::contains(
        r#"1 43 1
0 42 0
0 42 0
1 43 1
0 42 0"#,
    ));

    Ok(())
}

#[test]
fn arithmetic_expression() -> Result<(), Box<dyn std::error::Error>> {
    let source = assert_fs::NamedTempFile::new("source.awk")?;
    source.write_str(
        r#"
        BEGIN {
            x = y = 1 / 5 * 10 - 3^5
        }
        END {
            print x, y
        }
    "#,
    )?;

    let in1 = assert_fs::NamedTempFile::new("in1.awk")?;
    in1.write_str("a\nb")?;

    let mut cmd = Command::cargo_bin("yarawk")?;
    cmd.arg("-f").arg(source.path()).arg(in1.path());
    cmd.assert()
        .success()
        .stdout(predicate::str::contains("-241 -241"));

    Ok(())
}

#[test]
fn function_scope() -> Result<(), Box<dyn std::error::Error>> {
    let source = assert_fs::NamedTempFile::new("source.awk")?;
    source.write_str(
        r#"
        function foo(x) {
            print "x in foo is", x
            x = x + 1
        }

        BEGIN {
            x = 42
            print x
            foo()
            foo()
            print x
        }
    "#,
    )?;

    let mut cmd = Command::cargo_bin("yarawk")?;
    cmd.arg("-f").arg(source.path());
    cmd.assert().success().stdout(predicate::str::contains(
        "42\nx in foo is \nx in foo is \n42\n",
    ));

    Ok(())
}

#[test]
fn array_variable() -> Result<(), Box<dyn std::error::Error>> {
    let source = assert_fs::NamedTempFile::new("source.awk")?;
    source.write_str(
        r#"
        BEGIN {
            a[0] = "hello"
            a[1] = "world"
            print "'" a[0] "'", "'" a[1] "'"
        }
    "#,
    )?;

    let mut cmd = Command::cargo_bin("yarawk")?;
    cmd.arg("-f").arg(source.path());
    cmd.assert()
        .success()
        .stdout(predicate::str::contains("'hello' 'world'\n"));

    Ok(())
}

#[test]
fn getline() -> Result<(), Box<dyn std::error::Error>> {
    let in2 = assert_fs::NamedTempFile::new("in2.txt")?;
    in2.write_str("hello world ")?;

    let source = assert_fs::NamedTempFile::new("source.awk")?;
    source.write_str(&format!(
        r#"
        BEGIN {{
            a = "hello"
            print a, NR

            getline
            print NR

            getline a < "{}"
            print a, NR, FNR
            print NR, FNR

            getline a
            print a, NR, FNR
            getline a
            print a, NR, FNR
        }}

        END {{
            print NR
        }}
    "#,
        in2.path().to_str().unwrap()
    ))?;

    let in1 = assert_fs::NamedTempFile::new("in1.txt")?;
    in1.write_str("line 1 \nline 2 ")?;

    let mut cmd = Command::cargo_bin("yarawk")?;
    cmd.arg("-f").arg(source.path()).arg(in1.path());
    cmd.assert().success().stdout(predicate::str::contains(
        "hello 0\n1\nhello world 1 1\n1 1\nline 2 2 2\nline 2 2 2\n2",
    ));

    Ok(())
}
