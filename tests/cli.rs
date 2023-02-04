use assert_cmd::prelude::*;
use assert_fs::prelude::*;
use predicates::prelude::*;
use std::{fs, process::Command};

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

#[test]
fn getline_result() -> Result<(), Box<dyn std::error::Error>> {
    let in2 = assert_fs::NamedTempFile::new("in2.txt")?;
    in2.write_str("hello world ")?;

    let source = assert_fs::NamedTempFile::new("source.awk")?;
    source.write_str(&format!(
        r#"
        BEGIN {{
            x = getline x < "doesnotexist"
            print x

            x = getline x < "{}"
            print x

            x = getline x < "{}"
            print x
        }}
        END {{
            print NR
        }}
    "#,
        in2.path().to_str().unwrap(),
        in2.path().to_str().unwrap(),
    ))?;

    let mut cmd = Command::cargo_bin("yarawk")?;
    cmd.arg("-f").arg(source.path());
    cmd.assert()
        .success()
        .stdout(predicate::str::contains("-1\n1\n0\n0"));

    Ok(())
}

#[test]
fn getline_file_does_not_exit() -> Result<(), Box<dyn std::error::Error>> {
    let source = assert_fs::NamedTempFile::new("source.awk")?;
    source.write_str(
        r#"
        BEGIN {
            getline x < "doesnotexst"
            print "x=",x
        }
        END {
            print NR
        }
    "#,
    )?;

    let mut cmd = Command::cargo_bin("yarawk")?;
    cmd.arg("-f").arg(source.path());
    cmd.assert()
        .success()
        .stdout(predicate::str::contains("x= \n0"));

    Ok(())
}

#[test]
fn getline_pipe() -> Result<(), Box<dyn std::error::Error>> {
    let source = assert_fs::NamedTempFile::new("source.awk")?;
    source.write_str(
        r#"
        BEGIN {
            c = "ls " "-a"
            c | getline a
            print a, NR
            "ls -a" | getline a
        }
        
        END {
            print NR
        }
    "#,
    )?;

    let mut cmd = Command::cargo_bin("yarawk")?;
    cmd.arg("-f").arg(source.path());
    cmd.assert()
        .success()
        .stdout(predicate::str::contains(". 0\n0"));

    Ok(())
}

#[test]
fn print_append() -> Result<(), Box<dyn std::error::Error>> {
    let in2 = assert_fs::NamedTempFile::new("in2.txt")?;
    in2.write_str("hello world\n")?;

    match fs::remove_file("test_newfile.txt") {
        Ok(_) => {}
        Err(_) => {}
    }
    let source = assert_fs::NamedTempFile::new("source.awk")?;
    source.write_str(&format!(
        r#"
        {{
             print NR >> "{}"
             print >> "test_newfile.txt"
        }}
    "#,
        in2.path().to_str().unwrap(),
    ))?;

    let mut cmd = Command::cargo_bin("yarawk")?;
    let in1 = assert_fs::NamedTempFile::new("in1.txt")?;
    in1.write_str("line 1\nline 2\n")?;

    cmd.arg("-f").arg(source.path()).arg(in1.path());

    cmd.assert().success();

    in2.assert("hello world\n1\n2\n");

    assert_eq!(
        fs::read_to_string("test_newfile.txt")?,
        "line 1\nline 2\n".to_string()
    );
    fs::remove_file("test_newfile.txt")?;

    Ok(())
}

#[test]
fn printf_append() -> Result<(), Box<dyn std::error::Error>> {
    let in2 = assert_fs::NamedTempFile::new("printf_append_in2.txt")?;
    in2.write_str("hello world\n")?;

    match fs::remove_file("printf_append_test_newfile.txt") {
        Ok(_) => {}
        Err(_) => {}
    }
    let source = assert_fs::NamedTempFile::new("source.awk")?;
    source.write_str(&format!(
        r#"
        {{
             printf(NR) >> "{}"
             printf($0) >> "printf_append_test_newfile.txt"
        }}
    "#,
        in2.path().to_str().unwrap(),
    ))?;

    let mut cmd = Command::cargo_bin("yarawk")?;
    let in1 = assert_fs::NamedTempFile::new("printf_append_in1.txt")?;
    in1.write_str("line 1\nline 2\n")?;

    cmd.arg("-f").arg(source.path()).arg(in1.path());

    cmd.assert().success();

    in2.assert("hello world\n1\n2\n");

    assert_eq!(
        fs::read_to_string("printf_append_test_newfile.txt")?,
        "line 1\nline 2\n".to_string()
    );
    fs::remove_file("printf_append_test_newfile.txt")?;

    Ok(())
}

#[test]
fn print_write() -> Result<(), Box<dyn std::error::Error>> {
    let in2 = assert_fs::NamedTempFile::new("print_write_in2.txt")?;
    in2.write_str("hello world\n")?;

    match fs::remove_file("print_write_test_newfile.txt") {
        Ok(_) => {}
        Err(_) => {}
    }
    let source = assert_fs::NamedTempFile::new("source.awk")?;
    source.write_str(&format!(
        r#"
        {{
             print NR > "{}"
             print > "print_write_test_newfile.txt"
        }}
    "#,
        in2.path().to_str().unwrap(),
    ))?;

    let mut cmd = Command::cargo_bin("yarawk")?;
    let in1 = assert_fs::NamedTempFile::new("print_write_in1.txt")?;
    in1.write_str("line 1\nline 2\n")?;

    cmd.arg("-f").arg(source.path()).arg(in1.path());

    cmd.assert().success();

    in2.assert("1\n2\n");

    assert_eq!(
        fs::read_to_string("print_write_test_newfile.txt")?,
        "line 1\nline 2\n".to_string()
    );
    fs::remove_file("print_write_test_newfile.txt")?;

    Ok(())
}

#[test]
fn printf_write() -> Result<(), Box<dyn std::error::Error>> {
    let in2 = assert_fs::NamedTempFile::new("printf_write_in2.txt")?;
    in2.write_str("hello world\n")?;

    match fs::remove_file("printf_write_test_newfile.txt") {
        Ok(_) => {}
        Err(_) => {}
    }
    let source = assert_fs::NamedTempFile::new("source.awk")?;
    source.write_str(&format!(
        r#"
        {{
             printf(NR) > "{}"
             printf($0) > "printf_write_test_newfile.txt"
        }}
    "#,
        in2.path().to_str().unwrap(),
    ))?;

    let mut cmd = Command::cargo_bin("yarawk")?;
    let in1 = assert_fs::NamedTempFile::new("printf_write_in1.txt")?;
    in1.write_str("line 1\nline 2\n")?;

    cmd.arg("-f").arg(source.path()).arg(in1.path());

    cmd.assert().success();

    in2.assert("1\n2\n");

    assert_eq!(
        fs::read_to_string("printf_write_test_newfile.txt")?,
        "line 1\nline 2\n".to_string()
    );
    fs::remove_file("printf_write_test_newfile.txt")?;

    Ok(())
}

#[test]
fn print_pipe() -> Result<(), Box<dyn std::error::Error>> {
    let source = assert_fs::NamedTempFile::new("source.awk")?;
    source.write_str(
        r#"
        BEGIN {
            print "hello world" | "wc -w"
        }
        END {
            print NR
        }
    "#,
    )?;

    let mut cmd = Command::cargo_bin("yarawk")?;
    cmd.arg("-f").arg(source.path());
    cmd.assert()
        .success()
        .stdout(predicate::str::contains("0\n       2"));

    Ok(())
}

#[test]
fn in_operator() -> Result<(), Box<dyn std::error::Error>> {
    let source = assert_fs::NamedTempFile::new("source.awk")?;
    source.write_str(
        r#"
        BEGIN {
            a["hello"] = "world"
            a[3] = 4

            x1 = "hello" in a
            x2 = "3" in a
            x3 = "a" in a
            x4 = "a" in b

            print x1, x2, x3, x4
        }
    "#,
    )?;

    let mut cmd = Command::cargo_bin("yarawk")?;
    cmd.arg("-f").arg(source.path());
    cmd.assert().success().stdout(predicate::eq("1 1 0 0\n"));

    Ok(())
}

#[test]
fn delete_operator() -> Result<(), Box<dyn std::error::Error>> {
    let source = assert_fs::NamedTempFile::new("source.awk")?;
    source.write_str(
        r#"
        BEGIN {
            a["hello"] = "world"
            a[3] = 4

            x1 = "hello" in a
            x2 = "3" in a
            x3 = "a" in a
            x4 = "a" in b
            delete a["hello"]
            x5 = "hello" in a
            delete a
            x6 = 3 in a

            print x1, x2, x3, x4, x5, x6
        }
    "#,
    )?;

    let mut cmd = Command::cargo_bin("yarawk")?;
    cmd.arg("-f").arg(source.path());
    cmd.assert()
        .success()
        .stdout(predicate::eq("1 1 0 0 0 0\n"));

    Ok(())
}

#[test]
fn multidimensional_in_and_delete() -> Result<(), Box<dyn std::error::Error>> {
    let source = assert_fs::NamedTempFile::new("source.awk")?;
    source.write_str(
        r#"
        BEGIN {
            a["hello"] = "world"
            a[0,6,9] = 4.2

            x1 = "hello" in a
            x2 = (0,6,9) in a
            delete a[0,6,9]
            x3 = (0,6,9) in a

            print x1, x2, x3
        }
    "#,
    )?;

    let mut cmd = Command::cargo_bin("yarawk")?;
    cmd.arg("-f").arg(source.path());
    cmd.assert().success().stdout(predicate::eq("1 1 0\n"));

    Ok(())
}

#[test]
fn array_variable_static_type() -> Result<(), Box<dyn std::error::Error>> {
    let source = assert_fs::NamedTempFile::new("source.awk")?;
    source.write_str(
        r#"
        BEGIN {
            x = 42
            x[1] = 2
            print x
        }
    "#,
    )?;

    let mut cmd = Command::cargo_bin("yarawk")?;
    cmd.arg("-f").arg(source.path());
    cmd.assert()
        .failure()
        .stderr(predicate::str::contains("was used as array"));

    Ok(())
}

#[test]
fn standard_math_functions() -> Result<(), Box<dyn std::error::Error>> {
    let source = assert_fs::NamedTempFile::new("source.awk")?;
    source.write_str(
        r#"
        BEGIN {
        print (atan2(4,2.) <= 1.11), 
              (cos(69) > 0.99),
              (sin(96) > 0.98), 
              (exp(4) > 54),
              (log(9) >= 2.19),
              (sqrt(9) == 3),
              int(4.332),
              (rand() >= 0 && rand() <= 1),
              (srand() != 0)
        }
    "#,
    )?;

    let mut cmd = Command::cargo_bin("yarawk")?;
    cmd.arg("-f").arg(source.path());
    cmd.assert()
        .success()
        .stdout(predicate::eq("1 1 1 1 1 1 4 1 1\n"));

    Ok(())
}

#[test]
fn standard_string_functions() -> Result<(), Box<dyn std::error::Error>> {
    let source = assert_fs::NamedTempFile::new("source.awk")?;
    source.write_str(
        r#"
        BEGIN {
            x = 1
            print length(x), tolower("HELLO"), toupper("hello"), index(1, x), index(0, x)

            y = "world"
            print substr(y, 3, -1)
            print substr(y, 2, 69420)
            print substr(y, 2, 3)
            printf("%s %s\n", "wow", substr(y, 2, 3))
        }
    "#,
    )?;

    let mut cmd = Command::cargo_bin("yarawk")?;
    cmd.arg("-f").arg(source.path());
    cmd.assert()
        .success()
        .stdout(predicate::eq("1 hello HELLO 1 0\n\norld\norl\nwow orl\n"));

    Ok(())
}

#[test]
fn function_sub() -> Result<(), Box<dyn std::error::Error>> {
    let source = assert_fs::NamedTempFile::new("source.awk")?;
    source.write_str(
        r#"
            BEGIN {
                original = "punky and punky: hello!"
                repl = "[\\& with &]"
                sub("pun.y", repl, original)
                print original
                print repl
            }
            {
                sub("pun.y", "[\\& with &]")
                print
            }
    "#,
    )?;

    let mut cmd = Command::cargo_bin("yarawk")?;
    let in1 = assert_fs::NamedTempFile::new("function_sub.txt")?;
    in1.write_str("hello punky and punky\n")?;
    cmd.arg("-f").arg(source.path()).arg(in1.path());

    cmd.assert().success().stdout(predicate::eq(
        "[& with punky] and punky: hello!\n[\\\\& with &]\nhello [& with punky] and punky\n",
    ));

    Ok(())
}

#[test]
fn function_gsub() -> Result<(), Box<dyn std::error::Error>> {
    let source = assert_fs::NamedTempFile::new("source.awk")?;
    source.write_str(
        r#"
            BEGIN {
                original = "punky and punky: hello!"
                repl = "[\\& with &]"
                gsub("pun.y", repl, original)
                print original
                print repl
            }
            {
                gsub("pun.y", "[\\& with &]")
                print
            }
    "#,
    )?;

    let mut cmd = Command::cargo_bin("yarawk")?;
    let in1 = assert_fs::NamedTempFile::new("function_gsub.txt")?;
    in1.write_str("hello punky and punky\n")?;
    cmd.arg("-f").arg(source.path()).arg(in1.path());

    cmd.assert().success().stdout(predicate::eq(
        "[& with punky] and [& with punky]: hello!\n[\\\\& with &]\nhello [& with punky] and [& with punky]\n",
    ));

    Ok(())
}

#[test]
fn function_close() -> Result<(), Box<dyn std::error::Error>> {
    let in1 = assert_fs::NamedTempFile::new("function_close.txt")?;
    let source = assert_fs::NamedTempFile::new("source.awk")?;
    source.write_str(
        format!(
            r#"
            BEGIN {{
                getline line < "{}"
                print line
                close("{}")
                getline line < "{}"
                print line
            }}
        "#,
            in1.path().to_str().unwrap(),
            in1.path().to_str().unwrap(),
            in1.path().to_str().unwrap(),
        )
        .as_str(),
    )?;

    let mut cmd = Command::cargo_bin("yarawk")?;
    in1.write_str("line 1\nline 2\n ")?;
    cmd.arg("-f").arg(source.path()).arg(in1.path());

    cmd.assert()
        .success()
        .stdout(predicate::eq("line 1\nline 1\n"));

    Ok(())
}

#[test]
fn function_system() -> Result<(), Box<dyn std::error::Error>> {
    let source = assert_fs::NamedTempFile::new("source.awk")?;
    source.write_str(
        r#"
            BEGIN {
                x = system("echo 'hello world'")
                print x
            }
    "#,
    )?;

    let mut cmd = Command::cargo_bin("yarawk")?;
    cmd.arg("-f").arg(source.path());

    cmd.assert()
        .success()
        .stdout(predicate::eq("hello world\n0\n"));

    Ok(())
}

#[test]
fn function_split() -> Result<(), Box<dyn std::error::Error>> {
    let source = assert_fs::NamedTempFile::new("source.awk")?;
    source.write_str(
        r#"
            BEGIN {
                original = "a1b2c3d"
                x = split(original, a, "\d")
                print "x is", x
                print a[0], a[1], a[2], a[3]
            }
            {
                split($0, a)
                print a[0], a[1]
            }
    "#,
    )?;

    let in1 = assert_fs::NamedTempFile::new("function_close.txt")?;
    in1.write_str("x y\n")?;
    let mut cmd = Command::cargo_bin("yarawk")?;
    cmd.arg("-f").arg(source.path()).arg(in1.path());

    cmd.assert()
        .success()
        .stdout(predicate::eq("x is 4\na b c d\nx y\n"));

    Ok(())
}

#[test]
fn function_match() -> Result<(), Box<dyn std::error::Error>> {
    let source = assert_fs::NamedTempFile::new("source.awk")?;
    source.write_str(
        r#"
            BEGIN {
                original = "line-a 1 line-b 1"
                print match(original, "1"), RSTART, RLENGTH
                print match(original, "2"), RSTART, RLENGTH
            }
    "#,
    )?;

    let mut cmd = Command::cargo_bin("yarawk")?;
    cmd.arg("-f").arg(source.path());

    cmd.assert()
        .success()
        .stdout(predicate::eq("8 8 1\n0 0 -1\n"));

    Ok(())
}

#[test]
fn printf_formatting() -> Result<(), Box<dyn std::error::Error>> {
    let source = assert_fs::NamedTempFile::new("source.awk")?;
    source.write_str(
        r#"
            BEGIN {
                s = "hello"
                d = 4.2
                i = 123
                printf("char: %c\ndecimal: %d\nexponential: %e\nfloat: %f\nthe g: %010.6g\noctal: %o\nstring: %010.4s\nunsigned hex: %x\njust the %%: %%",
                       s, d, d, d, d, d, s, i)
            }
    "#,
    )?;

    let mut cmd = Command::cargo_bin("yarawk")?;
    cmd.arg("-f").arg(source.path());

    cmd.assert().success().stdout(predicate::eq(
        "char: h
decimal: 4
exponential: 4.200000e0
float: 4.200000
the g: 004.200000
octal: 4
string: 00000hello
unsigned hex: 7b
just the %: %",
    ));

    Ok(())
}
