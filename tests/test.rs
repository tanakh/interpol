use std::io::Write;

#[test]
fn test() {
    use interpol::format;

    let foo = 123;
    assert_eq!(format!("{foo}"), "123");

    assert_eq!(format!("{123:x}"), "7b");

    assert_eq!(format!("{vec![1,2,3]:?}"), "[1, 2, 3]");

    let foo = 3.14;
    assert_eq!(format!("{foo:.10}"), "3.1400000000");

    let a = 3;
    let b = 4;
    assert_eq!(format!("{a} times {b} is {a * b}."), "3 times 4 is 12.");

    assert_eq!(
        format!("PI = {std::f64::consts::PI}"),
        "PI = 3.141592653589793"
    );

    assert_eq!(
        format!("PI = {std::f64::consts::PI:.10}"),
        "PI = 3.1415926536"
    );

    let world = "世界";
    assert_eq!(format!("ハロー {world}"), "ハロー 世界");

    assert_eq!(
        format!("PI = { 1.0_f64.atan() * 4.0 :.10}"),
        "PI = 3.1415926536"
    );

    assert_eq!(format!("{ { let t = 123; t * t } }"), "15129");
}

#[test]
fn test_escaping() {
    assert_eq!(format!("{{"), "{");
    assert_eq!(format!("}}"), "}");
    assert_eq!(format!("{{}}"), "{}");
    assert_eq!(format!("}}{{"), "}{");
}

#[test]
fn test_write() {
    let mut s = vec![];

    let foo = 123;
    interpol::write!(&mut s, "{foo}").unwrap();
    assert_eq!(String::from_utf8(s).unwrap(), "123");
}

#[test]
fn test_writeln() {
    let mut s = vec![];

    let foo = 123;
    interpol::writeln!(&mut s, "{foo}").unwrap();

    assert_eq!(String::from_utf8(s).unwrap(), "123\n");
}
