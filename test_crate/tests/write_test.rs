#[test]
pub fn t1() {
    let v = vec![("k", "v"), ("k2", "v2")];

    let mut out = String::new();
    jprop::write(&v, &mut out, "\n").unwrap();

    assert_eq!(&out, "k=v\nk2=v2\n");
}

#[test]
pub fn t2() {
    let v = vec![("k", "v"), ("k2", "v2ä")];

    let mut out = String::new();
    jprop::write(&v, &mut out, "\n").unwrap();

    assert_eq!(&out, "k=v\nk2=v2ä\n");
}

#[test]
pub fn t3() {
    let v = vec![("k", "v"), ("k2", "v2𝕊")];

    let mut out = String::new();
    jprop::write(&v, &mut out, "\n").unwrap();

    assert_eq!(&out, "k=v\nk2=v2𝕊\n");
}

#[test]
pub fn t4() {
    let v = vec![("k", "v"), ("k2", "v2𝕊")];

    let mut out = Vec::new();
    jprop::write_ascii(&v, &mut out, "\n").unwrap();

    let str = String::from_utf8(out).unwrap();

    assert_eq!(&str, "k=v\nk2=v2\\uD835\\uDD4A\n");

    let n = jprop::parse_str_to_vec(&str).expect("Parsing failed");
    assert_eq!(format!("{n:?}").as_str(), "[(\"k\", \"v\"), (\"k2\", \"v2𝕊\")]");
}
