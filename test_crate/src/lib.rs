#[cfg(doctest)]
#[doc = include_str!("../../README.md")]
struct ReadmeDocTests;

#[test]
pub fn t() {
    println!("{:04x}", 23u32);
}