#[macro_use]
extern crate vexyz_math;
use vexyz_math::*;


fn main() {
    println!("Math main");
    let _u = vec3!(2);
    let m = mat2!(2) * mat2!(3);
    println!("{:?}", m);
    test();
}

fn test() {
    let u = vec2!(20, 30).dot(vec2!(2, 3));
    assert_eq!(u, 20.0 * 2.0 + 30.0 * 3.0);
}
