#[macro_use]
extern crate vexyz_math;
use vexyz_math::*;


fn main() {
    println!("Math main");
    let _u = vec3!(2);
    let m = mat2!(2) * mat2!(3);
    println!("{}", m);
    test();
}

fn test() {
    ((1 + 2 )as f64).sqrt();
}
