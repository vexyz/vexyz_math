#[macro_use]
extern crate vexyz_math;
use vexyz_math::*;

fn main() {
    println!("Math main");
    let q0 = quat!().rotate_x(90_f32.to_rad());
    let q1 = quat!().rotate_y(90_f32.to_rad());
    let q = q0.rotate(q1);
    println!("q = {}", q);
    let u = q.rotate_vec(vec3!(1, 1, 0));
    println!("u = {}", u);
    assert!(u.approx_equal(vec3!(0, 1, 1), 1e-8));
}
