mod gen_common;
mod vec_common;
mod mat_common;
mod mat_postfix;
mod mat_methods;
mod mat_ops;
mod util;

pub mod vector_gen_bool;
pub mod vector_gen_int;
pub mod vector_gen_float;
pub mod quaternion_gen;
pub mod matrix_gen;

//TODO vector*matrix
//TODO mutable index accessor
//TODO Implement modulo() method (do not overload %)

//TODO .extend() on mats, .sub_mat2() .sub_mat3() on mats
//TODO .extend(s) on Vectors .xyz() on Vectors

//TODO add .as_quat() on vec4
//TODO .as_vec4() on quat.

//TODO to_quat() to_mat3() to_angle_axis() on quat and mat3 (as applicable)

//TODO .apply(mat) on all mats

//MAYBE remove .sum() from quaterion (unless used for something else)
