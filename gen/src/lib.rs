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


// GENERIC:
//TODO rework NameOps naming
//TODO rework QuatQuatOps double naming
//TODO .xyz() on Vectors
//TODO .apply(mat) on all mats
//TODO add binary operations on integer vectors
//TODO add u32 vectors
//TODO to_ivec2(), to_uvec2(), to to_bvec2(), to_vec2() conversions

//GENERIC ON VECS AND SCALARS, U, I, F types
/*
to_deg
to_rad
sin
cos
tan
asin
acos
atan
sinh
cosh
tanh
asinh
acosh
atanh
pow
exp
log
exp2
log2
sqrt
inverse_sqrt
sign
floor
trunc
round
round_even
ceil
fract
mod
mod_vec
modf
min
min_vec
max
max_vec
clamp
clap_vec
mix
lerp_vec
step
step_vec
smoothstep
smoothstep_vec
isinf
isnan
is_valid_number //better name?
distance
normalize ??
faceforward
reflect
refract
smootherstep
smootherstep_vec
*/

// CUSTOM:
//TODO slerp on quat
//TODO inverse on mats
//TODO add .as_quat() on vec4
//TODO .extend() on mats, .sub_mat2() .sub_mat3() on mats
//TODO .extend(s) on Vectors
//TODO to_quat() to_mat3() to_angle_axis() on quat and mat3 (as applicable)
//TODO to_rotation_angle() on mat2
//TODO static look_at() on mat3
//TODO add rotate(s) to mat2 and rotate_xyz() to mat3
//TODO two static perspective and one static ortho on mat4
//TODO determinan on mats
//TODO component_mul on mats
//TODO outer product on vecs

