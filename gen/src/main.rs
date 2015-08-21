extern crate vexyz_math_gen;
use vexyz_math_gen::vector_gen_bool;
use vexyz_math_gen::vector_gen_int;
use vexyz_math_gen::vector_gen_usize;
use vexyz_math_gen::vector_gen_float;
use vexyz_math_gen::quaternion_gen;
use vexyz_math_gen::matrix_gen;

use std::io::prelude::*;
use std::fs::File;

fn main() {
    println!("generating math...");
    let target_dir = "../src/";
    write_vectors(target_dir);
    write_quaternion(target_dir);
    write_matrices(target_dir);
    println!("done!");
}

fn write_vectors(target_dir: &str) {
    println!("  generating vectors...");
    for n in 2 .. 5 {
        write_bool_vector(target_dir, n);
        write_int_vector(target_dir, n);
        write_usize_vector(target_dir, n);
        write_float_vector(target_dir, n);
    }
    println!("  done!");
}

fn write_matrices(target_dir: &str) {
    println!("  generating matrices...");
    for n in 2 .. 5 {
        write_matrix(target_dir, n);
    }
    println!("  done!");
}

fn write_bool_vector(target_dir: &str, n: usize) {
    write_vector(target_dir, format!("bvec{}.rs", n), vector_gen_bool::gen_bool_vector(n))
}

fn write_int_vector(target_dir: &str, n: usize) {
    write_vector(target_dir, format!("ivec{}.rs", n), vector_gen_int::gen_int_vector(n))
}

fn write_usize_vector(target_dir: &str, n: usize) {
    write_vector(target_dir, format!("uvec{}.rs", n), vector_gen_usize::gen_usize_vector(n))
}

fn write_float_vector(target_dir: &str, n: usize) {
    write_vector(target_dir, format!("vec{}.rs", n), vector_gen_float::gen_float_vector(n))
}

fn write_vector(target_dir: &str, file_name: String, contents: String) {
    print!("    generating {}...", file_name);
    let mut file = File::create(target_dir.to_string() + &file_name).unwrap();
    file.write_all(contents.as_bytes()).unwrap();
    println!(" done!");
}

fn write_quaternion(target_dir: &str) {
    println!("  generating quaternion...");
    write_vector(target_dir, "quat.rs".to_string(), quaternion_gen::gen_quaternion());
    println!("  done!");
}

fn write_matrix(target_dir: &str, n: usize) {
    let file_name = format!("mat{}.rs", n);
    print!("    generating {}...", file_name);
    let mut file = File::create(target_dir.to_string() + &file_name).unwrap();
    file.write_all(matrix_gen::gen_matrix(n).as_bytes()).unwrap();
    println!(" done!");
}
