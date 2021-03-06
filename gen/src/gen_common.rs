use std::fmt::{Display, Error, Formatter};

pub static XYZW: [&'static str; 4] = ["x", "y", "z", "w"];
pub static ABCD: [&'static str; 4] = ["a", "b", "c", "d"];

pub fn vec_getter(i: usize) -> String {
    format!(".{}()", XYZW[i])
}

pub fn quat_getter(i: usize) -> String {
    format!(".{}()", ABCD[i])
}

pub fn mat_getter(i: usize) -> String {
    format!("[{}]", i)
}

pub fn coerce(tpe: Type, arg: &str) -> String {
    if arg == "true" || arg == "false" {
        match tpe {
            Type::Bool => arg.to_string(),
            _ => panic!("Trying to coerce wrong type: {}.", tpe),
        }
    }
    else if arg.contains(".") || arg.to_lowercase().contains("e") {
        match tpe {
            Type::Bool | Type::U32 | Type::I32 => panic!("Trying to coerce wrong type: {}.", tpe),
            Type::F32 | Type::F64 => arg.to_string(),
        }
    }
    else {
        match tpe {
            Type::Bool | Type::U32 | Type::I32 => arg.to_string(),
            Type::F32 | Type::F64 => format!("{}.0", arg),
        }
    }
}

#[derive(Clone, Copy)]
pub enum Type {
    Bool,
    U32,
    I32,
    F32,
    F64,
}

impl Type {
    pub fn worded(&self) -> String {
        match *self {
            Type::Bool => "boolean".to_string(),
            Type::U32 => "unsigned integer".to_string(),
            Type::I32 => "integer".to_string(),
            Type::F32 => "32 bit floating point".to_string(),
            Type::F64 => "64 bit floating point".to_string(),
        }
    }
}

impl Display for Type {
    fn fmt(&self, formatter: &mut Formatter) -> Result<(), Error> {
        match *self {
            Type::Bool => write!(formatter, "bool"),
            Type::U32 => write!(formatter, "u32"),
            Type::I32 => write!(formatter, "i32"),
            Type::F32 => write!(formatter, "f32"),
            Type::F64 => write!(formatter, "f64"),
        }
    }
}

pub fn shorthands_bin_op_ref(
    trait_name: &str, fn_name: &str, op: &str,
    lhs_tpe: &str, rhs_tpe: &str, res_tpe: &str
) -> String {
    format! {"\
impl<'a> {trait_name}<{rhs_tpe}> for &'a {lhs_tpe} {{
    type Output = {res_tpe};
    
    /// Shorthand for `lhs {op} &rhs`.
    #[inline(always)] fn {fn_name}(self, rhs: {rhs_tpe}) -> {res_tpe} {{
        self {op} &rhs
    }}
}}

impl<'b> {trait_name}<&'b {rhs_tpe}> for {lhs_tpe} {{
    type Output = {res_tpe};
    
    /// Shorthand for `&lhs {op} rhs`.
    #[inline(always)] fn {fn_name}(self, rhs: &{rhs_tpe}) -> {res_tpe} {{
        &self {op} rhs
    }}
}}

impl {trait_name}<{rhs_tpe}> for {lhs_tpe} {{
    type Output = {res_tpe};
    
    /// Shorthand for `&lhs {op} &rhs`.
    #[inline(always)] fn {fn_name}(self, rhs: {rhs_tpe}) -> {res_tpe} {{
        &self {op} &rhs
    }}
}}",
        trait_name = trait_name, fn_name = fn_name, op = op,
        lhs_tpe = lhs_tpe, rhs_tpe = rhs_tpe, res_tpe = res_tpe,
    }
}

pub fn shorthands_op_bin_scalar(
    trait_name: &str, fn_name: &str, op: &str,
    lhs_tpe: &str, rhs_tpe: &str, res_tpe: &str
) -> String {
    format! {"\
impl {trait_name}<{rhs_tpe}> for {lhs_tpe} {{
    type Output = {res_tpe};
    
    /// Shorthand for `&lhs {op} rhs`.
    #[inline(always)] fn {fn_name}(self, rhs: {rhs_tpe}) -> {res_tpe} {{
        &self {op} rhs
    }}
}}",
        trait_name = trait_name, fn_name = fn_name, op = op,
        lhs_tpe = lhs_tpe, rhs_tpe = rhs_tpe, res_tpe = res_tpe,
    }
}

pub fn shorthand_op_unary(
    trait_name: &str, fn_name: &str, op: &str,
    arg_tpe: &str, res_tpe: &str
) -> String {
    format! {"\
impl {trait_name} for {arg_tpe} {{
    type Output = {res_tpe};
    
    /// Shorthand for `{op}&arg`.
    #[inline(always)] fn {fn_name}(self) -> {res_tpe} {{
        {op}&self
    }}
}}",
        trait_name = trait_name, fn_name = fn_name, op = op,
        arg_tpe = arg_tpe, res_tpe = res_tpe,
    }
}
