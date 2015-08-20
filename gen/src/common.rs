use std::fmt::{Display, Error, Formatter};

pub fn vec_getter(i: usize) -> String {
    format!("[{}]", i)
}

pub fn mat_getter(i: usize) -> String {
    format!("[{}]", i)
}

#[derive(Clone, Copy)]
pub enum Type {
    Bool,
    I32,
    F64,
}

impl Type {
    pub fn worded(&self) -> String {
        match *self {
            Type::Bool => "boolean".to_string(),
            Type::I32 => "integer".to_string(),
            Type::F64 => "floating point".to_string(),
        }
    }
}

impl Display for Type {
    fn fmt(&self, formatter: &mut Formatter) -> Result<(), Error> {
        match *self {
            Type::Bool => write!(formatter, "bool"),
            Type::I32 => write!(formatter, "i32"),
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
