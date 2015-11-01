use gen_common::*;
use mat_common::*;
use util::*;
use std::iter::*;

pub fn template_postfix(gen: &MatGen) -> String { format! {"\
    {fn_transpose}\
",
    fn_transpose = fn_transpose(gen),
}}

pub fn fn_transpose(gen: &MatGen) -> String { format! {"\
    /// Returns transpose of the matrix.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {{
    /// let a = {macro_builder}!(
    ///     {example_args},
    /// );
    /// let b = {macro_builder}!(
    ///     {example_res},
    /// );
    /// assert_eq!(a.transpose(), b);
    /// # }}
    /// ```
    pub fn transpose(&self) -> {struct_name} {{
        {struct_name}::new(
            {body},
        )
    }}",
    struct_name = gen.struct_name,
    body = (0..gen.nr_cols).map(|c| format!(
        "{col_tpe}::new({col_body})",
        col_tpe = gen.col_tpe,
        col_body = (0..gen.nr_rows).map(|r| format!(
            "self{col_r}{row_c}",
            col_r = mat_getter(r), row_c = vec_getter(c)
        )).concat(", "),
    )).concat(",\n            "),
    macro_builder = gen.macro_builder_name,
    example_args = (0..gen.nr_cols).map(|c|
        (0..gen.nr_rows).map(|r| gen.lhs(c, r)
    ).concat(", ")).concat(",\n    ///     "),
    example_res = (0..gen.nr_rows).map(|r|
        (0..gen.nr_cols).map(|c| gen.lhs(c, r)
    ).concat(", ")).concat(",\n    ///     "),
}}

//XXX not worth generating these, do hardcoded insert for all custom methods
/*
pub fn fn_determinant(gen: &MatGen) -> String {
    assert_eq!(gen.nr_cols, gen.nr_rows);
    match gen.nr_cols {
        2 => format!{"\
    //XXX doc all of these
    fn determinant(&self) -> {tpe} {{
        {m00}*{m11} - {m10}*{m01}
    }}",
                tpe = gen.tpe,
                m00 = format!("self{}{}", mat_getter(0), vec_getter(0)),
                m01 = format!("self{}{}", mat_getter(0), vec_getter(1)),
                m10 = format!("self{}{}", mat_getter(1), vec_getter(0)),
                m11 = format!("self{}{}", mat_getter(1), vec_getter(1)),
            },
        3 => format!{"\
    fn determinant(&self) -> {tpe} {{
        let a0 = {m11}*{m22} - {m21}*{m12};
        let a1 = {m21}*{m02} - {m01}*{m22};
        let a2 = {m01}*{m12} - {m11}*{m02};
    
        {m00}*a0 + {m10}*a1 + {m20}*a2
    }}",
                tpe = gen.tpe,
                m00 = format!("self{}{}", mat_getter(0), vec_getter(0)),
                m01 = format!("self{}{}", mat_getter(0), vec_getter(1)),
                m02 = format!("self{}{}", mat_getter(0), vec_getter(2)),
                m10 = format!("self{}{}", mat_getter(1), vec_getter(0)),
                m11 = format!("self{}{}", mat_getter(1), vec_getter(1)),
                m12 = format!("self{}{}", mat_getter(1), vec_getter(2)),
                m20 = format!("self{}{}", mat_getter(2), vec_getter(0)),
                m21 = format!("self{}{}", mat_getter(2), vec_getter(1)),
                m22 = format!("self{}{}", mat_getter(2), vec_getter(2)),
            },
        4 => format!{"\
    fn determinant(&self) -> {tpe} {{
        let a0 = {m00}*{m11} - {m10}*{m01};
        let a1 = {m00}*{m21} - {m20}*{m01};
        let a2 = {m00}*{m31} - {m30}*{m01};
        let a3 = {m10}*{m21} - {m20}*{m11};
        let a4 = {m10}*{m31} - {m30}*{m11};
        let a5 = {m20}*{m31} - {m30}*{m21};
        let b0 = {m02}*{m13} - {m12}*{m03};
        let b1 = {m02}*{m23} - {m22}*{m03};
        let b2 = {m02}*{m33} - {m32}*{m03};
        let b3 = {m12}*{m23} - {m22}*{m13};
        let b4 = {m12}*{m33} - {m32}*{m13};
        let b5 = {m22}*{m33} - {m32}*{m23};
    
        a0*b5 - a1*b4 + a2*b3 + a3*b2 - a4*b1 + a5*b0
    }}",
                tpe = gen.tpe,
                m00 = format!("self{}{}", mat_getter(0), vec_getter(0)),
                m01 = format!("self{}{}", mat_getter(0), vec_getter(1)),
                m02 = format!("self{}{}", mat_getter(0), vec_getter(2)),
                m03 = format!("self{}{}", mat_getter(0), vec_getter(3)),
                m10 = format!("self{}{}", mat_getter(1), vec_getter(0)),
                m11 = format!("self{}{}", mat_getter(1), vec_getter(1)),
                m12 = format!("self{}{}", mat_getter(1), vec_getter(2)),
                m13 = format!("self{}{}", mat_getter(1), vec_getter(3)),
                m20 = format!("self{}{}", mat_getter(2), vec_getter(0)),
                m21 = format!("self{}{}", mat_getter(2), vec_getter(1)),
                m22 = format!("self{}{}", mat_getter(2), vec_getter(2)),
                m23 = format!("self{}{}", mat_getter(2), vec_getter(3)),
                m30 = format!("self{}{}", mat_getter(3), vec_getter(0)),
                m31 = format!("self{}{}", mat_getter(3), vec_getter(1)),
                m32 = format!("self{}{}", mat_getter(3), vec_getter(2)),
                m33 = format!("self{}{}", mat_getter(3), vec_getter(3)),
            },
        _ => unreachable!(),
    }
}
*/