use gen_common::*;
use mat_common::*;
use util::*;
use std::iter::*;

pub fn fn_transpose(gen: &MatGen) -> String { format! {"\
    /// Transpose XXX doc this.
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
}}
