use gen_common::*;
use mat_common::*;
use mat_postfix::*;
use mat_methods::*;
use mat_ops::*;
use util::*;
use std::iter::*;

pub static IMPORTS: &'static str = "use std::ops::*;";

pub fn gen_matrix(n: usize) -> String {
    let gen = &MatGen {
        struct_name: format!("Mat{}", n),
        tpe: Type::F64,
        col_tpe: format!("Vec{}", n),
        col_builder: format!("vec{}", n),
        nr_cols: n,
        nr_rows: n,
        macro_builder_name: format!("mat{}", n),
    };
    template_file(gen)
}

fn template_file(gen: &MatGen) -> String { format! {"\
// Generated code.
{imports}

{template_struct}

{template_struct_impl}

{op_index}

{template_num_ops}

{macro_builder}
",
    imports = format!("{sized}\n{base}", sized = gen.sized_imports(), base = IMPORTS),
    template_struct = template_struct(gen),
    template_struct_impl = template_struct_impl(gen),
    op_index = op_index(gen),
    template_num_ops = template_num_ops(gen),
    macro_builder = macro_builder(gen),
}}

fn template_struct(gen: &MatGen) -> String { format! {"\
{doc_struct}
#[derive(Debug, PartialEq, Copy, Clone)]
pub struct {struct_name} {{ {body} }}",
    doc_struct = doc_struct(gen),
    struct_name = gen.struct_name,
    body = format!(
        "cols: [{col_tpe}; {nr_cols}]", col_tpe = gen.col_tpe, nr_cols = gen.nr_cols
    ),
}}

fn doc_struct(gen: &MatGen) -> String { format! {"\
/// Column major matrix with {c} columns and {r} rows.
///
/// Most operators and methods on matrices are performed in component-wise fashion. The notable
/// exceptions are matrix-matrix and matrix-vector multiplications.",
    c = gen.nr_cols,
    r = gen.nr_rows,
}}

fn template_struct_impl(gen: &MatGen) -> String { format! {"\
impl {struct_name} {{
    {fn_new}
    
    {fn_transpose}
}}",
    struct_name = gen.struct_name,
    fn_new = fn_new(gen),
    fn_transpose = fn_transpose(gen),
}}

fn fn_new(gen: &MatGen) -> String { format! {"\
    /// Constructs a new `{struct_name}`.
    pub fn new({args}) -> Self {{
         {struct_name} {{ cols: [{body}] }}
    }}",
    struct_name = gen.struct_name,
    args = gen.cols().map(|col| format!("{}: {}", col, gen.col_tpe)).concat(", "),
    body = gen.cols().concat(", "),
}}

fn macro_builder(gen: &MatGen) -> String { format! { "\
/// Builder macro for creating new {c}x{r} column major matrices.
///
/// # Examples
///
/// Create a new column major matrix with `{example_diag_arg}` on diagonal:
///
/// ```
/// #[macro_use] extern crate vexyz_math;
/// use vexyz_math::*;
///
/// # fn main() {{
/// let a = {macro_name}!({example_diag_arg});
/// let b = {struct_name}::new(
///     {example_diag_res},
/// );
/// assert_eq!(a, b);
/// # }}
/// ```
///
/// Create a new column major matrix with
///     {example_col_worded}:
///
/// ```
/// #[macro_use] extern crate vexyz_math;
/// use vexyz_math::*;
///
/// # fn main() {{
/// let a = {macro_name}!(
///     {example_col_args},
/// );
/// let b = {struct_name}::new(
///     {example_col_args},
/// );
/// assert_eq!(a, b);
/// # }}
/// ```
///
/// Create a new column major matrix with
///     {example_col_worded}:
///
/// ```
/// #[macro_use] extern crate vexyz_math;
/// use vexyz_math::*;
///
/// # fn main() {{
/// let a = {macro_name}!(
///     {example_n_args},
/// );
/// let b = {struct_name}::new(
///     {example_col_args},
/// );
/// assert_eq!(a, b);
/// # }}
/// ```
#[macro_export]
macro_rules! {macro_name} {{
    ($s:expr) => {{{{
        let s = $s as {tpe};
        {struct_name}::new(
            {body_1},
        )
    }}}};
    ({args_c}) => {{{{
        {struct_name}::new({body_c})
    }}}};
    ({args_c},) => {{{{
        {struct_name}::new({body_c})
    }}}};
    ({args_n}) => {{{{
        {struct_name}::new(
            {body_n},
        )
    }}}};
    ({args_n},) => {{{{
        {struct_name}::new(
            {body_n},
        )
    }}}};
}}",
    c = gen.nr_cols,
    r = gen.nr_rows,
    macro_name = gen.macro_builder_name,
    struct_name = gen.struct_name,
    tpe = gen.tpe,
    example_diag_arg = gen.rhs(0, 0),
    example_diag_res = (0..gen.nr_cols).map(|c| format!("{col_builder}!({col_body})",
        col_builder = gen.col_builder,
        col_body = (0..gen.nr_rows).map(|r| format!("{}",
            if r == c { gen.rhs(0, 0) } else { "0.0".to_string() } )
        ).concat(", "),
    )).concat(",\n///     "),
    example_col_worded = (0..gen.nr_cols).map(|c| format!(
        "`column{n} = {col_builder}({col_body})`",
        n = c,
        col_builder = gen.col_builder,
        col_body = (0..gen.nr_rows).map(|r| format!("{}", gen.rhs(c, r))).concat(", "),
    )).concat(",\n///     "),
    example_col_args = (0..gen.nr_cols).map(|c| format!(
        "{col_builder}!({col_body})",
        col_builder = gen.col_builder,
        col_body = (0..gen.nr_rows).map(|r| format!("{}", gen.rhs(c, r))).concat(", "),
    )).concat(",\n///     "),
    example_n_args = (0..gen.nr_cols).map(|c| format!(
        "{col_body}",
        col_body = (0..gen.nr_rows).map(|r| format!("{}", gen.rhs(c, r))).concat(", "),
    )).concat(",\n///     "),
    body_1 = (0..gen.nr_cols).map(|c| format!("{col_builder}!({col_body})",
        col_builder = gen.col_builder,
        col_body = (0..gen.nr_rows).map(|r| format!("{}",
            if r == c {"s"} else {"0"} )
        ).concat(", "),
    )).concat(",\n            "),
    args_c = gen.cols().map(|col| format!("${}:expr", col)).concat(", "),
    body_c = gen.cols().map(|col| format!("${}", col)).concat(", "),
    args_n = (0..gen.nr_cols).map(|c|
        (0..gen.nr_rows).map(|r| format!("$m{}{}:expr", c, r)).concat(", "),
    ).concat(",\n     "),
    body_n = (0..gen.nr_cols).map(|c| format!("{col_builder}!({col_body})",
        col_builder = gen.col_builder,
        col_body = (0..gen.nr_rows).map(|r|
            format!("$m{}{} as {}", c, r, gen.tpe)
        ).concat(", "),
    )).concat(",\n            "),
}}
