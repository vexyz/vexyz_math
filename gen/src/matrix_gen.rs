use common::*;
use util::*;
use std::iter::*;

static IMPORTS: &'static str = "use std::ops::*;";
static COLS: [&'static str; 4] = ["col0", "col1", "col2", "col3"];

//XXX break up into base/operators/arg_methods/noarg_methods
pub fn gen_matrix(n: usize) -> String {
    let gen = &MatGen {
        struct_name: format!("Mat{}", n),
        tpe: Type::F64,
        col_tpe: format!("Vec{}", n),
        col_builder: format!("vec{}", n),
        nr_cols: n,
        nr_rows: n,
        builder_macro_name: format!("mat{}", n),
    };
    template_matrix(gen)
}

fn template_matrix(gen: &MatGen) -> String { format! {"\
// Generated code.
{imports}

{struct_def}

{template_struct_impl}

{op_index}

{numerical_ops}

{builder_macro}
",
    imports = format!("{sized}\n{base}", sized = gen.sized_imports(), base = IMPORTS),
    struct_def = gen.struct_def(),
    template_struct_impl = gen.template_struct_impl(),
    op_index = gen.op_index(),
    numerical_ops = numerical_ops(gen),
    builder_macro = gen.builder_macro(),
}}

fn numerical_ops(gen: &MatGen) -> String { format! { "\
{op_add_mat}

{op_add_scalar}

{op_sub_mat}

{op_sub_scalar}

{op_mul_mat}

{op_mul_vec}

{op_mul_scalar}

{op_div_mat}

{op_div_scalar}

{op_neg}",
    op_add_mat = gen.bin_op_mat("Add", "add", "+",
        "Performs component-wise addition of two matrices"),
    
    op_sub_mat = gen.bin_op_mat("Sub", "sub", "-",
        "Subtracts each component of the `rhs` matrix from the \
        \n    /// corresponding component of the `lhs` matrix"),
    
    op_div_mat = gen.bin_op_mat("Div", "div", "/",
        "Divides each component of the `lhs` matrix by the \
        \n    /// corresponding component of the `rhs` matrix"),

    op_mul_mat = gen.template_mul_mat(),
        
    op_mul_vec = gen.template_mul_vec(),
    
    op_add_scalar = gen.template_op_bin_scalar("Add", "add", "+",
        "Adds a scalar to each component of a matrix"),
    
    op_sub_scalar = gen.template_op_bin_scalar("Sub", "sub", "-",
        "Subtracts a scalar from each component of a matrix"),
    
    op_mul_scalar = gen.template_op_bin_scalar("Mul", "mul", "*",
        "Multiplies each component of a matrix by a scalar"),
    
    op_div_scalar = gen.template_op_bin_scalar("Div", "div", "/",
        "Divides each component of a {doc_name} by a scalar"),
    
    op_neg = gen.template_op_unary("Neg", "neg", "-", "negation"),
}}

struct MatGen {
    struct_name: String,
    tpe: Type,
    col_tpe: String,
    col_builder: String,
    nr_cols: usize,
    nr_rows: usize,
    builder_macro_name: String,
}

impl MatGen {
    fn sized_imports(&self) -> String {
        format!("use vec{n}::*;", n = self.nr_rows)
    }
    
    fn getters(&self) -> Box<Iterator<Item = String>> {
        Box::new((0 .. self.nr_cols).map(|i| mat_getter(i)))
    }
    
    fn cols(&self) -> Box<Iterator<Item = String>> {
        Box::new(COLS.iter().take(self.nr_cols).map(|s| s.to_string()))
    }
    
    fn lhs(&self, c: usize, r: usize) -> String {
        format!("{}.{}", c, r + 1)
    }
    
    fn rhs(&self, c: usize, r: usize) -> String {
        format!("{}.{}", c, r + 5)
    }
    
    fn lhs_col(&self, c: usize) -> String {
        (0..self.nr_rows).map(|r| self.lhs(c, r)).concat(", ")
    }
    
    fn rhs_col(&self, c: usize) -> String {
        (0..self.nr_rows).map(|r| self.rhs(c, r)).concat(", ")
    }
    
    fn builder_macro(&self) -> String { format! { "\
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
        c = self.nr_cols,
        r = self.nr_rows,
        macro_name = self.builder_macro_name,
        struct_name = self.struct_name,
        tpe = self.tpe,
        example_diag_arg = self.rhs(0, 0),
        example_diag_res = (0..self.nr_cols).map(|c| format!("{col_builder}!({col_body})",
            col_builder = self.col_builder,
            col_body = (0..self.nr_rows).map(|r| format!("{}",
                if r == c { self.rhs(0, 0) } else { "0.0".to_string() } )
            ).concat(", "),
        )).concat(",\n///     "),
        example_col_worded = (0..self.nr_cols).map(|c| format!(
            "`column{n} = {col_builder}({col_body})`",
            n = c,
            col_builder = self.col_builder,
            col_body = (0..self.nr_rows).map(|r| format!("{}", self.rhs(c, r))).concat(", "),
        )).concat(",\n///     "),
        example_col_args = (0..self.nr_cols).map(|c| format!(
            "{col_builder}!({col_body})",
            col_builder = self.col_builder,
            col_body = (0..self.nr_rows).map(|r| format!("{}", self.rhs(c, r))).concat(", "),
        )).concat(",\n///     "),
        example_n_args = (0..self.nr_cols).map(|c| format!(
            "{col_body}",
            col_body = (0..self.nr_rows).map(|r| format!("{}", self.rhs(c, r))).concat(", "),
        )).concat(",\n///     "),
        body_1 = (0..self.nr_cols).map(|c| format!("{col_builder}!({col_body})",
            col_builder = self.col_builder,
            col_body = (0..self.nr_rows).map(|r| format!("{}",
                if r == c {"s"} else {"0"} )
            ).concat(", "),
        )).concat(",\n            "),
        args_c = self.cols().map(|col| format!("${}:expr", col)).concat(", "),
        body_c = self.cols().map(|col| format!("${}", col)).concat(", "),
        args_n = (0..self.nr_cols).map(|c|
            (0..self.nr_rows).map(|r| format!("$m{}{}:expr", c, r)).concat(", "),
        ).concat(",\n     "),
        body_n = (0..self.nr_cols).map(|c| format!("{col_builder}!({col_body})",
            col_builder = self.col_builder,
            col_body = (0..self.nr_rows).map(|r|
                format!("$m{}{} as {}", c, r, self.tpe)
            ).concat(", "),
        )).concat(",\n            "),
    }}
    
    fn struct_def(&self) -> String { format! {"\
{struct_doc}
#[derive(Debug, PartialEq, Copy, Clone)]
pub struct {struct_name} {{ {body} }}",
        struct_doc = self.struct_doc(),
        struct_name = self.struct_name,
        body = format!(
            "cols: [{col_tpe}; {nr_cols}]", col_tpe = self.col_tpe, nr_cols = self.nr_cols
        ),
    }}
    
    fn struct_doc(&self) -> String { format! {"\
/// Column major matrix with {c} columns and {r} rows.
///
/// Most operators and methods on matrices are performed in component-wise fashion. The notable
/// exceptions are matrix-matrix and matrix-vector multiplications.",
        c = self.nr_cols,
        r = self.nr_rows,
    }}
    
    fn template_struct_impl(&self) -> String { format! {"\
impl {struct_name} {{
    {fn_new}
    
    {fn_transpose}
}}",
        struct_name = self.struct_name,
        fn_new = self.fn_new(),
        fn_transpose = self.fn_transpose(),
    }}
    
    fn fn_new(&self) -> String { format! {"\
    /// Constructs a new `{struct_name}`.
    pub fn new({args}) -> Self {{
         {struct_name} {{ cols: [{body}] }}
    }}",
        struct_name = self.struct_name,
        args = self.cols().map(|col| format!("{}: {}", col, self.col_tpe)).concat(", "),
        body = self.cols().concat(", "),
    }}
    
    fn fn_transpose(&self) -> String { format! {"\
    /// Transpose XXX doc this.
    pub fn transpose(&self) -> {struct_name} {{
        {struct_name}::new(
            {body},
        )
    }}",
        struct_name = self.struct_name,
        body = (0..self.nr_cols).map(|c| format!(
            "{col_tpe}::new({col_body})",
            col_tpe = self.col_tpe,
            col_body = (0..self.nr_rows).map(|r| format!(
                "self{col_r}{row_c}",
                col_r = mat_getter(r), row_c = vec_getter(c)
            )).concat(", "),
        )).concat(",\n            "),
    }}
    
    fn op_index(&self) -> String { format! {"\
impl Index<usize> for {struct_name} {{
    type Output = {col_tpe};
    
    /// Index notation for accessing matrix columns.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {{
    /// let m = {builder_macro}!(
    ///     {example_args},
    /// );
    /// assert_eq!(m, {builder_macro}!({example_res}));
    /// # }}
    /// ```
    ///
    /// # Panics
    ///
    /// Will panic if the index is greater than {max_index}.
    #[inline(always)] fn index<'a>(&'a self, i: usize) -> &'a {col_tpe} {{
        &self.cols[i]
    }}
}}",
        builder_macro = self.builder_macro_name,
        struct_name = self.struct_name,
        max_index = self.nr_cols - 1,
        col_tpe = self.col_tpe,
        example_args = (0..self.nr_cols).map(|i| format!("{col_builder}!({col_body})",
            col_builder = self.col_builder, col_body = self.lhs_col(i))
        ).concat(",\n    ///     "),
        example_res = (0..self.nr_cols).map(|i| format!("m[{}]", i)).concat(", "),
    }}
    
    fn bin_op_mat(&self, trait_name: &str, fn_name: &str, op: &str, doc_desc: &str) -> String {
        format! {"\
impl<'a, 'b> {trait_name}<&'b {struct_name}> for &'a {struct_name} {{
    type Output = {struct_name};
    
    /// {doc_desc} producing a new matrix.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {{
    /// let a = {builder_macro}!(
    ///     {example_lhs_args},
    /// );
    /// let b = {builder_macro}!(
    ///     {example_rhs_args},
    /// );
    /// let c = {builder_macro}!(
    ///     {example_res},
    /// );
    /// assert_eq!(a {op} b, c);
    /// # }}
    /// ```
    fn {fn_name}(self, rhs: &{struct_name}) -> {struct_name} {{
        {struct_name}::new(
            {body}
        )
    }}
}}

{shorthands}",
        trait_name = trait_name,
        fn_name = fn_name,
        op = op,
        doc_desc = doc_desc,
        builder_macro = self.builder_macro_name,
        struct_name = self.struct_name,
        body = self.getters().map(|getter| format!(
            "self{getter} {op} rhs{getter}", getter = getter, op = op
        )).concat(", "),
        example_lhs_args = (0..self.nr_cols).map(|i| self.lhs_col(i)).concat(",\n    ///     "),
        example_rhs_args = (0..self.nr_cols).map(|i| self.rhs_col(i)).concat(",\n    ///     "),
        example_res = (0..self.nr_cols).map(|c|
            (0..self.nr_rows).map(|r| format!("{lhs} {op} {rhs}",
                lhs = self.lhs(c, r), rhs = self.rhs(c, r), op = op
            )).concat(", "),
        ).concat(",\n    ///     "),
        shorthands = shorthands_bin_op_ref(
            trait_name, fn_name, op, &self.struct_name, &self.struct_name, &self.struct_name
        ),
    }}
    
    fn template_mul_mat(&self) -> String {
        format! {"\
impl<'a, 'b> Mul<&'b {struct_name}> for &'a {struct_name} {{
    type Output = {struct_name};
    
    /// Performs algebraic multiplication of two matrices producing a new matrix.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {{
    /// let a = {builder_macro}!(
    ///     {example_lhs_args},
    /// );
    /// let b = {builder_macro}!(
    ///     {example_rhs_args},
    /// );
    /// let c = {builder_macro}!(
    ///     {example_res},
    /// );
    /// assert_eq!(a * b, c);//XXX fix this
    /// # }}
    /// ```
    fn mul(self, rhs: &{struct_name}) -> {struct_name} {{
        let t = self.transpose();
        {struct_name}::new(
            {body},
        )
    }}
}}

{shorthands}",
        builder_macro = self.builder_macro_name,
        struct_name = self.struct_name,
        body = (0..self.nr_cols).map(|c| format!(
            "{col_tpe}::new({col_body})",
            col_tpe = self.col_tpe,
            col_body = (0..self.nr_rows).map(|r| format!(
                "t{col_r}.dot(rhs{col_c})",
                col_r = mat_getter(r), col_c = mat_getter(c)
            )).concat(", "),
        )).concat(",\n            "),
        example_lhs_args = (0..self.nr_cols).map(|i| self.lhs_col(i)).concat(",\n    ///     "),
        example_rhs_args = (0..self.nr_cols).map(|i| self.rhs_col(i)).concat(",\n    ///     "),
        example_res = (0..self.nr_cols).map(|c|
            (0..self.nr_rows).map(|r| format!("{lhs} * {rhs}",
                lhs = self.lhs(c, r), rhs = self.rhs(c, r)
            )).concat(", "),
        ).concat(",\n    ///     "),
        shorthands = shorthands_bin_op_ref(
            "Mul", "mul", "*", &self.struct_name, &self.struct_name, &self.struct_name
        ),
    }}
    
    fn template_mul_vec(&self) -> String {
        format! {"\
impl<'a, 'b> Mul<&'b {col_tpe}> for &'a {struct_name} {{
    type Output = {col_tpe};
    
    /// Performs algebraic multiplication of a matrix by a vector, producing a new vector.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {{
    /// let a = {builder_macro}!(
    ///     {example_lhs_args},
    /// );
    /// let b = {builder_macro}!(
    ///     {example_rhs_args},
    /// );
    /// let c = {builder_macro}!(
    ///     {example_res},
    /// );
    /// assert_eq!(a * b, c);//XXX fix this
    /// # }}
    /// ```
    fn mul(self, rhs: &{col_tpe}) -> {col_tpe} {{
        let t = self.transpose();
        {col_tpe}::new(
            {body},
        )
    }}
}}

{shorthands}",
        builder_macro = self.builder_macro_name,
        struct_name = self.struct_name,
        col_tpe = self.col_tpe,
        body = (0..self.nr_rows).map(|r| format!(
            "t{col_r}.dot(rhs)", col_r = mat_getter(r),
        )).concat(", "),
        example_lhs_args = (0..self.nr_cols).map(|i| self.lhs_col(i)).concat(",\n    ///     "),
        example_rhs_args = (0..self.nr_cols).map(|i| self.rhs_col(i)).concat(",\n    ///     "),
        example_res = (0..self.nr_cols).map(|c|
            (0..self.nr_rows).map(|r| format!("{lhs} * {rhs}",
                lhs = self.lhs(c, r), rhs = self.rhs(c, r)
            )).concat(", "),
        ).concat(",\n    ///     "),
        shorthands = shorthands_bin_op_ref(
            "Mul", "mul", "*", &self.struct_name, &self.col_tpe, &self.col_tpe
        ),
    }}
    
    fn template_op_bin_scalar(&self, trait_name: &str, fn_name: &str, op: &str, doc_desc: &str) -> String {
        format! {"\
impl<'a> {trait_name}<{tpe}> for &'a {struct_name} {{
    type Output = {struct_name};
    
    /// {doc_desc} producing a new matrix.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {{
    /// let a = {builder_macro}!(
    ///     {example_lhs_args},
    /// );
    /// let b = {example_rhs_arg};
    /// let c = {builder_macro}!(
    ///     {example_res},
    /// );
    /// assert_eq!(a {op} b, c);
    /// # }}
    /// ```
    fn {fn_name}(self, rhs: {tpe}) -> {struct_name} {{
        {struct_name}::new({body})
    }}
}}

{shorthands}",
        trait_name = trait_name,
        fn_name = fn_name,
        op = op,
        doc_desc = doc_desc,
        builder_macro = self.builder_macro_name,
        struct_name = self.struct_name,
        tpe = self.tpe,
        body = self.getters().map(|getter| format!(
            "self{getter} {op} rhs", getter = getter, op = op
        )).concat(", "),
        example_lhs_args = (0..self.nr_cols).map(|i| self.lhs_col(i)).concat(",\n    ///     "),
        example_rhs_arg = "2.0",
        example_res = (0..self.nr_cols).map(|c|
            (0..self.nr_rows).map(|r| format!("{lhs} {op} {rhs}",
                lhs = self.lhs(c, r), rhs = "2.0", op = op
            )).concat(", "),
        ).concat(",\n    ///     "),
        shorthands = shorthands_op_bin_scalar(
            trait_name, fn_name, op, &self.struct_name, &format!("{}", self.tpe), &self.struct_name
        ),
    }}
    
    fn template_op_unary(&self, trait_name: &str, fn_name: &str, op: &str, doc_verb: &str) -> String {
        format! {"\
impl<'a> {trait_name} for &'a {struct_name} {{
    type Output = {struct_name};
    
    /// Applies {doc_verb} to each component of a matrix producing a new matrix.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {{
    /// let a = {op}{builder_macro}!(
    ///     {example_args},
    /// );
    /// let b = {builder_macro}!(
    ///     {example_res},
    /// );
    /// assert_eq!(a, b);
    /// # }}
    /// ```
    fn {fn_name}(self) -> {struct_name} {{
        {struct_name}::new({body})
    }}
}}

{shorthand}",
        trait_name = trait_name,
        fn_name = fn_name,
        op = op,
        doc_verb = doc_verb,
        builder_macro = self.builder_macro_name,
        struct_name = self.struct_name,
        body = self.getters().map(|getter| format!(
            "{op}self{getter}", getter = getter, op = op
        )).concat(", "),
        example_args = (0..self.nr_cols).map(|i| self.lhs_col(i)).concat(",\n    ///     "),
        example_res = (0..self.nr_cols).map(|c|
            (0..self.nr_rows).map(|r| format!("{op}{arg}",
                arg = self.lhs(c, r), op = op
            )).concat(", "),
        ).concat(",\n    ///     "),
        shorthand = shorthand_op_unary(
            trait_name, fn_name, op, &self.struct_name, &self.struct_name
        ),
    }}
}
