use gen_common::*;
use mat_common::*;
use util::*;
use std::iter::*;

pub fn op_index(gen: &MatGen) -> String { format! {"\
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
    /// let m = {macro_builder}!(
    ///     {example_args},
    /// );
    /// assert_eq!(m, {macro_builder}!({example_res}));
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
    macro_builder = gen.macro_builder_name,
    struct_name = gen.struct_name,
    max_index = gen.nr_cols - 1,
    col_tpe = gen.col_tpe,
    example_args = (0..gen.nr_cols).map(|i| format!("{col_builder}!({col_body})",
        col_builder = gen.col_builder, col_body = gen.lhs_col(i))
    ).concat(",\n    ///     "),
    example_res = (0..gen.nr_cols).map(|i| format!("m[{}]", i)).concat(", "),
}}

pub fn template_ops(gen: &MatGen) -> String { format! { "\
{op_index}

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
    op_index = op_index(gen),

    op_add_mat = template_op_bin_mat(gen, "Add", "add", "+",
        "Performs component-wise addition of two matrices"),
    
    op_sub_mat = template_op_bin_mat(gen, "Sub", "sub", "-",
        "Subtracts each component of the `rhs` matrix from the \
        \n    /// corresponding component of the `lhs` matrix"),
    
    op_div_mat = template_op_bin_mat(gen, "Div", "div", "/",
        "Divides each component of the `lhs` matrix by the \
        \n    /// corresponding component of the `rhs` matrix"),

    op_mul_mat = template_mul_mat(gen),
        
    op_mul_vec = template_mul_vec(gen),
    
    op_add_scalar = template_op_bin_scalar(gen, "Add", "add", "+",
        "Adds a scalar to each component of a matrix"),
    
    op_sub_scalar = template_op_bin_scalar(gen, "Sub", "sub", "-",
        "Subtracts a scalar from each component of a matrix"),
    
    op_mul_scalar = template_op_bin_scalar(gen, "Mul", "mul", "*",
        "Multiplies each component of a matrix by a scalar"),
    
    op_div_scalar = template_op_bin_scalar(gen, "Div", "div", "/",
        "Divides each component of a {doc_name} by a scalar"),
    
    op_neg = template_op_unary(gen, "Neg", "neg", "-", "negation"),
}}

fn template_op_bin_mat(
    gen: &MatGen, trait_name: &str, fn_name: &str, op: &str, doc_desc: &str) -> String
{
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
    /// let a = {macro_builder}!(
    ///     {example_lhs_args},
    /// );
    /// let b = {macro_builder}!(
    ///     {example_rhs_args},
    /// );
    /// let c = {macro_builder}!(
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
    macro_builder = gen.macro_builder_name,
    struct_name = gen.struct_name,
    body = gen.getters().map(|getter| format!(
        "self{getter} {op} rhs{getter}", getter = getter, op = op
    )).concat(", "),
    example_lhs_args = (0..gen.nr_cols).map(|i| gen.lhs_col(i)).concat(",\n    ///     "),
    example_rhs_args = (0..gen.nr_cols).map(|i| gen.rhs_col(i)).concat(",\n    ///     "),
    example_res = (0..gen.nr_cols).map(|c|
        (0..gen.nr_rows).map(|r| format!("{lhs} {op} {rhs}",
            lhs = gen.lhs(c, r), rhs = gen.rhs(c, r), op = op
        )).concat(", "),
    ).concat(",\n    ///     "),
    shorthands = shorthands_bin_op_ref(
        trait_name, fn_name, op, &gen.struct_name, &gen.struct_name, &gen.struct_name
    ),
}}

fn template_mul_mat(gen: &MatGen) -> String { format! {"\
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
    /// let a = {macro_builder}!(
    ///     {example_lhs_args},
    /// );
    /// let b = {macro_builder}!(
    ///     {example_rhs_args},
    /// );
    /// let c = {macro_builder}!(
    ///     {example_res},
    /// );
    /// assert_eq!(a * b, c);
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
    macro_builder = gen.macro_builder_name,
    struct_name = gen.struct_name,
    body = (0..gen.nr_cols).map(|c| format!(
        "{col_tpe}::new({col_body})",
        col_tpe = gen.col_tpe,
        col_body = (0..gen.nr_rows).map(|r| format!(
            "t{col_r}.dot(rhs{col_c})",
            col_r = mat_getter(r), col_c = mat_getter(c)
        )).concat(", "),
    )).concat(",\n            "),
    example_lhs_args = (0..gen.nr_cols).map(|i| gen.lhs_col(i)).concat(",\n    ///     "),
    example_rhs_args = (0..gen.nr_cols).map(|i| gen.rhs_col(i)).concat(",\n    ///     "),
    example_res = (0..gen.nr_cols).map(|c|
        (0..gen.nr_rows).map(|r|
            (0..gen.nr_rows).map(|i| format!("{lhs}*{rhs}",
                lhs = gen.lhs(i, r), rhs = gen.rhs(c, i)
            )).concat(" + ")
        ).concat(",\n    ///     ")
    ).concat(",\n    ///\n    ///     "),
    shorthands = shorthands_bin_op_ref(
        "Mul", "mul", "*", &gen.struct_name, &gen.struct_name, &gen.struct_name
    ),
}}

fn template_mul_vec(gen: &MatGen) -> String { format! {"\
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
    /// let a = {macro_builder}!(
    ///     {example_lhs_args},
    /// );
    /// let u = {row_builder}!({example_rhs_args});
    /// let v = {row_builder}!(
    ///     {example_res},
    /// );
    /// assert_eq!(a * u, v);
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
    macro_builder = gen.macro_builder_name,
    row_builder = gen.col_builder,
    struct_name = gen.struct_name,
    col_tpe = gen.col_tpe,
    body = (0..gen.nr_rows).map(|r| format!(
        "t{col_r}.dot(rhs)", col_r = mat_getter(r),
    )).concat(", "),
    example_lhs_args = (0..gen.nr_cols).map(|i| gen.lhs_col(i)).concat(",\n    ///     "),
    example_rhs_args = gen.rhs_col(0),
    example_res = (0..gen.nr_rows).map(|r|
        (0..gen.nr_rows).map(|i| format!("{lhs}*{rhs}",
            lhs = gen.lhs(i, r), rhs = gen.rhs(0, i)
        )).concat(" + ")
    ).concat(",\n    ///     "),
    shorthands = shorthands_bin_op_ref(
        "Mul", "mul", "*", &gen.struct_name, &gen.col_tpe, &gen.col_tpe
    ),
}}

fn template_op_bin_scalar(
    gen: &MatGen, trait_name: &str, fn_name: &str, op: &str, doc_desc: &str) -> String
{
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
    /// let a = {macro_builder}!(
    ///     {example_lhs_args},
    /// );
    /// let b = {example_rhs_arg};
    /// let c = {macro_builder}!(
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
    macro_builder = gen.macro_builder_name,
    struct_name = gen.struct_name,
    tpe = gen.tpe,
    body = gen.getters().map(|getter| format!(
        "self{getter} {op} rhs", getter = getter, op = op
    )).concat(", "),
    example_lhs_args = (0..gen.nr_cols).map(|i| gen.lhs_col(i)).concat(",\n    ///     "),
    example_rhs_arg = "2.0",
    example_res = (0..gen.nr_cols).map(|c|
        (0..gen.nr_rows).map(|r| format!("{lhs} {op} {rhs}",
            lhs = gen.lhs(c, r), rhs = "2.0", op = op
        )).concat(", "),
    ).concat(",\n    ///     "),
    shorthands = shorthands_op_bin_scalar(
        trait_name, fn_name, op, &gen.struct_name, &format!("{}", gen.tpe), &gen.struct_name
    ),
}}

fn template_op_unary(
    gen: &MatGen, trait_name: &str, fn_name: &str, op: &str, doc_verb: &str) -> String
{
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
    /// let a = {op}{macro_builder}!(
    ///     {example_args},
    /// );
    /// let b = {macro_builder}!(
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
    macro_builder = gen.macro_builder_name,
    struct_name = gen.struct_name,
    body = gen.getters().map(|getter| format!(
        "{op}self{getter}", getter = getter, op = op
    )).concat(", "),
    example_args = (0..gen.nr_cols).map(|i| gen.lhs_col(i)).concat(",\n    ///     "),
    example_res = (0..gen.nr_cols).map(|c|
        (0..gen.nr_rows).map(|r| format!("{op}{arg}",
            arg = gen.lhs(c, r), op = op
        )).concat(", "),
    ).concat(",\n    ///     "),
    shorthand = shorthand_op_unary(
        trait_name, fn_name, op, &gen.struct_name, &gen.struct_name
    ),
}}
