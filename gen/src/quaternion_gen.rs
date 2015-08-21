use gen_common::*;
use util::*;
use vec_common;
use vec_common::VecGen;

static ABCD: [&'static str; 4] = ["a", "b", "c", "d"];

pub fn gen_quaternion() -> String {
    let gen = &VecGen {
        struct_name: format!("Quat"),
        tpe: Type::F64,
        dims: 4,
        macro_builder_name: format!("quat"),
        all_ordinals: &ABCD,
        doc_name: "quaternion".to_string(),
        val_name: "q".to_string(),
        quaternion_override: true,
    };
    template_file(gen)
}

fn template_file(gen: &VecGen) -> String { format! {"\
// Generated code.
{imports}

{template_struct}

{template_struct_impl}

{op_index}

{template_common_num_ops}

{op_neg}

{macro_builder}
",
    imports = vec_common::IMPORTS,
    template_struct = vec_common::template_struct(gen, doc_struct()),
    template_struct_impl = template_struct_impl(gen),
    op_index = vec_common::op_index(gen),
    template_common_num_ops = vec_common::template_common_num_ops(gen, op_mul_quat(gen)),
    op_neg = vec_common::op_neg(gen),
    macro_builder = macro_builder(gen),
}}

pub fn template_struct_impl(gen: &VecGen) -> String { format! {"\
impl {struct_name} {{
    {fn_new}
    
    {accessors_primary}
    
    {template_num_postfix}
}}",
    struct_name = gen.struct_name,
    fn_new = vec_common::fn_new(gen),
    accessors_primary = (0..gen.dims).map( |i|
        vec_common::fn_accessor_primary(gen, i)
    ).concat("\n\n    "),
    template_num_postfix = vec_common::template_common_num_postfix(gen),
}}

fn doc_struct() -> String { format! {"\
/// Quaternion with 4 floating-point components `a`, `b`, `c`, and `d`.
/// Unit quaternions represent rotation.
///
{doc_quat_rotation}
///
/// Aside from rotation and multiplication, Vexyz provides a variety of component-wise
/// quaternion operators and methods.",
    doc_quat_rotation = doc_quat_rotation(),
}}

fn doc_quat_rotation() -> String { format!{"\
/// Multiple rotations are combine using quaternion multiplication. For example, given rotations
/// `r1` and `r2`, you can obtain a combined rotation that performs
/// `r1` **then** `r2` as `r2*r1`. Note the order of arguments! Quaternion multiplication is
/// not associative, so `r2*r1 != r1*r2`.
///
/// Alternatively, you can use `rotate_q()` method: `let r3 = r1.rotate_q(r2)`. Using rotate()
/// method is more intuitive, because the resulting transformation is equivalent to applying
/// all the operations one at a time, from left to right.
///
/// When combining multiple rotations, the resulting quaternion will accumulate floating point
/// errors. You can fix that by periodically normalizing the quaterion with `q.normalize()`."
}}

fn op_mul_quat(gen: &VecGen) -> String { format! {"\
impl<'a, 'b> Mul<&'b {struct_name}> for &'a {struct_name} {{
    type Output = {struct_name};

    /// Performs quaternion multiplication producing a new quaternion.
    /// When `lhs` and `rhs` are both unit quaternions, multiplication represents rotation.
    ///
{doc_quat_rotation}
    ///
    /// # Examples
    ///
    /// ```
    /// assert(true); //XXX fix this
    /// ```
    fn mul(self, rhs: &{struct_name}) -> {struct_name} {{
        Quat::new(
            self{a}*rhs{a} - self{b}*rhs{b} - self{c}*rhs{c} - self{d}*rhs{d},
            self{a}*rhs{b} + self{b}*rhs{a} + self{c}*rhs{d} - self{d}*rhs{c},
            self{a}*rhs{c} - self{b}*rhs{d} + self{c}*rhs{a} + self{d}*rhs{b},
            self{a}*rhs{d} + self{b}*rhs{c} - self{c}*rhs{b} + self{d}*rhs{a},
        )
    }}
}}

{shorthands}",
    doc_quat_rotation = doc_quat_rotation().prefix_lines("    "),
    struct_name = gen.struct_name,
    shorthands = shorthands_bin_op_ref(
        "Mul", "mul", "*", &gen.struct_name, &gen.struct_name, &gen.struct_name
    ),
    a = quat_getter(0), b = quat_getter(1), c = quat_getter(2), d = quat_getter(3),
}}

fn macro_builder(gen: &VecGen) -> String { format! { "\
/// Builder macro for creating new {doc_description}.
///
/// # Examples
///
/// Create a new identity {doc_name}:
///
/// ```
/// #[macro_use] extern crate vexyz_math;
/// use vexyz_math::*;
///
/// # fn main() {{
/// let {val_name} = {macro_name}!();
/// assert_eq!({val_name}, {struct_name}::new(0.0, 0.0, 0.0, 1.0));
/// # }}
/// ```
/// 
/// Create a new {doc_name} with {example_n_args_worded}:
///
/// ```
/// #[macro_use] extern crate vexyz_math;
/// use vexyz_math::*;
///
/// # fn main() {{
/// let {val_name} = {macro_name}!({example_n_args});
/// assert_eq!({val_name}, {struct_name}::new({example_n_args}));
/// # }}
/// ```
#[macro_export]
macro_rules! {macro_name} {{
    () => {{{{
        {struct_name}::new(0.0, 0.0, 0.0, 1.0)
    }}}};
    ({args_n}) => {{{{
        {struct_name}::new({body_n})
    }}}};
    ({args_n},) => {{{{
        {struct_name}::new({body_n})
    }}}};
}}",
    doc_description = gen.doc_description(),
    doc_name = gen.doc_name,
    val_name = gen.val_name,
    macro_name = gen.macro_builder_name,
    struct_name = gen.struct_name,
    args_n = gen.ordinals().map(|ord| format!("${}:expr", ord)).concat(", "),
    body_n = gen.ordinals().map(|ord| format!("${} as {}", ord, gen.tpe)).concat(", "),
    example_n_args_worded = gen.ordinals().zip(0 .. gen.dims).map(|(ord, i)| format!(
        "`{ord} = {value}`", ord = ord, value = gen.rhs(i)
    )).worded(),
    example_n_args = (0 .. gen.dims).map(|i| gen.coerce(gen.rhs(i))).concat(", "),
}}
