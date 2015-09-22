use util::*;
use gen_common::*;
use vec_common;
use vec_common::VecGen;

pub fn gen_bool_vector(n: usize) -> String {
    let gen = &VecGen {
        struct_name: format!("Vec{}b", n),
        tpe: Type::Bool,
        dims: n,
        macro_builder_name: format!("bvec{}", n),
        all_ordinals: &vec_common::XYZW,
        doc_name: "vector".to_string(),
        val_name: "u".to_string(),
        quaternion_override: false,
    };
    template_file(gen)
}

fn template_file(gen: &VecGen) -> String { format! {"\
// Generated code.
{imports}

{template_struct}

{template_struct_impl}

{trait_display}

{op_index}

{template_boolean_ops}

{macro_builder}
",
    imports = vec_common::IMPORTS,
    template_struct = vec_common::template_struct(gen, vec_common::doc_vec_struct(gen)),
    template_struct_impl = vec_common::template_struct_impl(gen, template_bool_postfix(gen)),
    trait_display = vec_common::trait_display(gen),
    op_index = vec_common::op_index(gen),
    template_boolean_ops = template_boolean_ops(gen),
    macro_builder = vec_common::macro_builder(gen),
}}

fn template_bool_postfix(gen: &VecGen) -> String { format! { "\
	{fn_all}
	
	{fn_any}
	
	{fn_not}",
	fn_all = fn_all(gen),
	fn_any = fn_any(gen),
	fn_not = fn_not(gen),
}}

fn fn_all(gen: &VecGen) -> String { format! {"\
	/// Returns true if all components of the vector are true, false otherwise.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {{
    /// assert!({macro_builder}!({example_true}).all());
    /// assert!(!{macro_builder}!({example_false}).all());
    /// # }}
    /// ```
	pub fn all(&self) -> bool {{
		{body}
	}}",
	macro_builder = gen.macro_builder_name,
	body = (0..gen.dims).map(|i| format!("self{}", vec_getter(i))).concat(" && "),
	example_true = (0..gen.dims).map(|_| "true".to_string()).concat(", "),
	example_false = (0..gen.dims).map(|i|
	    if i == 1 { "false".to_string() } else { "true".to_string() }
    ).concat(", "),
}}

fn fn_any(gen: &VecGen) -> String { format! {"\
	/// Returns true if at least one of the vector components is true, false otherwise.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {{
    /// assert!({macro_builder}!({example_true}).any());
    /// assert!(!{macro_builder}!({example_false}).any());
    /// # }}
    /// ```
	pub fn any(&self) -> bool {{
		{body}
	}}",
	macro_builder = gen.macro_builder_name,
	body = (0..gen.dims).map(|i| format!("self{}", vec_getter(i))).concat(" || "),
	example_true = (0..gen.dims).map(|i|
	    if i == 1 { "true".to_string() } else { "false".to_string() }
    ).concat(", "),
	example_false = (0..gen.dims).map(|_| "false".to_string()).concat(", "),
}}

fn fn_not(gen: &VecGen) -> String { format! {"\
	/// Performs component-wise negation of the vector, returning a new vector.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {{
    /// let u = {macro_builder}!({example_args});
    /// assert_eq!(u.not(), {macro_builder}!({example_res}));
    /// # }}
    /// ```
	pub fn not(&self) -> {struct_name} {{
		{struct_name}::new({body})
	}}",
	macro_builder = gen.macro_builder_name,
	struct_name = gen.struct_name,
	body = (0..gen.dims).map(|i| format!("!self{}", vec_getter(i))).concat(", "),
	example_args = (0..gen.dims).map(|i| (i % 2 == 0).to_string()).concat(", "),
	example_res = (0..gen.dims).map(|i| (i % 2 != 0).to_string()).concat(", "),
}}

fn template_boolean_ops(gen: &VecGen) -> String { format! { "\
{op_and_vec}

{op_and_scalar}

{op_or_vec}

{op_or_scalar}

{op_xor_vec}

{op_xor_scalar}

{op_not}",
    op_and_vec = vec_common::template_op_bin_vec(gen, "BitAnd", "bitand", "&",
        "Performs logical *and* between each component of the `lhs` vector\
        \n    /// and the corresponding component of the `rhs` vector"),
    
    op_or_vec = vec_common::template_op_bin_vec(gen, "BitOr", "bitor", "|",
        "Performs logical *or* between each component of the `lhs` vector\
        \n    /// and the corresponding component of the `rhs` vector"),
    
    op_xor_vec = vec_common::template_op_bin_vec(gen, "BitXor", "bitxor", "^",
        "Performs logical *xor* between each component of the `lhs` vector\
        \n    /// and the corresponding component of the `rhs` vector"),
    
    op_and_scalar = vec_common::template_op_bin_scalar(gen, "BitAnd", "bitand", "&",
        "Performs logical *and* between each component of a vector\
        \n    /// and a scalar"),
    
    op_or_scalar = vec_common::template_op_bin_scalar(gen, "BitOr", "bitor", "|",
        "Performs logical *or* between each component of a vector\
        \n    /// and a scalar"),
    
    op_xor_scalar = vec_common::template_op_bin_scalar(gen, "BitXor", "bitxor", "^",
        "Performs logical *xor* between each component of a vector\
        \n    /// and a scalar"),
    
    op_not = vec_common::template_op_unary(gen, "Not", "not", "!", "logical negation"),
}}
