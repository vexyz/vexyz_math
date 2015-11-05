use util::*;
use gen_common::*;
use vec_common;
use vec_common::VecGen;

pub fn gen_float_vector(n: usize) -> String {
    let gen = &VecGen {
        struct_name: format!("Vec{}", n),
        tpe: Type::F32,
        dims: n,
        macro_builder_name: format!("vec{}", n),
        all_ordinals: &XYZW,
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

{template_methods}

{trait_display}

{op_index}

{op_index_mut}

{template_common_num_ops}

{op_neg}

{op_mul_mat}

{macro_builder}
",
    imports = vec!{
        vec_common::IMPORTS,
        "use std::mem;",
        &format!("use {};", gen.bool_struct_name()),
        &format!("use Mat{};", gen.dims),
    }.iter().map(|s| s.to_string()).concat("\n"),
    template_struct = vec_common::template_struct(gen, vec_common::doc_vec_struct(gen)),
    template_struct_impl = vec_common::template_struct_impl(gen, format! {"\
    {template_common_num_postfix}
    
    {template_float_postfix}",
        template_common_num_postfix = vec_common::template_common_num_postfix(gen),
        template_float_postfix = template_float_postfix(gen),
    }),
    template_methods = vec_common::template_methods(gen, {
        let mut methods = vec_common::template_methods_compare(gen);
        methods.push(vec_common::method_approx_equal(gen));
        methods.push(vec_common::method_dot(gen));
        methods.push(method_lerp(gen));
        methods
    }),
    trait_display = vec_common::trait_display(gen),
    op_index = vec_common::op_index(gen),
    op_index_mut = vec_common::op_index_mut(gen),
    template_common_num_ops = vec_common::template_common_num_ops(gen,
        vec_common::op_mul_vec(gen)
    ),
    op_neg = vec_common::op_neg(gen),
    op_mul_mat = vec_common::template_op_mul_mat(gen),
    macro_builder = vec_common::macro_builder(gen),
}}

fn fn_length(gen: &VecGen) -> String { format! {"\
    /// Computes the length of the vector.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {{
    /// let u = {macro_builder}!({example_args});
    /// assert_eq!(u.length(), {example_res});
    /// # }}
    /// ```
    pub fn length(&self) -> {tpe} {{
        self.dot(self).sqrt()
    }}",
    tpe = gen.tpe,
    macro_builder = gen.macro_builder_name,
    example_args = (0..gen.dims).map(|i| gen.lhs(i)).concat(", "),
    example_res = (0..gen.dims).map(|i|
        format!("{s}*{s}", s = gen.lhs(i))
    ).mk_string("((", " + ", &format!(") as {}).sqrt()", gen.tpe)),
}}

fn template_float_postfix(gen: &VecGen) -> String { format! {"\
    {fn_length}",
    fn_length = fn_length(gen),
}}

fn method_lerp(gen: &VecGen) -> (String, String, String) {
    (
        format!("fn lerp(&self, rhs: Rhs, a: {tpe}) -> {struct_name};",
             struct_name = gen.struct_name,
             tpe = gen.tpe),
        fn_lerp(gen),
        format!("\
    /// Shorthand for `lhs.determinant(&rhs)`.
    #[inline(always)] fn lerp(&self, rhs: {struct_name}, a: {tpe}) -> {struct_name} {{
        self.lerp(&rhs, a)
    }}",
            struct_name = gen.struct_name,
            tpe = gen.tpe,
        ),
    )
}

fn fn_lerp(gen: &VecGen) -> String { format! {"\
    /// Computes linear interpolation `self*(1 - a) + rhs*a` producing a new vector.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {{
    /// let u = {macro_builder}!({example_lhs});
    /// let v = {macro_builder}!({example_rhs});
    /// let w = u.lerp(v, {example_amount});
    /// assert_eq!(w, {macro_builder}!({example_res}));
    /// # }}
    /// ```
    fn lerp(&self, rhs: &{struct_name}, a: {tpe}) -> {struct_name} {{
        self*({one} - a) + rhs*a
    }}",
    macro_builder = gen.macro_builder_name,
    struct_name = gen.struct_name,
    tpe = gen.tpe,
    one = coerce(gen.tpe, "1"),
    example_lhs = (0..gen.dims).map(|i| gen.lhs(i)).concat(", "),
    example_rhs = (0..gen.dims).map(|i| gen.rhs(i)).concat(", "),
    example_amount = coerce(gen.tpe, "0.25"),
    example_res = (0..gen.dims).map(|i| format! {
        "{lhs}*{b} + {rhs}*{a}",
        a = coerce(gen.tpe, "0.25"),
        b = coerce(gen.tpe, "0.75"),
        lhs = coerce(gen.tpe, &gen.lhs(i)),
        rhs = coerce(gen.tpe, &gen.rhs(i)),
    }).concat(", "),
}}
