use gen_common::*;
use mat_common::*;
use util::*;
use std::iter::*;


pub fn template_methods(
    gen: &MatGen, name: &str, rhs_tpe: &str, methods: Vec<(String, String, String)>
) -> String {
    let trait_signatures = methods.iter().map(|sig| sig.0.to_string()).collect::<Vec<_>>();
    let method_impls = methods.iter().map(|sig| sig.1.to_string()).collect::<Vec<_>>();
    let shorthands = methods.iter().map(|sig| sig.2.to_string()).collect::<Vec<_>>();
    
    format!{"\
pub trait {struct_name}{name}Ops<Rhs> {{
    {trait_signatures}
}}

impl<'a> {struct_name}{name}Ops<&'a {rhs_tpe}> for {struct_name} {{
    {method_impls}
}}

impl {struct_name}{name}Ops<{rhs_tpe}> for {struct_name} {{
    {shorthands}
}}",
    struct_name = gen.struct_name,
    name = name,
    rhs_tpe = rhs_tpe,
    trait_signatures = trait_signatures.into_iter().concat("\n\n    "),
    method_impls = method_impls.into_iter().concat("\n\n    "),
    shorthands = shorthands.into_iter().concat("\n\n    "),
}}

pub fn method_lerp(gen: &MatGen) -> (String, String, String) {
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

fn fn_lerp(gen: &MatGen) -> String { format! {"\
    /// Computes linear interpolation `self*(1 - a) + rhs*a` producing a new matrix.
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
    /// let c = a*{example_scale_left} + b*{example_scale_right};
    /// assert_eq!(a.lerp(b, {example_scale_right}), c);
    /// # }}
    /// ```
    fn lerp(&self, rhs: &{struct_name}, a: {tpe}) -> {struct_name} {{
        let b = {one} - a;
        {struct_name}::new(
            {body}
        )
    }}",
    macro_builder = gen.macro_builder_name,
    struct_name = gen.struct_name,
    tpe = gen.tpe,
    one = coerce(gen.tpe, "1"),
    body = gen.getters().map(|getter| format!(
        "self{getter}*b + rhs{getter}*a", getter = getter
    )).concat(", "),
    example_lhs_args = (0..gen.nr_cols).map(|c| gen.lhs_col(c)).concat(",\n    ///     "),
    example_rhs_args = (0..gen.nr_cols).map(|c| gen.rhs_col(c)).concat(",\n    ///     "),
    example_scale_left = format!("({} - {})", coerce(gen.tpe, "1.0"), coerce(gen.tpe, "0.25")),
    example_scale_right = coerce(gen.tpe, "0.25"),
}}
