use gen_common::*;
use util::*;

pub static IMPORTS: &'static str = "\
use std::fmt::{Display, Formatter, Result};
use std::ops::*;";

pub static XYZW: [&'static str; 4] = ["x", "y", "z", "w"];
static RGBA: [&'static str; 4] = ["r", "g", "b", "a"];
static DELTAS: [&'static str; 4] = ["-1", "0", "1", "1"];

pub struct VecGen {
    pub struct_name: String,
    pub tpe: Type,
    pub dims: usize,
    pub macro_builder_name: String,
    pub all_ordinals: &'static [&'static str; 4],
    pub doc_name: String,
    pub val_name: String,
    pub quaternion_override: bool,
}

impl VecGen {
    pub fn raw_getter(&self, i: usize) -> String {
        vec_getter(i)
    }
    
    pub fn getters(&self) -> Box<Iterator<Item = String>> {
        Box::new((0 .. self.dims).map(|i| vec_getter(i)))
    }
    
    pub fn ordinals(&self) -> Box<Iterator<Item = String>> {
        Box::new(self.all_ordinals.iter().take(self.dims).map(|s| s.to_string()))
    }
    
    pub fn lhs(&self, i: usize) -> String {
        match self.tpe {
            Type::Bool => format!("{}", i % 2 == 0),
            _ => ((i + 2)*10).to_string(),
        }
    }
    pub fn rhs(&self, i: usize) -> String {
        match self.tpe {
            Type::Bool => format!("{}", i == 0 || i % 2 != 0),
            _ => (i + 2).to_string(),
        }
    }
    
    pub fn doc_description(&self) -> String {
        if self.quaternion_override {
            "quaternions".to_string()
        }
        else { format! {
            "{n}-dimensional vectors with {tpe_worded} components",
            n = self.dims,
            tpe_worded = self.tpe.worded(),
        }}
    }
    
    pub fn bool_macro_builder_name(&self) -> String {
        format!("bvec{}", self.dims)
    }
    
    pub fn bool_struct_name(&self) -> String {
        format!("Vec{}b", self.dims)
    }
    
    pub fn deltas(&self) -> Box<Iterator<Item = String>> {
        Box::new((0 .. self.dims).map(|i| DELTAS[i].to_string()))
    }
}

pub fn template_common_num_ops(gen: &VecGen, op_mul_vec: String) -> String { format! { "\
{op_add_vec}

{op_add_scalar}

{op_sub_vec}

{op_sub_scalar}

{op_mul_vec}

{op_mul_scalar}

{op_div_vec}

{op_div_scalar}",
    op_add_vec = template_op_bin_vec(gen, "Add", "add", "+", &format!(
        "Performs component-wise addition of two {doc_name}s", doc_name = gen.doc_name)),
    
    op_sub_vec = template_op_bin_vec(gen, "Sub", "sub", "-", &format!(
        "Subtracts each component of the `rhs` {doc_name} from the \
        \n    /// corresponding component of the `lhs` {doc_name}", doc_name = gen.doc_name)),
    
    op_mul_vec = op_mul_vec,
    
    op_div_vec = template_op_bin_vec(gen, "Div", "div", "/", &format!(
        "Divides each component of the `lhs` {doc_name} by the \
        \n    /// corresponding component of the `rhs` {doc_name}", doc_name = gen.doc_name)),
    
    op_add_scalar = template_op_bin_scalar(gen, "Add", "add", "+", &format!(
        "Adds a scalar to each component of a {doc_name}", doc_name = gen.doc_name)),
    
    op_sub_scalar = template_op_bin_scalar(gen, "Sub", "sub", "-", &format!(
        "Subtracts a scalar from each component of a {doc_name}", doc_name = gen.doc_name)),
    
    op_mul_scalar = template_op_bin_scalar(gen, "Mul", "mul", "*", &format!(
        "Multiplies each component of a {doc_name} by a scalar", doc_name = gen.doc_name)),
    
    op_div_scalar = template_op_bin_scalar(gen, "Div", "div", "/", &format!(
        "Divides each component of a {doc_name} by a scalar", doc_name = gen.doc_name)),
}}

pub fn template_struct(gen: &VecGen, doc_struct: String) -> String { format! {"\
{doc_struct}
#[derive(Debug, PartialEq, Copy, Clone)]
pub struct {struct_name} {{ data: [{tpe}; {dims}] }}",
    doc_struct = doc_struct,
    struct_name = gen.struct_name,
    tpe = gen.tpe,
    dims = gen.dims,
}}

pub fn doc_vec_struct(gen: &VecGen) -> String { format! {"\
/// {n}-dimensional vector with {tpe_worded} {components_worded} components.
///
/// Vectors can also represent colors with the help of
/// {color_accessors_worded} accessors.
///
/// Most operators and methods on vectors are performed in component-wise fashion. The notable
/// exception is vector-matrix multiplication.",
    n = gen.dims,
    tpe_worded = gen.tpe.worded(),
    components_worded = gen.ordinals().map(|ord| format!("`{}`", ord)).worded(),
    color_accessors_worded = (0 .. gen.dims).map(|i| format!("`{}()`", RGBA[i])).worded(),
}}

pub fn template_struct_impl(gen: &VecGen, template_postfix: String) -> String { format! {"\
impl {struct_name} {{
    {fn_new}
    
    {accessors_primary}
    
	{accessors_secondary}
	
	{template_postfix}
}}",
    struct_name = gen.struct_name,
    fn_new = fn_new(gen),
    accessors_primary = (0..gen.dims).map(|i| fn_accessor_primary(gen, i)).concat("\n\n    "),
    accessors_secondary = (0..gen.dims).map(|i|
        fn_accessor_secondary(gen, i)
    ).mk_string("\n    ", "\n\n    ", ""),
    template_postfix = template_postfix,
}}

pub fn template_common_num_postfix(gen: &VecGen) -> String { format!{"\
	{fn_sum}
	
	{fn_abs}",
    fn_sum = fn_sum(gen),
    fn_abs = fn_abs(gen),
}}
 
pub fn template_methods(gen: &VecGen, methods: Vec<(String, String, String)>) -> String {
    template_named_methods(gen, "", &gen.struct_name, methods)
}

pub fn template_named_methods(
    gen: &VecGen, name: &str, rhs_tpe: &str, methods: Vec<(String, String, String)>
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

pub fn fn_accessor_primary(gen: &VecGen, index: usize) -> String { format! {"\
    /// Component accessor, returns the {nth} component of the {doc_name}.
    #[inline(always)] pub fn {accessor}(&self) -> {tpe} {{ self{raw_getter} }}",
    accessor = gen.all_ordinals[index],
    raw_getter = gen.raw_getter(index),
    tpe = gen.tpe,
    nth = nth(index),
    doc_name = gen.doc_name
}}

pub fn fn_accessor_secondary(gen: &VecGen, index: usize) -> String { format! {"\
    /// Color-style component accessor, returns the {nth} component of the vector.
    #[inline(always)] pub fn {accessor}(&self) -> {tpe} {{ self{getter} }}",
    accessor = RGBA[index],
    getter = gen.raw_getter(index),
    tpe = gen.tpe,
    nth = nth(index)
}}

pub fn fn_new(gen: &VecGen) -> String { format! {"\
    /// Constructs a new `{struct_name}`.
    pub fn new({args}) -> Self {{
         {struct_name} {{ data: [{body}] }}
    }}",
    struct_name = gen.struct_name,
    args = gen.ordinals().map(|ord| format!(
        "{ordinal}: {tpe}", ordinal = ord, tpe = gen.tpe
    )).concat(", "),
    body = gen.ordinals().concat(", ")
}}

pub fn fn_sum(gen: &VecGen) -> String { format! {"\
    /// Returns the sum of vector components.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {{
    /// let u = {macro_builder}!({example_args});
    /// assert_eq!(u.sum(), {example_res});
    /// # }}
    /// ```
    pub fn sum(&self) -> {tpe} {{
        {body}
    }}",
    macro_builder = gen.macro_builder_name,
    tpe = gen.tpe,
    body = gen.getters().map(|getter| format!(
        "self{}",getter
    )).concat(" + "),
    example_args = (0..gen.dims).map(|i| gen.lhs(i)).concat(", "),
    example_res = (0..gen.dims).map(|i| coerce(gen.tpe, &gen.lhs(i))).concat(" + "),
}}

pub fn fn_abs(gen: &VecGen) -> String { format! {"\
    /// Performs `abs()` on each component, producing a new vector.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {{
    /// let u = {macro_builder}!({example_args});
    /// assert_eq!(u.abs(), {macro_builder}!({example_res}));
    /// # }}
    /// ```
    pub fn abs(&self) -> {struct_name} {{
        {struct_name}::new({body})
    }}",
    macro_builder = gen.macro_builder_name,
    struct_name = gen.struct_name,
    body = gen.getters().map(|getter| format!(
        "self{}.abs()",getter
    )).concat(", "),
    example_args = (0..gen.dims).map(|i| format!("{sign}{literal}",
        sign = if i % 2 != 0 { "-" } else { "" },
        literal = gen.lhs(i)
    )).concat(", "),
    example_res = (0..gen.dims).map(|i| gen.lhs(i)).concat(", "),
}}

pub fn method_approx_equal(gen: &VecGen) -> (String, String, String) {
    (
        format!("fn approx_equal(&self, rhs: Rhs, eps: {tpe}) -> bool;", tpe = gen.tpe),
        fn_approx_equal(gen),
        format!("\
    /// Shorthand for `lhs.approx_equals(&rhs, eps)`.
    #[inline(always)] fn approx_equal(&self, rhs: {struct_name}, eps: {tpe}) -> bool {{
        self.approx_equal(&rhs, eps)
    }}",
            struct_name = gen.struct_name,
            tpe = gen.tpe,
        ),
    )
}

fn fn_approx_equal(gen: &VecGen) -> String { format! {"\
    /// Tests for approximate equality within given absolute error.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {{
    /// let {val_name} = {macro_builder}!({example_lhs});
    /// assert!({val_name}.approx_equal({val_name} + {macro_builder}!({delta_eq}), {eps}));
    /// assert!(!{val_name}.approx_equal({val_name} + {macro_builder}!({delta_ne}), {eps}));
    /// # }}
    /// ```
    fn approx_equal(&self, rhs: &{struct_name}, eps: {tpe}) -> bool {{
    	let eps = {struct_name}::new({eps_body});
        (self - rhs).abs().less_than(eps).all()
    }}",
    macro_builder = gen.macro_builder_name,
    struct_name = gen.struct_name,
    tpe = gen.tpe,
    val_name = gen.val_name,
    eps = coerce(gen.tpe, "1e-8"),
    eps_body = (0..gen.dims).map(|_| coerce(gen.tpe, "eps")).concat(", "),
    delta_eq = coerce(gen.tpe, "1e-9"),
    delta_ne = coerce(gen.tpe, "1e-8"),
    example_lhs = (0..gen.dims).map(|i| gen.lhs(i)).concat(", "),
}}

pub fn method_dot(gen: &VecGen) -> (String, String, String) {
    (
        format!("fn dot(&self, rhs: Rhs) -> {tpe};", tpe = gen.tpe),
        fn_dot(gen),
        format!("\
    /// Shorthand for `lhs.dot(&rhs)`.
    #[inline(always)] fn dot(&self, rhs: {struct_name}) -> {tpe} {{
        self.dot(&rhs)
    }}",
            struct_name = gen.struct_name,
            tpe = gen.tpe,
        ),
    )
}

fn fn_dot(gen: &VecGen) -> String { format! {"\
    /// Returns dot product of two vectors.
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
    /// assert_eq!(u.dot(v), {example_res});
    /// # }}
    /// ```
    fn dot(&self, rhs: &{struct_name}) -> {tpe} {{
        (self * rhs).sum()
    }}",
    macro_builder = gen.macro_builder_name,
    struct_name = gen.struct_name,
    tpe = gen.tpe,
    example_lhs = (0..gen.dims).map(|i| gen.lhs(i)).concat(", "),
    example_rhs = (0..gen.dims).map(|i| gen.rhs(i)).concat(", "),
    example_res = (0..gen.dims).map(|i| format!(
        "{lhs} * {rhs}", lhs = coerce(gen.tpe, &gen.lhs(i)), rhs = coerce(gen.tpe, &gen.rhs(i))
    )).concat(" + "),
}}

pub fn template_methods_compare(gen: &VecGen) -> Vec<(String, String, String)> { vec! {
    template_method_compare(gen, &"less_than", &"<", &"less than"),
    template_method_compare(gen, &"less_than_equal", &"<=", &"less than or equal"),
    template_method_compare(gen, &"greater_than", &">", &"greater than"),
    template_method_compare(gen, &"greater_than_equals", &">=", &"greater than or equal"),
    template_method_compare(gen, &"equal", &"==", &"equal"),
    template_method_compare(gen, &"not_equal", &"!=", &"not equal"),
}}

fn template_method_compare(
    gen: &VecGen, method_name: &str, compare_op: &str, doc_name: &str
) -> (String, String, String) {
    (
        format!("fn {method_name}(&self, rhs: Rhs) -> {bool_struct_name};",
            method_name = method_name, bool_struct_name = gen.bool_struct_name()
        ),
        template_fn_compare(gen, method_name, compare_op, doc_name),
        format!("\
    /// Shorthand for `lhs.{method_name}(&rhs)`.
    #[inline(always)] fn {method_name}(&self, rhs: {struct_name}) -> {bool_struct_name} {{
        self.{method_name}(&rhs)
    }}",
            struct_name = gen.struct_name,
            bool_struct_name = gen.bool_struct_name(),
            method_name = method_name,
        ),
    )
}

fn template_fn_compare(
    gen: &VecGen, method_name: &str, compare_op: &str, doc_name: &str
) -> String { format! {"\
    /// Performs component-wise numerical `{doc_name}` comparision of two vectors,
    /// returning a boolean vector.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {{
    /// let u = {macro_builder}!({example_lhs});
    /// let v = u + {macro_builder}!({example_rhs});
    /// assert_eq!(u.{method_name}(v), {res_macro_builder}!({example_res}));
    /// # }}
    /// ```
    fn {method_name}(&self, rhs: &{struct_name}) -> {bool_struct_name} {{
        {bool_struct_name}::new({body})
    }}",
    macro_builder = gen.macro_builder_name,
    res_macro_builder = gen.bool_macro_builder_name(),
    struct_name = gen.struct_name,
    method_name = method_name,
    bool_struct_name = gen.bool_struct_name(),
    body = (0..gen.dims).map(|i| format!(
        "self{getter} {op} rhs{getter}", getter = vec_getter(i), op = compare_op,
    )).concat(", "),
    doc_name = doc_name,
    example_lhs = (0..gen.dims).map(|i| gen.lhs(i)).concat(", "),
    example_rhs = gen.deltas().concat(", "),
    example_res = gen.ordinals().map(|ord| format!(
        "u.{ord}() {op} v.{ord}()", ord = ord, op = compare_op
    )).concat(", "),
}}

pub fn trait_display(gen: &VecGen) -> String { format! {"\
impl Display for {struct_name} {{
    fn fmt(&self, f: &mut Formatter) -> Result {{
    	write!(f, \"{struct_name}({tokens})\", {body})
    }}
}}",
    struct_name = gen.struct_name,
    tokens = (0..gen.dims).map(|_| "{}".to_string()).concat(", "),
    body = (0..gen.dims).map(|i| format!("self{}", vec_getter(i))).concat(", "),
}}

pub fn op_index(gen: &VecGen) -> String { format! {"\
impl Index<usize> for {struct_name} {{
    type Output = {tpe};
    
    /// Index notation for acessing components of a {doc_name}.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {{
    /// let {val_name} = {macro_builder}!({example_args});
    /// assert_eq!({val_name}, {macro_builder}!({example_res}));
    /// # }}
    /// ```
    ///
    /// # Panics
    ///
    /// Will panic if the index is greater than {max_index}.
    #[inline(always)] fn index<'a>(&'a self, i: usize) -> &'a {tpe} {{
        &self.data[i]
    }}
}}",
    macro_builder = gen.macro_builder_name,
    struct_name = gen.struct_name,
    doc_name = gen.doc_name,
    val_name = gen.val_name,
    max_index = gen.dims - 1,
    tpe = gen.tpe,
    example_args = (0..gen.dims).map(|i| gen.lhs(i)).concat(", "),
    example_res = (0..gen.dims).map(|i| format!(
        "{val_name}[{index}]", val_name = gen.val_name, index = i
    )).concat(", "),
}}

pub fn op_mul_vec(gen: &VecGen) -> String {
    template_op_bin_vec(gen, "Mul", "mul", "*",
        "Performs component-wise multiplication of two vectors")
}

pub fn op_neg(gen: &VecGen) -> String {
    template_op_unary(gen, "Neg", "neg", "-", "negation")
}

pub fn template_op_bin_vec(
    gen: &VecGen, trait_name: &str, fn_name: &str, op: &str, doc_desc: &str) -> String
{
    format! {"\
impl<'a, 'b> {trait_name}<&'b {struct_name}> for &'a {struct_name} {{
    type Output = {struct_name};

    /// {doc_desc}, producing a new {doc_name}.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {{
    /// let {val_name} = {macro_builder}!({example_lhs}) {op} {macro_builder}!({example_rhs});
    /// assert_eq!({val_name}, {macro_builder}!({example_res}));
    /// # }}
    /// ```
    fn {fn_name}(self, rhs: &{struct_name}) -> {struct_name} {{
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
    doc_name = gen.doc_name,
    val_name = gen.val_name,
    body = gen.getters().map(|getter| format!(
        "self{getter} {op} rhs{getter}", getter = getter, op = op
    )).concat(", "),
    example_lhs = (0..gen.dims).map(|i| gen.lhs(i)).concat(", "),
    example_rhs = (0..gen.dims).map(|i| gen.rhs(i)).concat(", "),
    example_res = (0..gen.dims).map(|i| format!(
        "{lhs} {op} {rhs}", lhs = gen.lhs(i), op = op, rhs = gen.rhs(i)
    )).concat(", "),
    shorthands = shorthands_bin_op_ref(
        trait_name, fn_name, op, &gen.struct_name, &gen.struct_name, &gen.struct_name
    ),
}}

pub fn template_op_bin_scalar(
    gen: &VecGen, trait_name: &str, fn_name: &str, op: &str, doc_desc: &str) -> String
{
    format! {"\
impl<'a> {trait_name}<{tpe}> for &'a {struct_name} {{
    type Output = {struct_name};
    
    /// {doc_desc}, producing a new {doc_name}.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {{
    /// let {val_name} = {macro_builder}!({example_lhs}) {op} {example_rhs_arg};
    /// assert_eq!({val_name}, {macro_builder}!({example_res}));
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
    doc_name = gen.doc_name,
    val_name = gen.val_name,
    tpe = gen.tpe,
    body = gen.getters().map(|getter| format!(
        "self{getter} {op} rhs", getter = getter, op = op
    )).concat(", "),
    example_lhs = (0..gen.dims).map(|i| gen.lhs(i)).concat(", "),
    example_rhs_arg = coerce(gen.tpe, &gen.rhs(0)),
    example_res = (0..gen.dims).map(|i| format!(
        "{lhs} {op} {rhs}", lhs = gen.lhs(i), op = op, rhs = gen.rhs(0)
    )).concat(", "),
    shorthands = shorthands_op_bin_scalar(
        trait_name, fn_name, op, &gen.struct_name, &format!("{}", gen.tpe), &gen.struct_name
    ),
}}

pub fn template_op_unary(gen: &VecGen, trait_name: &str, fn_name: &str, op: &str, doc_verb: &str) -> String {
    format! {"\
impl<'a> {trait_name} for &'a {struct_name} {{
    type Output = {struct_name};
    
    /// Applies {doc_verb} to each component of a {doc_name}, producing a new {doc_name}.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {{
    /// let {val_name} = {macro_builder}!({example_args});
    /// assert_eq!({op}{val_name}, {macro_builder}!({example_res}));
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
    doc_name = gen.doc_name,
    val_name = gen.val_name,
    body = gen.getters().map(|getter| format!(
        "{op}self{getter}", getter = getter, op = op
    )).concat(", "),
    example_args = (0..gen.dims).map(|i| gen.lhs(i)).concat(", "),
    example_res = (0..gen.dims).map(|i| format!(
        "{op}{arg}", op = op, arg = gen.lhs(i)
    )).concat(", "),
    shorthand = shorthand_op_unary(
        trait_name, fn_name, op, &gen.struct_name, &gen.struct_name
    ),
}}

pub fn macro_builder(gen: &VecGen) -> String { format! { "\
/// Builder macro for creating new {doc_description}.
///
/// # Examples
///
/// Create a new {doc_name} with all components set to `{example_1_arg}`:
///
/// ```
/// #[macro_use] extern crate vexyz_math;
/// use vexyz_math::*;
///
/// # fn main() {{
/// let {val_name} = {macro_name}!({example_1_arg});
/// assert_eq!({val_name}, {struct_name}::new({example_1_body}));
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
    ($s:expr) => {{{{
        let s = $s as {tpe};
        {struct_name}::new({body_1})
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
    tpe = gen.tpe,
    body_1 = (0 .. gen.dims).map(|_| "s".to_string()).concat(", "),
    args_n = gen.ordinals().map(|ord| format!("${}:expr", ord)).concat(", "),
    example_1_arg = coerce(gen.tpe, &gen.rhs(0)),
    example_1_body = (0 .. gen.dims).map(|_| coerce(gen.tpe, &gen.rhs(0))).concat(", "),
    body_n = gen.ordinals().map(|ord| format!("${} as {}", ord, gen.tpe)).concat(", "),
    example_n_args_worded = gen.ordinals().zip(0 .. gen.dims).map(|(ord, i)| format!(
        "`{ord} = {value}`", ord = ord, value = gen.rhs(i)
    )).worded(),
    example_n_args = (0 .. gen.dims).map(|i| coerce(gen.tpe, &gen.rhs(i))).concat(", "),
}}
