use common::*;
use util::*;

pub static IMPORTS: &'static str = "use std::ops::*;";
pub static XYZW: [&'static str; 4] = ["x", "y", "z", "w"];
pub static RGBA: [&'static str; 4] = ["r", "g", "b", "a"];

pub struct VecGen {
    pub struct_name: String,
    pub tpe: Type,
    pub dims: usize,
    pub builder_macro_name: String,
    pub all_ordinals: &'static [&'static str; 4],
    pub doc_name: String,
    pub val_name: String,
    pub quaternion_override: bool,
}

impl VecGen {
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
    pub fn coerce(&self, arg: String) -> String {
        match self.tpe {
            Type::Bool => arg,
            Type::I32 => arg,
            Type::F64 => format!("{}.0", arg),
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
}

// XXX mutable index accessor

// TODO u.extend(1).yzx();
//TODO Do not implement Rem (%) operator, modulo() is what you want instead!
pub fn template_common_num_ops(gen: &VecGen, op_mul_vec: String) -> String { format! { "\
{op_add_vec}

{op_add_scalar}

{op_sub_vec}

{op_sub_scalar}

{op_mul_vec}

{op_mul_scalar}

{op_div_vec}

{op_div_scalar}

{op_neg}",
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
    
    op_neg = template_op_unary(gen, "Neg", "neg", "-", "negation"),
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
    macro_name = gen.builder_macro_name,
    struct_name = gen.struct_name,
    tpe = gen.tpe,
    body_1 = (0 .. gen.dims).map(|_| "s".to_string()).concat(", "),
    args_n = gen.ordinals().map(|ord| format!("${}:expr", ord)).concat(", "),
    example_1_arg = gen.coerce(gen.rhs(0)),
    example_1_body = (0 .. gen.dims).map(|_| gen.coerce(gen.rhs(0))).concat(", "),
    body_n = gen.ordinals().map(|ord| format!("${} as {}", ord, gen.tpe)).concat(", "),
    example_n_args_worded = gen.ordinals().zip(0 .. gen.dims).map(|(ord, i)| format!(
        "`{ord} = {value}`", ord = ord, value = gen.rhs(i)
    )).worded(),
    example_n_args = (0 .. gen.dims).map(|i| gen.coerce(gen.rhs(i))).concat(", "),
}}

pub fn struct_def(gen: &VecGen, doc_struct: String) -> String { format! {"\
{doc_struct}
#[derive(Debug, PartialEq, Copy, Clone)]
pub struct {struct_name} {{ data: [{tpe}; {dims}] }}",
    doc_struct = doc_struct,
    struct_name = gen.struct_name,
    tpe = gen.tpe,
    dims = gen.dims,
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

pub fn template_common_num_methods(gen: &VecGen) -> String { format!{"\
pub trait {struct_name}Ops<Rhs> {{
    fn dot(&self, rhs: Rhs) -> {tpe};
}}

impl<'a> {struct_name}Ops<&'a {struct_name}> for {struct_name} {{
    {fn_dot}
}}

impl {struct_name}Ops<{struct_name}> for {struct_name} {{
	/// Shorthand for `lhs.dot(&rhs)`.
    fn dot(&self, rhs: {struct_name}) -> {tpe} {{
        self.dot(&rhs)
    }}
}}",
    struct_name = gen.struct_name,
    tpe = gen.tpe,
    fn_dot = fn_dot(gen),
}}

//XXX move this into template_common_num_methods
pub fn template_common_num_postfix(gen: &VecGen) -> String { format!{"\
    {fn_sum}",
    fn_sum = fn_sum(gen),
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
    /// let u = {builder_macro}!({example_args});
    /// assert_eq!(u.sum(), {example_res});
    /// # }}
    /// ```
    pub fn sum(&self) -> {tpe} {{
        {body}
    }}",
    builder_macro = gen.builder_macro_name,
    tpe = gen.tpe,
    body = gen.getters().map(|getter| format!(
        "self{}",getter
    )).concat(" + "),
    example_args = (0..gen.dims).map(|i| gen.lhs(i)).concat(", "),
    example_res = (0..gen.dims).map(|i| gen.coerce(gen.lhs(i))).concat(" + "),
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

pub fn fn_accessor_primary(gen: &VecGen, index: usize) -> String { format! {"\
    /// Component accessor, returns the {nth} component of the {doc_name}.
    #[inline(always)] pub fn {accessor}(&self) -> {tpe} {{ self{getter} }}",
    accessor = gen.all_ordinals[index],
    getter = vec_getter(index),
    tpe = gen.tpe,
    nth = nth(index),
    doc_name = gen.doc_name
}}

pub fn fn_accessor_secondary(gen: &VecGen, index: usize) -> String { format! {"\
    /// Color-style component accessor, returns the {nth} component of the vector.
    #[inline(always)] pub fn {accessor}(&self) -> {tpe} {{ self{getter} }}",
    accessor = RGBA[index],
    getter = vec_getter(index),
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

pub fn fn_dot(gen: &VecGen) -> String { format! {"\
    /// Returns dot product of two vectors.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {{
    /// let u = {builder_macro}!({example_lhs}).dot({builder_macro}!({example_rhs}));
    /// assert_eq!(u, {example_res});
    /// # }}
    /// ```
    fn dot(&self, rhs: &{struct_name}) -> {tpe} {{
        (self * rhs).sum()
    }}",
    builder_macro = gen.builder_macro_name,
    struct_name = gen.struct_name,
    tpe = gen.tpe,
    example_lhs = (0..gen.dims).map(|i| gen.lhs(i)).concat(", "),
    example_rhs = (0..gen.dims).map(|i| gen.rhs(i)).concat(", "),
    example_res = (0..gen.dims).map(|i| format!(
        "{lhs} * {rhs}", lhs = gen.coerce(gen.lhs(i)), rhs = gen.coerce(gen.rhs(i))
    )).concat(" + "),
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
    /// let {val_name} = {builder_macro}!({example_args});
    /// assert_eq!({val_name}, {builder_macro}!({example_res}));
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
    builder_macro = gen.builder_macro_name,
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

pub fn template_op_bin_vec(
    gen: &VecGen, trait_name: &str, fn_name: &str, op: &str, doc_desc: &str) -> String
{
    format! {"\
impl<'a, 'b> {trait_name}<&'b {struct_name}> for &'a {struct_name} {{
    type Output = {struct_name};

    /// {doc_desc} producing a new {doc_name}.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {{
    /// let {val_name} = {builder_macro}!({example_lhs}) {op} {builder_macro}!({example_rhs});
    /// assert_eq!({val_name}, {builder_macro}!({example_res}));
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
    builder_macro = gen.builder_macro_name,
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
    
    /// {doc_desc} producing a new {doc_name}.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {{
    /// let {val_name} = {builder_macro}!({example_lhs}) {op} {example_rhs_arg};
    /// assert_eq!({val_name}, {builder_macro}!({example_res}));
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
    builder_macro = gen.builder_macro_name,
    struct_name = gen.struct_name,
    doc_name = gen.doc_name,
    val_name = gen.val_name,
    tpe = gen.tpe,
    body = gen.getters().map(|getter| format!(
        "self{getter} {op} rhs", getter = getter, op = op
    )).concat(", "),
    example_lhs = (0..gen.dims).map(|i| gen.lhs(i)).concat(", "),
    example_rhs_arg = gen.coerce(gen.rhs(0)),
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
    
    /// Applies {doc_verb} to each component of a {doc_name} producing a new {doc_name}.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {{
    /// let {val_name} = {builder_macro}!({example_args});
    /// assert_eq!({op}{val_name}, {builder_macro}!({example_res}));
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
    builder_macro = gen.builder_macro_name,
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
