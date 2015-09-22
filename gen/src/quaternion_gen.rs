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

{template_quat_methods}

{template_vec_methods}

{trait_display}

{op_index}

{template_common_num_ops}

{op_neg}

{macro_builder}
",
    imports = vec!{
        vec_common::IMPORTS,
        "use Vec3;",
        "use Vec3Ops;",
        "use Vec4;",
        "use Vec4Ops;",
    }.iter().map(|s| s.to_string()).concat("\n"),
    template_struct = vec_common::template_struct(gen, doc_struct()),
    template_struct_impl = template_struct_impl(gen),
    template_quat_methods = vec_common::template_named_methods(
        gen, &gen.struct_name, &gen.struct_name, vec!(
            method_approx_equal(gen),
            method_rotate(gen),
        )
    ),
    template_vec_methods = vec_common::template_named_methods(
        gen, "Vec3", "Vec3", vec!(
            method_rotate_vec(gen, "Vec3", "vec3"),
        )
    ),
    trait_display = vec_common::trait_display(gen),
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
    
    {template_rotate_xyz}
    
    {fn_as_vec4}
    
    {fn_norm}
    
    {fn_normalize}
}}",
    struct_name = gen.struct_name,
    fn_new = vec_common::fn_new(gen),
    accessors_primary = (0..gen.dims).map( |i|
        vec_common::fn_accessor_primary(gen, i)
    ).concat("\n\n    "),
    template_num_postfix = vec_common::template_common_num_postfix(gen),
    template_rotate_xyz = template_rotate_xyz(gen),
    fn_as_vec4 = fn_as_vec4(gen, "Vec4", "vec4"),
    fn_norm = fn_norm(gen),
    fn_normalize = fn_normalize(gen),
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
    example_n_args = (0 .. gen.dims).map(|i| coerce(gen.tpe, &gen.rhs(i))).concat(", "),
}}

fn method_rotate(gen: &VecGen) -> (String, String, String) {
    (
        format!("fn rotate(&self, rhs: Rhs) -> {struct_name};", struct_name = gen.struct_name),
        fn_rotate(gen),
        format!("\
    /// Shorthand for `lhs.rotate(&rhs)`.
    #[inline(always)] fn rotate(&self, rhs: {struct_name}) -> {struct_name} {{
        self.rotate(&rhs)
    }}",
            struct_name = gen.struct_name,
        ),
    )
}

fn fn_rotate(gen: &VecGen) -> String { format! {"\
	/// Returns combined rotation of the `lhs` quaternion followed by `rhs` quaternion.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {{
    /// //XXX add test
    /// # }}
    /// ```
	fn rotate(&self, rhs: &{struct_name}) -> {struct_name} {{
		rhs*self
	}}",
	struct_name = gen.struct_name,
}}

fn method_rotate_vec(gen: &VecGen, vec3_struct_name: &str, vec3_macro_builder: &str)
-> (String, String, String) {
    (
        format!("fn rotate_vec(&self, rhs: Rhs) -> {vec3_struct_name};",
            vec3_struct_name = vec3_struct_name
        ),
        fn_rotate_vec(gen, vec3_struct_name, vec3_macro_builder),
        format!("\
    /// Shorthand for `lhs.rotate_vec(&rhs)`.
    #[inline(always)] fn rotate_vec(&self, rhs: {vec3_struct_name}) -> {vec3_struct_name} {{
        self.rotate_vec(&rhs)
    }}",
            vec3_struct_name = vec3_struct_name),
    )
}

fn fn_rotate_vec(_gen: &VecGen, vec3_struct_name: &str, vec3_macro_builder: &str) -> String {
    format! {"\
	/// Applies rotation to the vector returning a new vector.
	/// XXX Add 'rotations are oder dependent' in all docs.
	/// XXX Add note about normalization in all docs.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {{
    /// let q = quat!().rotate_z(90.ro_radians());
    /// let u = q.rotate_vec({vec3_macro_builder}!(1, 0, 1));
    /// assert!(u.approx_equal({vec3_macro_builder}!(0, 1, 1), 1e-8));
    /// # }}
    /// ```
	fn rotate_vec(&self, rhs: &{vec3_struct_name}) -> {vec3_struct_name} {{
		let t1 = self{a}*self{b};
        let t2 = self{a}*self{c};
        let t3 = self{a}*self{d};
        let t4 = -self{b}*self{b};
        let t5 = self{b}*self{c};
        let t6 = self{b}*self{d};
        let t7 = -self{c}*self{c};
        let t8 = self{c}*self{d};
        let t9 = -self{d}*self{d};
    
        {vec3_struct_name}::new(
              {vec3_struct_name}::new(t7 + t9, t5 - t3, t2 + t6).dot(rhs),
              {vec3_struct_name}::new(t3 + t5, t4 + t9, t8 - t1).dot(rhs),
              {vec3_struct_name}::new(t6 - t2, t1 + t8, t4 + t7).dot(rhs),
        )*2.0 + rhs
	}}",
    vec3_struct_name = vec3_struct_name,
    vec3_macro_builder = vec3_macro_builder,
    a = quat_getter(0), b = quat_getter(1), c = quat_getter(2), d = quat_getter(3),
}}

fn template_rotate_xyz(gen: &VecGen) -> String { format! {"\
	/// Returns combined rotation of the `lhs` quaternion followed by rotation around `x` axis.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {{
    /// //XXX add test
    /// # }}
    /// ```
	pub fn rotate_x(&self, rads: {tpe}) -> {struct_name} {{
		let half_angle = rads*0.5;
        let qa = half_angle.cos();
        let qb = half_angle.sin();
    
    	self*qa + Quat::new(-qb*self{b}, qb*self{a}, -qb*self{d}, qb*self{c})
	}}
	
	/// Returns combined rotation of the `lhs` quaternion followed by rotation around `y` axis.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {{
    /// //XXX add test
    /// # }}
    /// ```
	pub fn rotate_y(&self, rads: {tpe}) -> {struct_name} {{
		let half_angle = rads*0.5;
        let qa = half_angle.cos();
        let qc = half_angle.sin();
    
        self*qa + Quat::new(-qc*self{c}, qc*self{d}, qc*self{a}, -qc*self{b})
	}}
	
	/// Returns combined rotation of the `lhs` quaternion followed by rotation around `z` axis.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {{
    /// //XXX add test
    /// # }}
    /// ```
	pub fn rotate_z(&self, rads: {tpe}) -> {struct_name} {{
		let half_angle = rads*0.5;
        let qa = half_angle.cos();
        let qd = half_angle.sin();
    
        self*qa + Quat::new(-qd*self{d}, -qd*self{c}, qd*self{b}, qd*self{a})
	}}",
	struct_name = gen.struct_name,
	tpe = gen.tpe,
	a = quat_getter(0), b = quat_getter(1), c = quat_getter(2), d = quat_getter(3),
}}

fn method_approx_equal(gen: &VecGen) -> (String, String, String) {
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
    /// let q = {macro_builder}!({example_lhs});
    /// assert!(q.approx_equal(q + {macro_builder}!({delta_eq}), {eps}));
    /// assert!(!q.approx_equal(q + {macro_builder}!({delta_ne}), {eps}));
    /// # }}
    /// ```
    fn approx_equal(&self, rhs: &{struct_name}, eps: {tpe}) -> bool {{
    	self.as_vec4().approx_equal(rhs.as_vec4(), eps)
    }}",
    macro_builder = gen.macro_builder_name,
    struct_name = gen.struct_name,
    tpe = gen.tpe,
    eps = coerce(gen.tpe, "1e-8"),
    delta_eq = (0..gen.dims).map(|_| coerce(gen.tpe, "1e-9")).concat(", "),
    delta_ne = (0..gen.dims).map(|_| coerce(gen.tpe, "1e-8")).concat(", "),
    example_lhs = (0..gen.dims).map(|i| gen.lhs(i)).concat(", "),
}}

fn fn_as_vec4(gen: &VecGen, vec4_struct_name: &str, vec4_macro_builder: &str) -> String {
    format! { "\
	/// Extracts quaternion components into a vector: `Quat(a, b, c, d) -> Vec4(b, c, d, a)`.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {{
    /// let q = {macro_builder}!({example_args});
    /// assert_eq!(q.as_vec4(), {vec4_macro_builder}!({example_res}));
    /// # }}
    /// ```
	pub fn as_vec4(&self) -> {vec4_struct_name} {{
		{vec4_struct_name}::new({body})
	}}",
	macro_builder = gen.macro_builder_name,
	vec4_struct_name = vec4_struct_name,
	vec4_macro_builder = vec4_macro_builder,
	body = format!("self{}, self{}, self{}, self{}",
	    quat_getter(1), quat_getter(2), quat_getter(3), quat_getter(0)
    ),
	example_args = [0, 1, 2, 3].iter().map(|i| gen.rhs(*i as usize)).concat(", "),
	example_res = [1, 2, 3, 0].iter().map(|i| gen.rhs(*i as usize)).concat(", "),
}}

fn fn_norm(gen: &VecGen) -> String { format! {"\
	/// Computes the norm of the quaternion (similar to vector length).
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
		self.as_vec4().length()
	}}",
	tpe = gen.tpe,
	macro_builder = gen.macro_builder_name,
	example_args = (0..gen.dims).map(|i| gen.lhs(i)).concat(", "),
	example_res = (0..gen.dims).map(|i|
        format!("{s}*{s}", s = gen.lhs(i))
    ).mk_string("((", " + ", ") as f64).sqrt()"),
}}

fn fn_normalize(gen: &VecGen) -> String {
    format! { "\
	/// Normalizes the quaternion.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {{
    /// let q = {macro_builder}!({example_args});
    /// assert!(q.normalize().norm().approx_equal({one}, {eps}));
    /// # }}
    /// ```
	pub fn normalize(&self) -> {struct_name} {{
		{struct_name}::new({body})
	}}",
	macro_builder = gen.macro_builder_name,
	struct_name = gen.struct_name,
	body = format!("self{}, self{}, self{}, self{}",
	    quat_getter(1), quat_getter(2), quat_getter(3), quat_getter(0)
    ),
	example_args = (0..gen.dims).map(|i| gen.lhs(i)).concat(", "),
	one = coerce(gen.tpe, "1.0"),
	eps = coerce(gen.tpe, "1e-8"),
}}

/* XXX implement these
def conjugate(q: inQuat4d) :Quat4d = new Quat4d(q.a, -q.b, -q.c, -q.d)
def inverse(q: inQuat4d) :Quat4d = conjugate(q)/normSquare(q)
*/
