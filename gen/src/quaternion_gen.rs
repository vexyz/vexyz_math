use gen_common::*;
use util::*;
use vec_common;
use vec_common::VecGen;

struct Namespace {
    vec3_struct_name: String,
    vec3_macro_builder: String,
    vec4_struct_name: String,
    vec4_macro_builder: String,
}

pub fn gen_quaternion() -> String {
    let gen = &VecGen {
        struct_name: "Quat".to_string(),
        tpe: Type::F32,
        dims: 4,
        macro_builder_name: "quat".to_string(),
        all_ordinals: &ABCD,
        doc_name: "quaternion".to_string(),
        val_name: "q".to_string(),
        quaternion_override: true,
    };
    let namespace = Namespace {
        vec3_struct_name: "Vec3".to_string(),
        vec3_macro_builder: "vec3".to_string(),
        vec4_struct_name: "Vec4".to_string(),
        vec4_macro_builder: "vec4".to_string(),
    };
    template_file(gen, &namespace)
}

fn template_file(gen: &VecGen, namespace: &Namespace) -> String { format! {"\
// Generated code.
{imports}

{template_struct}

{template_struct_impl}

{template_quat_methods}

{template_vec_methods}

{trait_display}

{op_index}

{op_index_mut}

{template_common_num_ops}

{op_neg}

{macro_builder}
",
    imports = vec!{
        vec_common::IMPORTS,
        "use std::mem;",
        &format!("use {};", namespace.vec3_struct_name),
        &format!("use {}Ops;", namespace.vec3_struct_name),
        &format!("use {};", namespace.vec4_struct_name),
        &format!("use {}Ops;", namespace.vec4_struct_name),
    }.iter().map(|s| s.to_string()).concat("\n"),
    template_struct = vec_common::template_struct(gen, doc_struct()),
    template_struct_impl = template_struct_impl(gen, namespace),
    template_quat_methods = vec_common::template_named_methods(
        gen, &gen.struct_name, &gen.struct_name, vec!(
            method_approx_equal(gen),
            method_rotate(gen, namespace),
        )
    ),
    template_vec_methods = vec_common::template_named_methods(
        gen, &namespace.vec3_struct_name, &namespace.vec3_struct_name, vec!(
            method_rotate_vec(gen, namespace),
        )
    ),
    trait_display = vec_common::trait_display(gen),
    op_index = vec_common::op_index(gen),
    op_index_mut = vec_common::op_index_mut(gen),
    template_common_num_ops = vec_common::template_common_num_ops(gen, op_mul_quat(gen)),
    op_neg = vec_common::op_neg(gen),
    macro_builder = macro_builder(gen),
}}

fn template_struct_impl(gen: &VecGen, namespace: &Namespace) -> String { format! {"\
impl {struct_name} {{
    {fn_new}
    
    {accessors_primary}
    
    {template_num_postfix}
    
    {template_rotate_xyz}
    
    {fn_as_vec4}
    
    {fn_norm}
    
    {fn_normalize}
    
    {fn_conjugate}
    
    {fn_inverse}
}}",
    struct_name = gen.struct_name,
    fn_new = vec_common::fn_new(gen),
    accessors_primary = (0..gen.dims).map( |i|
        vec_common::fn_accessor_primary(gen, i)
    ).concat("\n\n    "),
    template_num_postfix = vec_common::template_common_num_postfix(gen),
    template_rotate_xyz = template_rotate_xyz(gen, namespace),
    fn_as_vec4 = fn_as_vec4(gen, namespace),
    fn_norm = fn_norm(gen),
    fn_normalize = fn_normalize(gen),
    fn_conjugate = fn_conjugate(gen),
    fn_inverse = fn_inverse(gen, namespace),
}}

fn doc_struct() -> String { format! {"\
/// Quaternion with 4 floating-point components `a`, `b`, `c`, and `d`.
///
{doc_quat_rotation}
///
/// In addition to rotation and multiplication, Vexyz provides a variety of component-wise
/// quaternion operators and methods.",
    doc_quat_rotation = doc_quat_rotation(),
}}

fn doc_quat_rotation() -> String { format!{"\
/// Unit quaternions represent rotation. Multiple rotations are combined using `rotate()`
/// method. For example, given rotations `r1` and `r2`, you can obtain a combined rotation that
/// performs `r1` **then** `r2` as `let r3 = r1.rotate(r2)`. Note the order of arguments!
/// Quaternion rotation is not associative, so `r1.rotate(r2) != r2.rotate(r1)`.
///
/// `r1.rotate(r2)` simply calls `r2*r1`. However using `rotate()` method is more intuitive,
/// because the result is equivalent to applying all the operations from left to right.
///
/// When combining multiple rotations, the resulting quaternion will accumulate floating point
/// errors. You can remedy that by periodically normalizing the quaterion with `q.normalize()`."
}}

fn op_mul_quat(gen: &VecGen) -> String { format! {"\
impl<'a, 'b> Mul<&'b {struct_name}> for &'a {struct_name} {{
    type Output = {struct_name};

    /// Performs quaternion multiplication producing a new quaternion.
    ///
{doc_quat_rotation}
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {{
    /// let q0 = quat!().rotate_x(90_{tpe}.to_rad());
    /// let q1 = quat!().rotate_y(90_{tpe}.to_rad());
    /// assert_eq!(q1*q0, q0.rotate(q1));
    /// # }}
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
    tpe = gen.tpe,
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
/// assert_eq!({val_name}, {struct_name}::new(1.0, 0.0, 0.0, 0.0));
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
        {struct_name}::new(1.0, 0.0, 0.0, 0.0)
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

fn method_rotate(gen: &VecGen, namespace: &Namespace) -> (String, String, String) {
    (
        format!("fn rotate(&self, rhs: Rhs) -> {struct_name};", struct_name = gen.struct_name),
        fn_rotate(gen, namespace),
        format!("\
    /// Shorthand for `lhs.rotate(&rhs)`.
    #[inline(always)] fn rotate(&self, rhs: {struct_name}) -> {struct_name} {{
        self.rotate(&rhs)
    }}",
            struct_name = gen.struct_name,
        ),
    )
}

fn fn_rotate(gen: &VecGen, namespace: &Namespace) -> String { format! {"\
    /// Returns combined rotation of the `lhs` quaternion followed by `rhs` quaternion.
    ///
{doc_quat_rotation}
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {{
    /// let q0 = quat!().rotate_x(90_{tpe}.to_rad());
    /// let q1 = quat!().rotate_y(90_{tpe}.to_rad());
    /// let q = q0.rotate(q1);
    /// let u = q.rotate_vec({vec3_macro_builder}!(1, 1, 0));
    /// assert!(u.approx_equal({vec3_macro_builder}!(1, 0, -1), {eps}));
    /// # }}
    /// ```
    fn rotate(&self, rhs: &{struct_name}) -> {struct_name} {{
        rhs*self
    }}",
    struct_name = gen.struct_name,
    tpe = gen.tpe,
    eps = gen.eps_low(),
    vec3_macro_builder = namespace.vec3_macro_builder,
    doc_quat_rotation = doc_quat_rotation().prefix_lines("    "),
}}

fn method_rotate_vec(gen: &VecGen, namespace: &Namespace)
-> (String, String, String) {
    (
        format!("fn rotate_vec(&self, rhs: Rhs) -> {vec3_struct_name};",
            vec3_struct_name = namespace.vec3_struct_name
        ),
        fn_rotate_vec(gen, namespace),
        format!("\
    /// Shorthand for `lhs.rotate_vec(&rhs)`.
    #[inline(always)] fn rotate_vec(&self, rhs: {vec3_struct_name}) -> {vec3_struct_name} {{
        self.rotate_vec(&rhs)
    }}",
            vec3_struct_name = namespace.vec3_struct_name),
    )
}

fn fn_rotate_vec(gen: &VecGen, namespace: &Namespace) -> String {
    format! {"\
    /// Applies rotation to the vector returning a new vector. Quaternion must be normalized
    /// to produce accurate results.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {{
    /// let q = quat!().rotate_y(90_{tpe}.to_rad());
    /// let u = q.rotate_vec({vec3_macro_builder}!(1, 1, 0));
    /// assert!(u.approx_equal({vec3_macro_builder}!(0, 1, -1), {eps}));
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
    tpe = gen.tpe,
    eps = gen.eps_low(),
    vec3_struct_name = namespace.vec3_struct_name,
    vec3_macro_builder = namespace.vec3_macro_builder,
    a = quat_getter(0), b = quat_getter(1), c = quat_getter(2), d = quat_getter(3),
}}

fn template_rotate_xyz(gen: &VecGen, namespace: &Namespace) -> String { format! {"\
    /// Returns combined rotation of the `lhs` quaternion followed by rotation around `x` axis.
    ///
    {doc_rotation}
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {{
    /// let q = quat!().rotate_x(90_{tpe}.to_rad());
    /// let u = q.rotate_vec({vec3_macro_builder}!(1, 1, 0));
    /// assert!(u.approx_equal({vec3_macro_builder}!(1, 0, 1), {eps}));
    /// # }}
    /// ```
    pub fn rotate_x(&self, rads: {tpe}) -> {struct_name} {{
        let half_angle = rads*0.5;
        let qa = half_angle.cos();
        let qb = half_angle.sin();
    
        self*qa + Quat::new(-self{b}, self{a}, -self{d}, self{c})*qb
    }}
    
    /// Returns combined rotation of the `lhs` quaternion followed by rotation around `y` axis.
    ///
    {doc_rotation}
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {{
    /// let q = quat!().rotate_y(90_{tpe}.to_rad());
    /// let u = q.rotate_vec({vec3_macro_builder}!(1, 1, 0));
    /// assert!(u.approx_equal({vec3_macro_builder}!(0, 1, -1), {eps}));
    /// # }}
    /// ```
    pub fn rotate_y(&self, rads: {tpe}) -> {struct_name} {{
        let half_angle = rads*0.5;
        let qa = half_angle.cos();
        let qc = half_angle.sin();
    
        self*qa + Quat::new(-self{c}, self{d}, self{a}, -self{b})*qc
    }}
    
    /// Returns combined rotation of the `lhs` quaternion followed by rotation around `z` axis.
    ///
    {doc_rotation}
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {{
    /// let q = quat!().rotate_z(90_{tpe}.to_rad());
    /// let u = q.rotate_vec({vec3_macro_builder}!(0, 1, 1));
    /// assert!(u.approx_equal({vec3_macro_builder}!(-1, 0, 1), {eps}));
    /// # }}
    /// ```
    pub fn rotate_z(&self, rads: {tpe}) -> {struct_name} {{
        let half_angle = rads*0.5;
        let qa = half_angle.cos();
        let qd = half_angle.sin();
    
        self*qa + Quat::new(-self{d}, -self{c}, self{b}, self{a})*qd
    }}",
    struct_name = gen.struct_name,
    tpe = gen.tpe,
    eps = gen.eps_low(),
    vec3_macro_builder = namespace.vec3_macro_builder,
    a = quat_getter(0), b = quat_getter(1), c = quat_getter(2), d = quat_getter(3),
    doc_rotation = "\
    /// Rotation operations are ordere dependent, so
    /// `q.rotate_x(a).rotate_y(b) != q.rotate_y(b).rotate_x(a)`
    ///
    /// Keep in mind, that only unit quaternions represent rotation. When combining multiple
    /// rotations, the resulting quaternion will accumulate floating point errors. You can remedy
    /// that by periodically normalizing the quaterion with `q.normalize()`.",
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
    eps = gen.eps_low(),
    delta_eq = (0..gen.dims).map(|_| gen.eps_high()).concat(", "),
    delta_ne = (0..gen.dims).map(|_| gen.eps_low()).concat(", "),
    example_lhs = (0..gen.dims).map(|i| gen.lhs(i)).concat(", "),
}}

fn fn_as_vec4(gen: &VecGen, namespace: &Namespace) -> String {
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
    /// assert_eq!(*q.as_vec4(), {vec4_macro_builder}!({example_args}));
    /// # }}
    /// ```
    pub fn as_vec4(&self) -> &{vec4_struct_name} {{
        unsafe {{
            mem::transmute::<&{struct_name}, &{vec4_struct_name}>(&self)
        }}
    }}",
    macro_builder = gen.macro_builder_name,
    struct_name = gen.struct_name,
    vec4_struct_name = namespace.vec4_struct_name,
    vec4_macro_builder = namespace.vec4_macro_builder,
    example_args = (0..4).map(|i| gen.rhs(i as usize)).concat(", "),
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
    /// assert_eq!(u.norm(), {example_res});
    /// # }}
    /// ```
    pub fn norm(&self) -> {tpe} {{
        self.as_vec4().length()
    }}",
    tpe = gen.tpe,
    macro_builder = gen.macro_builder_name,
    example_args = (0..gen.dims).map(|i| gen.lhs(i)).concat(", "),
    example_res = (0..gen.dims).map(|i|
        format!("{s}*{s}", s = gen.lhs(i))
    ).mk_string("((", " + ", &format!(") as {}).sqrt()", gen.tpe)),
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
        self/self.norm()
    }}",
    macro_builder = gen.macro_builder_name,
    struct_name = gen.struct_name,
    example_args = (0..gen.dims).map(|i| gen.lhs(i)).concat(", "),
    one = coerce(gen.tpe, "1.0"),
    eps = gen.eps_low(),
}}

fn fn_conjugate(gen: &VecGen) -> String {
    format! { "\
    /// Returns quaternion conjugate. When applied to a unit quaternion, produces the inverse.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {{
    /// let q = {macro_builder}!({example_args});
    /// assert_eq!(q.conjugate(), {macro_builder}!({example_res}));
    /// # }}
    /// ```
    pub fn conjugate(&self) -> {struct_name} {{
        {struct_name}::new(self{a}, -self{b}, -self{c}, -self{d})
    }}",
    macro_builder = gen.macro_builder_name,
    struct_name = gen.struct_name,
    example_args = (0..gen.dims).map(|i| gen.lhs(i)).concat(", "),
    example_res = (0..gen.dims).map(|i| format!("{}{}",
        if i == 0 { "" } else { "-" }, gen.lhs(i)
    )).concat(", "),
    a = quat_getter(0), b = quat_getter(1), c = quat_getter(2), d = quat_getter(3),
}}

fn fn_inverse(gen: &VecGen, namespace: &Namespace) -> String {
    format! { "\
    /// Inverts the quaternion. When dealing with unit quaternions, use `conjugate` instead.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {{
    /// let q = quat!().rotate_x(90_{tpe}.to_rad());
    /// let u = q.rotate_vec({vec3_macro_builder}!(1, 1, 0));
    /// let v = q.inverse().rotate_vec(u);
    /// assert!(v.approx_equal({vec3_macro_builder}!(1, 1, 0), {eps}));
    /// # }}
    /// ```
    pub fn inverse(&self) -> {struct_name} {{
        let u = self.as_vec4();
        let norm_square = u.dot(u);
        self.conjugate()/norm_square
    }}",
    struct_name = gen.struct_name,
    tpe = gen.tpe,
    vec3_macro_builder = namespace.vec3_macro_builder,
    eps = gen.eps_low(),
}}
