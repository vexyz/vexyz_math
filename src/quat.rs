// Generated code.
use std::fmt::{Display, Formatter, Result};
use std::ops::*;
use std::mem;
use Vec3;
use Vec3Ops;
use Vec4;
use Vec4Ops;

/// Quaternion with 4 floating-point components `a`, `b`, `c`, and `d`.
///
/// Unit quaternions represent rotation. Multiple rotations are combined using `rotate()`
/// method. For example, given rotations `r1` and `r2`, you can obtain a combined rotation that
/// performs `r1` **then** `r2` as `let r3 = r1.rotate(r2)`. Note the order of arguments!
/// Quaternion rotation is not associative, so `r1.rotate(r2) != r2.rotate(r1)`.
///
/// `r1.rotate(r2)` simply calls `r2*r1`. However using `rotate()` method is more intuitive,
/// because the result is equivalent to applying all the operations from left to right.
///
/// When combining multiple rotations, the resulting quaternion will accumulate floating point
/// errors. You can remedy that by periodically normalizing the quaterion with `q.normalize()`.
///
/// In addition to rotation and multiplication, Vexyz provides a variety of component-wise
/// quaternion operators and methods.
#[derive(Debug, PartialEq, Copy, Clone)]
pub struct Quat { data: [f32; 4] }

impl Quat {
    /// Constructs a new `Quat`.
    pub fn new(a: f32, b: f32, c: f32, d: f32) -> Self {
         Quat { data: [a, b, c, d] }
    }
    
    /// Component accessor, returns the 1st component of the quaternion.
    #[inline(always)] pub fn a(&self) -> f32 { self[0] }

    /// Component accessor, returns the 2nd component of the quaternion.
    #[inline(always)] pub fn b(&self) -> f32 { self[1] }

    /// Component accessor, returns the 3rd component of the quaternion.
    #[inline(always)] pub fn c(&self) -> f32 { self[2] }

    /// Component accessor, returns the 4th component of the quaternion.
    #[inline(always)] pub fn d(&self) -> f32 { self[3] }
    
    /// Returns the sum of vector components.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let u = quat!(20, 30, 40, 50);
    /// assert_eq!(u.sum(), 20.0 + 30.0 + 40.0 + 50.0);
    /// # }
    /// ```
    pub fn sum(&self) -> f32 {
        self.a() + self.b() + self.c() + self.d()
    }
    
    /// Performs `abs()` on each component, producing a new vector.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let u = quat!(20, -30, 40, -50);
    /// assert_eq!(u.abs(), quat!(20, 30, 40, 50));
    /// # }
    /// ```
    pub fn abs(&self) -> Quat {
        Quat::new(self.a().abs(), self.b().abs(), self.c().abs(), self.d().abs())
    }
    
    /// Returns combined rotation of the `lhs` quaternion followed by rotation around `x` axis.
    ///
    /// Rotation operations are ordere dependent, so
    /// `q.rotate_x(a).rotate_y(b) != q.rotate_y(b).rotate_x(a)`
    ///
    /// Keep in mind, that only unit quaternions represent rotation. When combining multiple
    /// rotations, the resulting quaternion will accumulate floating point errors. You can remedy
    /// that by periodically normalizing the quaterion with `q.normalize()`.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let q = quat!().rotate_x(90_f32.to_rad());
    /// let u = q.rotate_vec(vec3!(1, 1, 0));
    /// assert!(u.approx_equal(vec3!(1, 0, 1), 1e-6));
    /// # }
    /// ```
    pub fn rotate_x(&self, rads: f32) -> Quat {
        let half_angle = rads*0.5;
        let qa = half_angle.cos();
        let qb = half_angle.sin();
    
        self*qa + Quat::new(-self.b(), self.a(), -self.d(), self.c())*qb
    }
    
    /// Returns combined rotation of the `lhs` quaternion followed by rotation around `y` axis.
    ///
    /// Rotation operations are ordere dependent, so
    /// `q.rotate_x(a).rotate_y(b) != q.rotate_y(b).rotate_x(a)`
    ///
    /// Keep in mind, that only unit quaternions represent rotation. When combining multiple
    /// rotations, the resulting quaternion will accumulate floating point errors. You can remedy
    /// that by periodically normalizing the quaterion with `q.normalize()`.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let q = quat!().rotate_y(90_f32.to_rad());
    /// let u = q.rotate_vec(vec3!(1, 1, 0));
    /// assert!(u.approx_equal(vec3!(0, 1, -1), 1e-6));
    /// # }
    /// ```
    pub fn rotate_y(&self, rads: f32) -> Quat {
        let half_angle = rads*0.5;
        let qa = half_angle.cos();
        let qc = half_angle.sin();
    
        self*qa + Quat::new(-self.c(), self.d(), self.a(), -self.b())*qc
    }
    
    /// Returns combined rotation of the `lhs` quaternion followed by rotation around `z` axis.
    ///
    /// Rotation operations are ordere dependent, so
    /// `q.rotate_x(a).rotate_y(b) != q.rotate_y(b).rotate_x(a)`
    ///
    /// Keep in mind, that only unit quaternions represent rotation. When combining multiple
    /// rotations, the resulting quaternion will accumulate floating point errors. You can remedy
    /// that by periodically normalizing the quaterion with `q.normalize()`.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let q = quat!().rotate_z(90_f32.to_rad());
    /// let u = q.rotate_vec(vec3!(0, 1, 1));
    /// assert!(u.approx_equal(vec3!(-1, 0, 1), 1e-6));
    /// # }
    /// ```
    pub fn rotate_z(&self, rads: f32) -> Quat {
        let half_angle = rads*0.5;
        let qa = half_angle.cos();
        let qd = half_angle.sin();
    
        self*qa + Quat::new(-self.d(), -self.c(), self.b(), self.a())*qd
    }
    
    /// Extracts quaternion components into a vector: `Quat(a, b, c, d) -> Vec4(b, c, d, a)`.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let q = quat!(2, 3, 4, 5);
    /// assert_eq!(*q.as_vec4(), vec4!(2, 3, 4, 5));
    /// # }
    /// ```
    pub fn as_vec4(&self) -> &Vec4 {
        unsafe {
            mem::transmute::<&Quat, &Vec4>(&self)
        }
    }
    
    /// Computes the norm of the quaternion (similar to vector length).
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let u = quat!(20, 30, 40, 50);
    /// assert_eq!(u.norm(), ((20*20 + 30*30 + 40*40 + 50*50) as f32).sqrt());
    /// # }
    /// ```
    pub fn norm(&self) -> f32 {
        self.as_vec4().length()
    }
    
    /// Normalizes the quaternion.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let q = quat!(20, 30, 40, 50);
    /// assert!(q.normalize().norm().approx_equal(1.0, 1e-6));
    /// # }
    /// ```
    pub fn normalize(&self) -> Quat {
        self/self.norm()
    }
    
    /// Returns quaternion conjugate. When applied to a unit quaternion, produces the inverse.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let q = quat!(20, 30, 40, 50);
    /// assert_eq!(q.conjugate(), quat!(20, -30, -40, -50));
    /// # }
    /// ```
    pub fn conjugate(&self) -> Quat {
        Quat::new(self.a(), -self.b(), -self.c(), -self.d())
    }
    
    /// Inverts the quaternion. When dealing with unit quaternions, use `conjugate` instead.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let q = quat!().rotate_x(90_f32.to_rad());
    /// let u = q.rotate_vec(vec3!(1, 1, 0));
    /// let v = q.inverse().rotate_vec(u);
    /// assert!(v.approx_equal(vec3!(1, 1, 0), 1e-6));
    /// # }
    /// ```
    pub fn inverse(&self) -> Quat {
        let u = self.as_vec4();
        let norm_square = u.dot(u);
        self.conjugate()/norm_square
    }
}

pub trait QuatQuatOps<Rhs> {
    fn approx_equal(&self, rhs: Rhs, eps: f32) -> bool;

    fn rotate(&self, rhs: Rhs) -> Quat;
}

impl<'a> QuatQuatOps<&'a Quat> for Quat {
    /// Tests for approximate equality within given absolute error.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let q = quat!(20, 30, 40, 50);
    /// assert!(q.approx_equal(q + quat!(1e-7, 1e-7, 1e-7, 1e-7), 1e-6));
    /// assert!(!q.approx_equal(q + quat!(1e-6, 1e-6, 1e-6, 1e-6), 1e-6));
    /// # }
    /// ```
    fn approx_equal(&self, rhs: &Quat, eps: f32) -> bool {
        self.as_vec4().approx_equal(rhs.as_vec4(), eps)
    }

    /// Returns combined rotation of the `lhs` quaternion followed by `rhs` quaternion.
    ///
    /// Unit quaternions represent rotation. Multiple rotations are combined using `rotate()`
    /// method. For example, given rotations `r1` and `r2`, you can obtain a combined rotation that
    /// performs `r1` **then** `r2` as `let r3 = r1.rotate(r2)`. Note the order of arguments!
    /// Quaternion rotation is not associative, so `r1.rotate(r2) != r2.rotate(r1)`.
    ///
    /// `r1.rotate(r2)` simply calls `r2*r1`. However using `rotate()` method is more intuitive,
    /// because the result is equivalent to applying all the operations from left to right.
    ///
    /// When combining multiple rotations, the resulting quaternion will accumulate floating point
    /// errors. You can remedy that by periodically normalizing the quaterion with `q.normalize()`.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let q0 = quat!().rotate_x(90_f32.to_rad());
    /// let q1 = quat!().rotate_y(90_f32.to_rad());
    /// let q = q0.rotate(q1);
    /// let u = q.rotate_vec(vec3!(1, 1, 0));
    /// assert!(u.approx_equal(vec3!(1, 0, -1), 1e-6));
    /// # }
    /// ```
    fn rotate(&self, rhs: &Quat) -> Quat {
        rhs*self
    }
}

impl QuatQuatOps<Quat> for Quat {
    /// Shorthand for `lhs.approx_equals(&rhs, eps)`.
    #[inline(always)] fn approx_equal(&self, rhs: Quat, eps: f32) -> bool {
        self.approx_equal(&rhs, eps)
    }

    /// Shorthand for `lhs.rotate(&rhs)`.
    #[inline(always)] fn rotate(&self, rhs: Quat) -> Quat {
        self.rotate(&rhs)
    }
}

pub trait QuatVec3Ops<Rhs> {
    fn rotate_vec(&self, rhs: Rhs) -> Vec3;
}

impl<'a> QuatVec3Ops<&'a Vec3> for Quat {
    /// Applies rotation to the vector returning a new vector. Quaternion must be normalized
    /// to produce accurate results.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let q = quat!().rotate_y(90_f32.to_rad());
    /// let u = q.rotate_vec(vec3!(1, 1, 0));
    /// assert!(u.approx_equal(vec3!(0, 1, -1), 1e-6));
    /// # }
    /// ```
    fn rotate_vec(&self, rhs: &Vec3) -> Vec3 {
        let t1 = self.a()*self.b();
        let t2 = self.a()*self.c();
        let t3 = self.a()*self.d();
        let t4 = -self.b()*self.b();
        let t5 = self.b()*self.c();
        let t6 = self.b()*self.d();
        let t7 = -self.c()*self.c();
        let t8 = self.c()*self.d();
        let t9 = -self.d()*self.d();
    
        Vec3::new(
              Vec3::new(t7 + t9, t5 - t3, t2 + t6).dot(rhs),
              Vec3::new(t3 + t5, t4 + t9, t8 - t1).dot(rhs),
              Vec3::new(t6 - t2, t1 + t8, t4 + t7).dot(rhs),
        )*2.0 + rhs
    }
}

impl QuatVec3Ops<Vec3> for Quat {
    /// Shorthand for `lhs.rotate_vec(&rhs)`.
    #[inline(always)] fn rotate_vec(&self, rhs: Vec3) -> Vec3 {
        self.rotate_vec(&rhs)
    }
}

impl Display for Quat {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "Quat({}, {}, {}, {})", self[0], self[1], self[2], self[3])
    }
}

impl Index<usize> for Quat {
    type Output = f32;
    
    /// Index notation for acessing components of a quaternion.
    ///
    /// Caveat: due to language constraints, index-based accessors are slower than corresponding
    /// method-based accessors for SIMD implementation.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let q = quat!(20, 30, 40, 50);
    /// assert_eq!(q, quat!(q[0], q[1], q[2], q[3]));
    /// # }
    /// ```
    ///
    /// # Panics
    ///
    /// Will panic if the index is greater than 3.
    #[inline(always)] fn index<'a>(&'a self, i: usize) -> &'a f32 {
        &self.data[i]
    }
}

impl IndexMut<usize> for Quat {

    /// Index notation for mutating components of a quaternion.
    ///
    /// Caveat: due to language constraints, index-based accessors are slower than corresponding
    /// method-based accessors for SIMD implementation.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let mut q = quat!(20, 30, 40, 50);
    /// q[0] = 2.0; q[1] = 3.0; q[2] = 4.0; q[3] = 5.0;
    /// assert_eq!(q, quat!(2, 3, 4, 5));
    /// # }
    /// ```
    ///
    /// # Panics
    ///
    /// Will panic if the index is greater than 3.
    #[inline(always)] fn index_mut<'a>(&'a mut self, i: usize) -> &'a mut f32 {
        &mut self.data[i]
    }
}

impl<'a, 'b> Add<&'b Quat> for &'a Quat {
    type Output = Quat;

    /// Performs component-wise addition of two quaternions, producing a new quaternion.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let q = quat!(20, 30, 40, 50) + quat!(2, 3, 4, 5);
    /// assert_eq!(q, quat!(20 + 2, 30 + 3, 40 + 4, 50 + 5));
    /// # }
    /// ```
    fn add(self, rhs: &Quat) -> Quat {
        Quat::new(self.a() + rhs.a(), self.b() + rhs.b(), self.c() + rhs.c(), self.d() + rhs.d())
    }
}

impl<'a> Add<Quat> for &'a Quat {
    type Output = Quat;
    
    /// Shorthand for `lhs + &rhs`.
    #[inline(always)] fn add(self, rhs: Quat) -> Quat {
        self + &rhs
    }
}

impl<'b> Add<&'b Quat> for Quat {
    type Output = Quat;
    
    /// Shorthand for `&lhs + rhs`.
    #[inline(always)] fn add(self, rhs: &Quat) -> Quat {
        &self + rhs
    }
}

impl Add<Quat> for Quat {
    type Output = Quat;
    
    /// Shorthand for `&lhs + &rhs`.
    #[inline(always)] fn add(self, rhs: Quat) -> Quat {
        &self + &rhs
    }
}

impl<'a> Add<f32> for &'a Quat {
    type Output = Quat;
    
    /// Adds a scalar to each component of a quaternion, producing a new quaternion.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let q = quat!(20, 30, 40, 50) + 2.0;
    /// assert_eq!(q, quat!(20 + 2, 30 + 2, 40 + 2, 50 + 2));
    /// # }
    /// ```
    fn add(self, rhs: f32) -> Quat {
        Quat::new(self.a() + rhs, self.b() + rhs, self.c() + rhs, self.d() + rhs)
    }
}

impl Add<f32> for Quat {
    type Output = Quat;
    
    /// Shorthand for `&lhs + rhs`.
    #[inline(always)] fn add(self, rhs: f32) -> Quat {
        &self + rhs
    }
}

impl<'a, 'b> Sub<&'b Quat> for &'a Quat {
    type Output = Quat;

    /// Subtracts each component of the `rhs` quaternion from the 
    /// corresponding component of the `lhs` quaternion, producing a new quaternion.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let q = quat!(20, 30, 40, 50) - quat!(2, 3, 4, 5);
    /// assert_eq!(q, quat!(20 - 2, 30 - 3, 40 - 4, 50 - 5));
    /// # }
    /// ```
    fn sub(self, rhs: &Quat) -> Quat {
        Quat::new(self.a() - rhs.a(), self.b() - rhs.b(), self.c() - rhs.c(), self.d() - rhs.d())
    }
}

impl<'a> Sub<Quat> for &'a Quat {
    type Output = Quat;
    
    /// Shorthand for `lhs - &rhs`.
    #[inline(always)] fn sub(self, rhs: Quat) -> Quat {
        self - &rhs
    }
}

impl<'b> Sub<&'b Quat> for Quat {
    type Output = Quat;
    
    /// Shorthand for `&lhs - rhs`.
    #[inline(always)] fn sub(self, rhs: &Quat) -> Quat {
        &self - rhs
    }
}

impl Sub<Quat> for Quat {
    type Output = Quat;
    
    /// Shorthand for `&lhs - &rhs`.
    #[inline(always)] fn sub(self, rhs: Quat) -> Quat {
        &self - &rhs
    }
}

impl<'a> Sub<f32> for &'a Quat {
    type Output = Quat;
    
    /// Subtracts a scalar from each component of a quaternion, producing a new quaternion.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let q = quat!(20, 30, 40, 50) - 2.0;
    /// assert_eq!(q, quat!(20 - 2, 30 - 2, 40 - 2, 50 - 2));
    /// # }
    /// ```
    fn sub(self, rhs: f32) -> Quat {
        Quat::new(self.a() - rhs, self.b() - rhs, self.c() - rhs, self.d() - rhs)
    }
}

impl Sub<f32> for Quat {
    type Output = Quat;
    
    /// Shorthand for `&lhs - rhs`.
    #[inline(always)] fn sub(self, rhs: f32) -> Quat {
        &self - rhs
    }
}

impl<'a, 'b> Mul<&'b Quat> for &'a Quat {
    type Output = Quat;

    /// Performs quaternion multiplication producing a new quaternion.
    ///
    /// Unit quaternions represent rotation. Multiple rotations are combined using `rotate()`
    /// method. For example, given rotations `r1` and `r2`, you can obtain a combined rotation that
    /// performs `r1` **then** `r2` as `let r3 = r1.rotate(r2)`. Note the order of arguments!
    /// Quaternion rotation is not associative, so `r1.rotate(r2) != r2.rotate(r1)`.
    ///
    /// `r1.rotate(r2)` simply calls `r2*r1`. However using `rotate()` method is more intuitive,
    /// because the result is equivalent to applying all the operations from left to right.
    ///
    /// When combining multiple rotations, the resulting quaternion will accumulate floating point
    /// errors. You can remedy that by periodically normalizing the quaterion with `q.normalize()`.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let q0 = quat!().rotate_x(90_f32.to_rad());
    /// let q1 = quat!().rotate_y(90_f32.to_rad());
    /// assert_eq!(q1*q0, q0.rotate(q1));
    /// # }
    /// ```
    fn mul(self, rhs: &Quat) -> Quat {
        Quat::new(
            self.a()*rhs.a() - self.b()*rhs.b() - self.c()*rhs.c() - self.d()*rhs.d(),
            self.a()*rhs.b() + self.b()*rhs.a() + self.c()*rhs.d() - self.d()*rhs.c(),
            self.a()*rhs.c() - self.b()*rhs.d() + self.c()*rhs.a() + self.d()*rhs.b(),
            self.a()*rhs.d() + self.b()*rhs.c() - self.c()*rhs.b() + self.d()*rhs.a(),
        )
    }
}

impl<'a> Mul<Quat> for &'a Quat {
    type Output = Quat;
    
    /// Shorthand for `lhs * &rhs`.
    #[inline(always)] fn mul(self, rhs: Quat) -> Quat {
        self * &rhs
    }
}

impl<'b> Mul<&'b Quat> for Quat {
    type Output = Quat;
    
    /// Shorthand for `&lhs * rhs`.
    #[inline(always)] fn mul(self, rhs: &Quat) -> Quat {
        &self * rhs
    }
}

impl Mul<Quat> for Quat {
    type Output = Quat;
    
    /// Shorthand for `&lhs * &rhs`.
    #[inline(always)] fn mul(self, rhs: Quat) -> Quat {
        &self * &rhs
    }
}

impl<'a> Mul<f32> for &'a Quat {
    type Output = Quat;
    
    /// Multiplies each component of a quaternion by a scalar, producing a new quaternion.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let q = quat!(20, 30, 40, 50) * 2.0;
    /// assert_eq!(q, quat!(20 * 2, 30 * 2, 40 * 2, 50 * 2));
    /// # }
    /// ```
    fn mul(self, rhs: f32) -> Quat {
        Quat::new(self.a() * rhs, self.b() * rhs, self.c() * rhs, self.d() * rhs)
    }
}

impl Mul<f32> for Quat {
    type Output = Quat;
    
    /// Shorthand for `&lhs * rhs`.
    #[inline(always)] fn mul(self, rhs: f32) -> Quat {
        &self * rhs
    }
}

impl<'a, 'b> Div<&'b Quat> for &'a Quat {
    type Output = Quat;

    /// Divides each component of the `lhs` quaternion by the 
    /// corresponding component of the `rhs` quaternion, producing a new quaternion.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let q = quat!(20, 30, 40, 50) / quat!(2, 3, 4, 5);
    /// assert_eq!(q, quat!(20 / 2, 30 / 3, 40 / 4, 50 / 5));
    /// # }
    /// ```
    fn div(self, rhs: &Quat) -> Quat {
        Quat::new(self.a() / rhs.a(), self.b() / rhs.b(), self.c() / rhs.c(), self.d() / rhs.d())
    }
}

impl<'a> Div<Quat> for &'a Quat {
    type Output = Quat;
    
    /// Shorthand for `lhs / &rhs`.
    #[inline(always)] fn div(self, rhs: Quat) -> Quat {
        self / &rhs
    }
}

impl<'b> Div<&'b Quat> for Quat {
    type Output = Quat;
    
    /// Shorthand for `&lhs / rhs`.
    #[inline(always)] fn div(self, rhs: &Quat) -> Quat {
        &self / rhs
    }
}

impl Div<Quat> for Quat {
    type Output = Quat;
    
    /// Shorthand for `&lhs / &rhs`.
    #[inline(always)] fn div(self, rhs: Quat) -> Quat {
        &self / &rhs
    }
}

impl<'a> Div<f32> for &'a Quat {
    type Output = Quat;
    
    /// Divides each component of a quaternion by a scalar, producing a new quaternion.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let q = quat!(20, 30, 40, 50) / 2.0;
    /// assert_eq!(q, quat!(20 / 2, 30 / 2, 40 / 2, 50 / 2));
    /// # }
    /// ```
    fn div(self, rhs: f32) -> Quat {
        Quat::new(self.a() / rhs, self.b() / rhs, self.c() / rhs, self.d() / rhs)
    }
}

impl Div<f32> for Quat {
    type Output = Quat;
    
    /// Shorthand for `&lhs / rhs`.
    #[inline(always)] fn div(self, rhs: f32) -> Quat {
        &self / rhs
    }
}

impl<'a> Neg for &'a Quat {
    type Output = Quat;
    
    /// Applies negation to each component of a quaternion, producing a new quaternion.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let q = quat!(20, 30, 40, 50);
    /// assert_eq!(-q, quat!(-20, -30, -40, -50));
    /// # }
    /// ```
    fn neg(self) -> Quat {
        Quat::new(-self.a(), -self.b(), -self.c(), -self.d())
    }
}

impl Neg for Quat {
    type Output = Quat;
    
    /// Shorthand for `-&arg`.
    #[inline(always)] fn neg(self) -> Quat {
        -&self
    }
}

/// Builder macro for creating new quaternions.
///
/// # Examples
///
/// Create a new identity quaternion:
///
/// ```
/// #[macro_use] extern crate vexyz_math;
/// use vexyz_math::*;
///
/// # fn main() {
/// let q = quat!();
/// assert_eq!(q, Quat::new(1.0, 0.0, 0.0, 0.0));
/// # }
/// ```
/// 
/// Create a new quaternion with `a = 2`, `b = 3`, `c = 4`, and `d = 5`:
///
/// ```
/// #[macro_use] extern crate vexyz_math;
/// use vexyz_math::*;
///
/// # fn main() {
/// let q = quat!(2.0, 3.0, 4.0, 5.0);
/// assert_eq!(q, Quat::new(2.0, 3.0, 4.0, 5.0));
/// # }
/// ```
#[macro_export]
macro_rules! quat {
    () => {{
        Quat::new(1.0, 0.0, 0.0, 0.0)
    }};
    ($a:expr, $b:expr, $c:expr, $d:expr) => {{
        Quat::new($a as f32, $b as f32, $c as f32, $d as f32)
    }};
    ($a:expr, $b:expr, $c:expr, $d:expr,) => {{
        Quat::new($a as f32, $b as f32, $c as f32, $d as f32)
    }};
}
