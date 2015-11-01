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
pub struct Quat { data: [f64; 4] }

impl Quat {
    /// Constructs a new `Quat`.
    pub fn new(a: f64, b: f64, c: f64, d: f64) -> Self {
         Quat { data: [a, b, c, d] }
    }
    
    /// Component accessor, returns the 1st component of the quaternion.
    #[inline(always)] pub fn a(&self) -> f64 { self[0] }

    /// Component accessor, returns the 2nd component of the quaternion.
    #[inline(always)] pub fn b(&self) -> f64 { self[1] }

    /// Component accessor, returns the 3rd component of the quaternion.
    #[inline(always)] pub fn c(&self) -> f64 { self[2] }

    /// Component accessor, returns the 4th component of the quaternion.
    #[inline(always)] pub fn d(&self) -> f64 { self[3] }
    
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
    pub fn sum(&self) -> f64 {
        self[0] + self[1] + self[2] + self[3]
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
        Quat::new(self[0].abs(), self[1].abs(), self[2].abs(), self[3].abs())
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
    /// let q = quat!().rotate_x(90_f64.to_radians());
    /// let u = q.rotate_vec(vec3!(1, 1, 0));
    /// assert!(u.approx_equal(vec3!(1, 0, 1), 1e-8));
    /// # }
    /// ```
    pub fn rotate_x(&self, rads: f64) -> Quat {
        let half_angle = rads*0.5;
        let qa = half_angle.cos();
        let qb = half_angle.sin();
    
        self*qa + Quat::new(-self[1], self[0], -self[3], self[2])*qb
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
    /// let q = quat!().rotate_y(90_f64.to_radians());
    /// let u = q.rotate_vec(vec3!(1, 1, 0));
    /// assert!(u.approx_equal(vec3!(0, 1, -1), 1e-8));
    /// # }
    /// ```
    pub fn rotate_y(&self, rads: f64) -> Quat {
        let half_angle = rads*0.5;
        let qa = half_angle.cos();
        let qc = half_angle.sin();
    
        self*qa + Quat::new(-self[2], self[3], self[0], -self[1])*qc
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
    /// let q = quat!().rotate_z(90_f64.to_radians());
    /// let u = q.rotate_vec(vec3!(0, 1, 1));
    /// assert!(u.approx_equal(vec3!(-1, 0, 1), 1e-8));
    /// # }
    /// ```
    pub fn rotate_z(&self, rads: f64) -> Quat {
        let half_angle = rads*0.5;
        let qa = half_angle.cos();
        let qd = half_angle.sin();
    
        self*qa + Quat::new(-self[3], -self[2], self[1], self[0])*qd
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
    /// assert_eq!(u.norm(), ((20*20 + 30*30 + 40*40 + 50*50) as f64).sqrt());
    /// # }
    /// ```
    pub fn norm(&self) -> f64 {
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
    /// assert!(q.normalize().norm().approx_equal(1.0, 1e-8));
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
        Quat::new(self[0], -self[1], -self[2], -self[3])
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
    /// let q = quat!().rotate_x(90_f64.to_radians());
    /// let u = q.rotate_vec(vec3!(1, 1, 0));
    /// let v = q.inverse().rotate_vec(u);
    /// assert!(v.approx_equal(vec3!(1, 1, 0), 1e-8));
    /// # }
    /// ```
    pub fn inverse(&self) -> Quat {
        let u = self.as_vec4();
        let norm_square = u.dot(u);
        self.conjugate()/norm_square
    }
}

pub trait QuatQuatOps<Rhs> {
    fn approx_equal(&self, rhs: Rhs, eps: f64) -> bool;

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
    /// assert!(q.approx_equal(q + quat!(1e-9, 1e-9, 1e-9, 1e-9), 1e-8));
    /// assert!(!q.approx_equal(q + quat!(1e-8, 1e-8, 1e-8, 1e-8), 1e-8));
    /// # }
    /// ```
    fn approx_equal(&self, rhs: &Quat, eps: f64) -> bool {
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
    /// let q0 = quat!().rotate_x(90_f64.to_radians());
    /// let q1 = quat!().rotate_y(90_f64.to_radians());
    /// let q = q0.rotate(q1);
    /// let u = q.rotate_vec(vec3!(1, 1, 0));
    /// assert!(u.approx_equal(vec3!(1, 0, -1), 1e-8));
    /// # }
    /// ```
    fn rotate(&self, rhs: &Quat) -> Quat {
        rhs*self
    }
}

impl QuatQuatOps<Quat> for Quat {
    /// Shorthand for `lhs.approx_equals(&rhs, eps)`.
    #[inline(always)] fn approx_equal(&self, rhs: Quat, eps: f64) -> bool {
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
    /// let q = quat!().rotate_y(90_f64.to_radians());
    /// let u = q.rotate_vec(vec3!(1, 1, 0));
    /// assert!(u.approx_equal(vec3!(0, 1, -1), 1e-8));
    /// # }
    /// ```
    fn rotate_vec(&self, rhs: &Vec3) -> Vec3 {
        let t1 = self[0]*self[1];
        let t2 = self[0]*self[2];
        let t3 = self[0]*self[3];
        let t4 = -self[1]*self[1];
        let t5 = self[1]*self[2];
        let t6 = self[1]*self[3];
        let t7 = -self[2]*self[2];
        let t8 = self[2]*self[3];
        let t9 = -self[3]*self[3];
    
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
    type Output = f64;
    
    /// Index notation for acessing components of a quaternion.
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
    #[inline(always)] fn index<'a>(&'a self, i: usize) -> &'a f64 {
        &self.data[i]
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
        Quat::new(self[0] + rhs[0], self[1] + rhs[1], self[2] + rhs[2], self[3] + rhs[3])
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

impl<'a> Add<f64> for &'a Quat {
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
    fn add(self, rhs: f64) -> Quat {
        Quat::new(self[0] + rhs, self[1] + rhs, self[2] + rhs, self[3] + rhs)
    }
}

impl Add<f64> for Quat {
    type Output = Quat;
    
    /// Shorthand for `&lhs + rhs`.
    #[inline(always)] fn add(self, rhs: f64) -> Quat {
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
        Quat::new(self[0] - rhs[0], self[1] - rhs[1], self[2] - rhs[2], self[3] - rhs[3])
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

impl<'a> Sub<f64> for &'a Quat {
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
    fn sub(self, rhs: f64) -> Quat {
        Quat::new(self[0] - rhs, self[1] - rhs, self[2] - rhs, self[3] - rhs)
    }
}

impl Sub<f64> for Quat {
    type Output = Quat;
    
    /// Shorthand for `&lhs - rhs`.
    #[inline(always)] fn sub(self, rhs: f64) -> Quat {
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
    /// let q0 = quat!().rotate_x(90_f64.to_radians());
    /// let q1 = quat!().rotate_y(90_f64.to_radians());
    /// assert_eq!(q1*q0, q0.rotate(q1));
    /// # }
    /// ```
    fn mul(self, rhs: &Quat) -> Quat {
        Quat::new(
            self[0]*rhs[0] - self[1]*rhs[1] - self[2]*rhs[2] - self[3]*rhs[3],
            self[0]*rhs[1] + self[1]*rhs[0] + self[2]*rhs[3] - self[3]*rhs[2],
            self[0]*rhs[2] - self[1]*rhs[3] + self[2]*rhs[0] + self[3]*rhs[1],
            self[0]*rhs[3] + self[1]*rhs[2] - self[2]*rhs[1] + self[3]*rhs[0],
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

impl<'a> Mul<f64> for &'a Quat {
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
    fn mul(self, rhs: f64) -> Quat {
        Quat::new(self[0] * rhs, self[1] * rhs, self[2] * rhs, self[3] * rhs)
    }
}

impl Mul<f64> for Quat {
    type Output = Quat;
    
    /// Shorthand for `&lhs * rhs`.
    #[inline(always)] fn mul(self, rhs: f64) -> Quat {
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
        Quat::new(self[0] / rhs[0], self[1] / rhs[1], self[2] / rhs[2], self[3] / rhs[3])
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

impl<'a> Div<f64> for &'a Quat {
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
    fn div(self, rhs: f64) -> Quat {
        Quat::new(self[0] / rhs, self[1] / rhs, self[2] / rhs, self[3] / rhs)
    }
}

impl Div<f64> for Quat {
    type Output = Quat;
    
    /// Shorthand for `&lhs / rhs`.
    #[inline(always)] fn div(self, rhs: f64) -> Quat {
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
        Quat::new(-self[0], -self[1], -self[2], -self[3])
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
        Quat::new($a as f64, $b as f64, $c as f64, $d as f64)
    }};
    ($a:expr, $b:expr, $c:expr, $d:expr,) => {{
        Quat::new($a as f64, $b as f64, $c as f64, $d as f64)
    }};
}
