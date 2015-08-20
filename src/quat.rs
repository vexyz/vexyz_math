// Generated code.
use std::ops::*;

/// Quaternion with 4 floating-point components `a`, `b`, `c`, and `d`.
/// Unit quaternions represent rotation.
///
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
/// errors. You can fix that by periodically normalizing the quaterion with `q.normalize()`.
///
/// Aside from rotation and multiplication, Vexyz provides a variety of component-wise
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

    /// Performs component-wise addition of two quaternions producing a new quaternion.
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
    
    /// Adds a scalar to each component of a quaternion producing a new quaternion.
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
    /// corresponding component of the `lhs` quaternion producing a new quaternion.
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
    
    /// Subtracts a scalar from each component of a quaternion producing a new quaternion.
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
    /// When `lhs` and `rhs` are both unit quaternions, multiplication represents rotation.
    ///
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
    /// errors. You can fix that by periodically normalizing the quaterion with `q.normalize()`.
    ///
    /// # Examples
    ///
    /// ```
    /// assert(true); //XXX fix this
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
    
    /// Multiplies each component of a quaternion by a scalar producing a new quaternion.
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
    /// corresponding component of the `rhs` quaternion producing a new quaternion.
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
    
    /// Divides each component of a quaternion by a scalar producing a new quaternion.
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
    
    /// Applies negation to each component of a quaternion producing a new quaternion.
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
/// assert_eq!(q, Quat::new(0.0, 0.0, 0.0, 1.0));
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
        Quat::new(0.0, 0.0, 0.0, 1.0)
    }};
    ($a:expr, $b:expr, $c:expr, $d:expr) => {{
        Quat::new($a as f64, $b as f64, $c as f64, $d as f64)
    }};
    ($a:expr, $b:expr, $c:expr, $d:expr,) => {{
        Quat::new($a as f64, $b as f64, $c as f64, $d as f64)
    }};
}
