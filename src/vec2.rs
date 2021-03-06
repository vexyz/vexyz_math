// Generated code.
use std::fmt::{Display, Formatter, Result};
use std::ops::*;
use std::mem;
use Vec2b;
use Mat2;

/// 2-dimensional vector with 32 bit floating point `x`, and `y` components.
///
/// Vectors can also represent colors with the help of
/// `r()`, and `g()` accessors.
///
/// Most operators and methods on vectors are performed in component-wise fashion. The notable
/// exception is vector-matrix multiplication.
#[derive(Debug, PartialEq, Copy, Clone)]
pub struct Vec2 { data: [f32; 2] }

impl Vec2 {
    /// Constructs a new `Vec2`.
    pub fn new(x: f32, y: f32) -> Self {
         Vec2 { data: [x, y] }
    }
    
    /// Component accessor, returns the 1st component of the vector.
    #[inline(always)] pub fn x(&self) -> f32 { self[0] }

    /// Component accessor, returns the 2nd component of the vector.
    #[inline(always)] pub fn y(&self) -> f32 { self[1] }
    
    
    /// Color-style component accessor, returns the 1st component of the vector.
    #[inline(always)] pub fn r(&self) -> f32 { self[0] }

    /// Color-style component accessor, returns the 2nd component of the vector.
    #[inline(always)] pub fn g(&self) -> f32 { self[1] }
    
    /// Returns the sum of vector components.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let u = vec2!(20, 30);
    /// assert_eq!(u.sum(), 20.0 + 30.0);
    /// # }
    /// ```
    pub fn sum(&self) -> f32 {
        self.x() + self.y()
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
    /// let u = vec2!(20, -30);
    /// assert_eq!(u.abs(), vec2!(20, 30));
    /// # }
    /// ```
    pub fn abs(&self) -> Vec2 {
        Vec2::new(self.x().abs(), self.y().abs())
    }
    
    /// Computes the length of the vector.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let u = vec2!(20, 30);
    /// assert_eq!(u.length(), ((20*20 + 30*30) as f32).sqrt());
    /// # }
    /// ```
    pub fn length(&self) -> f32 {
        self.dot(self).sqrt()
    }
}

pub trait Vec2Ops<Rhs> {
    fn less_than(&self, rhs: Rhs) -> Vec2b;

    fn less_than_equal(&self, rhs: Rhs) -> Vec2b;

    fn greater_than(&self, rhs: Rhs) -> Vec2b;

    fn greater_than_equals(&self, rhs: Rhs) -> Vec2b;

    fn equal(&self, rhs: Rhs) -> Vec2b;

    fn not_equal(&self, rhs: Rhs) -> Vec2b;

    fn approx_equal(&self, rhs: Rhs, eps: f32) -> bool;

    fn dot(&self, rhs: Rhs) -> f32;

    fn lerp(&self, rhs: Rhs, a: f32) -> Vec2;
}

impl<'a> Vec2Ops<&'a Vec2> for Vec2 {
    /// Performs component-wise numerical `less than` comparision of two vectors,
    /// returning a boolean vector.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let u = vec2!(20, 30);
    /// let v = u + vec2!(-1, 0);
    /// assert_eq!(u.less_than(v), bvec2!(u.x() < v.x(), u.y() < v.y()));
    /// # }
    /// ```
    fn less_than(&self, rhs: &Vec2) -> Vec2b {
        Vec2b::new(self.x() < rhs.x(), self.y() < rhs.y())
    }

    /// Performs component-wise numerical `less than or equal` comparision of two vectors,
    /// returning a boolean vector.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let u = vec2!(20, 30);
    /// let v = u + vec2!(-1, 0);
    /// assert_eq!(u.less_than_equal(v), bvec2!(u.x() <= v.x(), u.y() <= v.y()));
    /// # }
    /// ```
    fn less_than_equal(&self, rhs: &Vec2) -> Vec2b {
        Vec2b::new(self.x() <= rhs.x(), self.y() <= rhs.y())
    }

    /// Performs component-wise numerical `greater than` comparision of two vectors,
    /// returning a boolean vector.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let u = vec2!(20, 30);
    /// let v = u + vec2!(-1, 0);
    /// assert_eq!(u.greater_than(v), bvec2!(u.x() > v.x(), u.y() > v.y()));
    /// # }
    /// ```
    fn greater_than(&self, rhs: &Vec2) -> Vec2b {
        Vec2b::new(self.x() > rhs.x(), self.y() > rhs.y())
    }

    /// Performs component-wise numerical `greater than or equal` comparision of two vectors,
    /// returning a boolean vector.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let u = vec2!(20, 30);
    /// let v = u + vec2!(-1, 0);
    /// assert_eq!(u.greater_than_equals(v), bvec2!(u.x() >= v.x(), u.y() >= v.y()));
    /// # }
    /// ```
    fn greater_than_equals(&self, rhs: &Vec2) -> Vec2b {
        Vec2b::new(self.x() >= rhs.x(), self.y() >= rhs.y())
    }

    /// Performs component-wise numerical `equal` comparision of two vectors,
    /// returning a boolean vector.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let u = vec2!(20, 30);
    /// let v = u + vec2!(-1, 0);
    /// assert_eq!(u.equal(v), bvec2!(u.x() == v.x(), u.y() == v.y()));
    /// # }
    /// ```
    fn equal(&self, rhs: &Vec2) -> Vec2b {
        Vec2b::new(self.x() == rhs.x(), self.y() == rhs.y())
    }

    /// Performs component-wise numerical `not equal` comparision of two vectors,
    /// returning a boolean vector.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let u = vec2!(20, 30);
    /// let v = u + vec2!(-1, 0);
    /// assert_eq!(u.not_equal(v), bvec2!(u.x() != v.x(), u.y() != v.y()));
    /// # }
    /// ```
    fn not_equal(&self, rhs: &Vec2) -> Vec2b {
        Vec2b::new(self.x() != rhs.x(), self.y() != rhs.y())
    }

    /// Tests for approximate equality within given absolute error.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let u = vec2!(20, 30);
    /// assert!(u.approx_equal(u + vec2!(1e-7), 1e-6));
    /// assert!(!u.approx_equal(u + vec2!(1e-6), 1e-6));
    /// # }
    /// ```
    fn approx_equal(&self, rhs: &Vec2, eps: f32) -> bool {
        let eps = Vec2::new(eps, eps);
        (self - rhs).abs().less_than(eps).all()
    }

    /// Returns dot product of two vectors.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let u = vec2!(20, 30);
    /// let v = vec2!(2, 3);
    /// assert_eq!(u.dot(v), 20.0 * 2.0 + 30.0 * 3.0);
    /// # }
    /// ```
    fn dot(&self, rhs: &Vec2) -> f32 {
        (self * rhs).sum()
    }

    /// Computes linear interpolation `self*(1 - a) + rhs*a` producing a new vector.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let u = vec2!(20, 30);
    /// let v = vec2!(2, 3);
    /// let w = u.lerp(v, 0.25);
    /// assert_eq!(w, vec2!(20.0*0.75 + 2.0*0.25, 30.0*0.75 + 3.0*0.25));
    /// # }
    /// ```
    fn lerp(&self, rhs: &Vec2, a: f32) -> Vec2 {
        self*(1.0 - a) + rhs*a
    }
}

impl Vec2Ops<Vec2> for Vec2 {
    /// Shorthand for `lhs.less_than(&rhs)`.
    #[inline(always)] fn less_than(&self, rhs: Vec2) -> Vec2b {
        self.less_than(&rhs)
    }

    /// Shorthand for `lhs.less_than_equal(&rhs)`.
    #[inline(always)] fn less_than_equal(&self, rhs: Vec2) -> Vec2b {
        self.less_than_equal(&rhs)
    }

    /// Shorthand for `lhs.greater_than(&rhs)`.
    #[inline(always)] fn greater_than(&self, rhs: Vec2) -> Vec2b {
        self.greater_than(&rhs)
    }

    /// Shorthand for `lhs.greater_than_equals(&rhs)`.
    #[inline(always)] fn greater_than_equals(&self, rhs: Vec2) -> Vec2b {
        self.greater_than_equals(&rhs)
    }

    /// Shorthand for `lhs.equal(&rhs)`.
    #[inline(always)] fn equal(&self, rhs: Vec2) -> Vec2b {
        self.equal(&rhs)
    }

    /// Shorthand for `lhs.not_equal(&rhs)`.
    #[inline(always)] fn not_equal(&self, rhs: Vec2) -> Vec2b {
        self.not_equal(&rhs)
    }

    /// Shorthand for `lhs.approx_equals(&rhs, eps)`.
    #[inline(always)] fn approx_equal(&self, rhs: Vec2, eps: f32) -> bool {
        self.approx_equal(&rhs, eps)
    }

    /// Shorthand for `lhs.dot(&rhs)`.
    #[inline(always)] fn dot(&self, rhs: Vec2) -> f32 {
        self.dot(&rhs)
    }

    /// Shorthand for `lhs.determinant(&rhs)`.
    #[inline(always)] fn lerp(&self, rhs: Vec2, a: f32) -> Vec2 {
        self.lerp(&rhs, a)
    }
}

impl Display for Vec2 {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "Vec2({}, {})", self[0], self[1])
    }
}

impl Index<usize> for Vec2 {
    type Output = f32;
    
    /// Index notation for acessing components of a vector.
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
    /// let u = vec2!(20, 30);
    /// assert_eq!(u, vec2!(u[0], u[1]));
    /// # }
    /// ```
    ///
    /// # Panics
    ///
    /// Will panic if the index is greater than 1.
    #[inline(always)] fn index<'a>(&'a self, i: usize) -> &'a f32 {
        &self.data[i]
    }
}

impl IndexMut<usize> for Vec2 {

    /// Index notation for mutating components of a vector.
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
    /// let mut u = vec2!(20, 30);
    /// u[0] = 2.0; u[1] = 3.0;
    /// assert_eq!(u, vec2!(2, 3));
    /// # }
    /// ```
    ///
    /// # Panics
    ///
    /// Will panic if the index is greater than 1.
    #[inline(always)] fn index_mut<'a>(&'a mut self, i: usize) -> &'a mut f32 {
        &mut self.data[i]
    }
}

impl<'a, 'b> Add<&'b Vec2> for &'a Vec2 {
    type Output = Vec2;

    /// Performs component-wise addition of two vectors, producing a new vector.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let u = vec2!(20, 30) + vec2!(2, 3);
    /// assert_eq!(u, vec2!(20 + 2, 30 + 3));
    /// # }
    /// ```
    fn add(self, rhs: &Vec2) -> Vec2 {
        Vec2::new(self.x() + rhs.x(), self.y() + rhs.y())
    }
}

impl<'a> Add<Vec2> for &'a Vec2 {
    type Output = Vec2;
    
    /// Shorthand for `lhs + &rhs`.
    #[inline(always)] fn add(self, rhs: Vec2) -> Vec2 {
        self + &rhs
    }
}

impl<'b> Add<&'b Vec2> for Vec2 {
    type Output = Vec2;
    
    /// Shorthand for `&lhs + rhs`.
    #[inline(always)] fn add(self, rhs: &Vec2) -> Vec2 {
        &self + rhs
    }
}

impl Add<Vec2> for Vec2 {
    type Output = Vec2;
    
    /// Shorthand for `&lhs + &rhs`.
    #[inline(always)] fn add(self, rhs: Vec2) -> Vec2 {
        &self + &rhs
    }
}

impl<'a> Add<f32> for &'a Vec2 {
    type Output = Vec2;
    
    /// Adds a scalar to each component of a vector, producing a new vector.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let u = vec2!(20, 30) + 2.0;
    /// assert_eq!(u, vec2!(20 + 2, 30 + 2));
    /// # }
    /// ```
    fn add(self, rhs: f32) -> Vec2 {
        Vec2::new(self.x() + rhs, self.y() + rhs)
    }
}

impl Add<f32> for Vec2 {
    type Output = Vec2;
    
    /// Shorthand for `&lhs + rhs`.
    #[inline(always)] fn add(self, rhs: f32) -> Vec2 {
        &self + rhs
    }
}

impl<'a, 'b> Sub<&'b Vec2> for &'a Vec2 {
    type Output = Vec2;

    /// Subtracts each component of the `rhs` vector from the 
    /// corresponding component of the `lhs` vector, producing a new vector.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let u = vec2!(20, 30) - vec2!(2, 3);
    /// assert_eq!(u, vec2!(20 - 2, 30 - 3));
    /// # }
    /// ```
    fn sub(self, rhs: &Vec2) -> Vec2 {
        Vec2::new(self.x() - rhs.x(), self.y() - rhs.y())
    }
}

impl<'a> Sub<Vec2> for &'a Vec2 {
    type Output = Vec2;
    
    /// Shorthand for `lhs - &rhs`.
    #[inline(always)] fn sub(self, rhs: Vec2) -> Vec2 {
        self - &rhs
    }
}

impl<'b> Sub<&'b Vec2> for Vec2 {
    type Output = Vec2;
    
    /// Shorthand for `&lhs - rhs`.
    #[inline(always)] fn sub(self, rhs: &Vec2) -> Vec2 {
        &self - rhs
    }
}

impl Sub<Vec2> for Vec2 {
    type Output = Vec2;
    
    /// Shorthand for `&lhs - &rhs`.
    #[inline(always)] fn sub(self, rhs: Vec2) -> Vec2 {
        &self - &rhs
    }
}

impl<'a> Sub<f32> for &'a Vec2 {
    type Output = Vec2;
    
    /// Subtracts a scalar from each component of a vector, producing a new vector.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let u = vec2!(20, 30) - 2.0;
    /// assert_eq!(u, vec2!(20 - 2, 30 - 2));
    /// # }
    /// ```
    fn sub(self, rhs: f32) -> Vec2 {
        Vec2::new(self.x() - rhs, self.y() - rhs)
    }
}

impl Sub<f32> for Vec2 {
    type Output = Vec2;
    
    /// Shorthand for `&lhs - rhs`.
    #[inline(always)] fn sub(self, rhs: f32) -> Vec2 {
        &self - rhs
    }
}

impl<'a, 'b> Mul<&'b Vec2> for &'a Vec2 {
    type Output = Vec2;

    /// Performs component-wise multiplication of two vectors, producing a new vector.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let u = vec2!(20, 30) * vec2!(2, 3);
    /// assert_eq!(u, vec2!(20 * 2, 30 * 3));
    /// # }
    /// ```
    fn mul(self, rhs: &Vec2) -> Vec2 {
        Vec2::new(self.x() * rhs.x(), self.y() * rhs.y())
    }
}

impl<'a> Mul<Vec2> for &'a Vec2 {
    type Output = Vec2;
    
    /// Shorthand for `lhs * &rhs`.
    #[inline(always)] fn mul(self, rhs: Vec2) -> Vec2 {
        self * &rhs
    }
}

impl<'b> Mul<&'b Vec2> for Vec2 {
    type Output = Vec2;
    
    /// Shorthand for `&lhs * rhs`.
    #[inline(always)] fn mul(self, rhs: &Vec2) -> Vec2 {
        &self * rhs
    }
}

impl Mul<Vec2> for Vec2 {
    type Output = Vec2;
    
    /// Shorthand for `&lhs * &rhs`.
    #[inline(always)] fn mul(self, rhs: Vec2) -> Vec2 {
        &self * &rhs
    }
}

impl<'a> Mul<f32> for &'a Vec2 {
    type Output = Vec2;
    
    /// Multiplies each component of a vector by a scalar, producing a new vector.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let u = vec2!(20, 30) * 2.0;
    /// assert_eq!(u, vec2!(20 * 2, 30 * 2));
    /// # }
    /// ```
    fn mul(self, rhs: f32) -> Vec2 {
        Vec2::new(self.x() * rhs, self.y() * rhs)
    }
}

impl Mul<f32> for Vec2 {
    type Output = Vec2;
    
    /// Shorthand for `&lhs * rhs`.
    #[inline(always)] fn mul(self, rhs: f32) -> Vec2 {
        &self * rhs
    }
}

impl<'a, 'b> Div<&'b Vec2> for &'a Vec2 {
    type Output = Vec2;

    /// Divides each component of the `lhs` vector by the 
    /// corresponding component of the `rhs` vector, producing a new vector.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let u = vec2!(20, 30) / vec2!(2, 3);
    /// assert_eq!(u, vec2!(20 / 2, 30 / 3));
    /// # }
    /// ```
    fn div(self, rhs: &Vec2) -> Vec2 {
        Vec2::new(self.x() / rhs.x(), self.y() / rhs.y())
    }
}

impl<'a> Div<Vec2> for &'a Vec2 {
    type Output = Vec2;
    
    /// Shorthand for `lhs / &rhs`.
    #[inline(always)] fn div(self, rhs: Vec2) -> Vec2 {
        self / &rhs
    }
}

impl<'b> Div<&'b Vec2> for Vec2 {
    type Output = Vec2;
    
    /// Shorthand for `&lhs / rhs`.
    #[inline(always)] fn div(self, rhs: &Vec2) -> Vec2 {
        &self / rhs
    }
}

impl Div<Vec2> for Vec2 {
    type Output = Vec2;
    
    /// Shorthand for `&lhs / &rhs`.
    #[inline(always)] fn div(self, rhs: Vec2) -> Vec2 {
        &self / &rhs
    }
}

impl<'a> Div<f32> for &'a Vec2 {
    type Output = Vec2;
    
    /// Divides each component of a vector by a scalar, producing a new vector.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let u = vec2!(20, 30) / 2.0;
    /// assert_eq!(u, vec2!(20 / 2, 30 / 2));
    /// # }
    /// ```
    fn div(self, rhs: f32) -> Vec2 {
        Vec2::new(self.x() / rhs, self.y() / rhs)
    }
}

impl Div<f32> for Vec2 {
    type Output = Vec2;
    
    /// Shorthand for `&lhs / rhs`.
    #[inline(always)] fn div(self, rhs: f32) -> Vec2 {
        &self / rhs
    }
}

impl<'a> Neg for &'a Vec2 {
    type Output = Vec2;
    
    /// Applies negation to each component of a vector, producing a new vector.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let u = vec2!(20, 30);
    /// assert_eq!(-u, vec2!(-20, -30));
    /// # }
    /// ```
    fn neg(self) -> Vec2 {
        Vec2::new(-self.x(), -self.y())
    }
}

impl Neg for Vec2 {
    type Output = Vec2;
    
    /// Shorthand for `-&arg`.
    #[inline(always)] fn neg(self) -> Vec2 {
        -&self
    }
}

impl<'a, 'b> Mul<&'b Mat2> for &'a Vec2 {
    type Output = Vec2;

    /// Multiplies a transpose of a vector by a matrix, producing a new vector.
    ///
    /// When all the matrix columns are orthogonal to each other, and each column has a unit length,
    /// then the matrix is called 'orthogonal matrix'. Orthogonal matrices represent rotation.
    /// Transpose of an orthogonal matrix is equal to its inverse.
    ///
    /// This operator is equivalent to matrix.transpose()*vector. Moreover, if the `rhs` matrix is
    /// orthogonal, then it is equivalent to matrix.inverse()*vector.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let u = vec2!(20, 30);
    /// let m = mat2!(
    ///     15.0, 16.0,
    ///     25.0, 26.0,
    /// );
    /// let v = vec2!(
    ///     u.dot(vec2!(15.0, 16.0)),
    ///     u.dot(vec2!(25.0, 26.0)),
    /// );
    /// assert_eq!(u*m, v);
    /// # }
    /// ```
    fn mul(self, rhs: &Mat2) -> Vec2 {
        Vec2::new(self.dot(rhs[0]), self.dot(rhs[1]))
    }
}

impl<'a> Mul<Mat2> for &'a Vec2 {
    type Output = Vec2;
    
    /// Shorthand for `lhs * &rhs`.
    #[inline(always)] fn mul(self, rhs: Mat2) -> Vec2 {
        self * &rhs
    }
}

impl<'b> Mul<&'b Mat2> for Vec2 {
    type Output = Vec2;
    
    /// Shorthand for `&lhs * rhs`.
    #[inline(always)] fn mul(self, rhs: &Mat2) -> Vec2 {
        &self * rhs
    }
}

impl Mul<Mat2> for Vec2 {
    type Output = Vec2;
    
    /// Shorthand for `&lhs * &rhs`.
    #[inline(always)] fn mul(self, rhs: Mat2) -> Vec2 {
        &self * &rhs
    }
}

/// Builder macro for creating new 2-dimensional vectors with 32 bit floating point components.
///
/// # Examples
///
/// Create a new vector with all components set to `2.0`:
///
/// ```
/// #[macro_use] extern crate vexyz_math;
/// use vexyz_math::*;
///
/// # fn main() {
/// let u = vec2!(2.0);
/// assert_eq!(u, Vec2::new(2.0, 2.0));
/// # }
/// ```
/// 
/// Create a new vector with `x = 2`, and `y = 3`:
///
/// ```
/// #[macro_use] extern crate vexyz_math;
/// use vexyz_math::*;
///
/// # fn main() {
/// let u = vec2!(2.0, 3.0);
/// assert_eq!(u, Vec2::new(2.0, 3.0));
/// # }
/// ```
#[macro_export]
macro_rules! vec2 {
    ($s:expr) => {{
        let s = $s as f32;
        Vec2::new(s, s)
    }};
    ($x:expr, $y:expr) => {{
        Vec2::new($x as f32, $y as f32)
    }};
    ($x:expr, $y:expr,) => {{
        Vec2::new($x as f32, $y as f32)
    }};
}
