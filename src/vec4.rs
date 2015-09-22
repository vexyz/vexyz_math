// Generated code.
use std::fmt::{Display, Formatter, Result};
use std::ops::*;
use Vec4b;

/// 4-dimensional vector with floating point `x`, `y`, `z`, and `w` components.
///
/// Vectors can also represent colors with the help of
/// `r()`, `g()`, `b()`, and `a()` accessors.
///
/// Most operators and methods on vectors are performed in component-wise fashion. The notable
/// exception is vector-matrix multiplication.
#[derive(Debug, PartialEq, Copy, Clone)]
pub struct Vec4 { data: [f64; 4] }

impl Vec4 {
    /// Constructs a new `Vec4`.
    pub fn new(x: f64, y: f64, z: f64, w: f64) -> Self {
         Vec4 { data: [x, y, z, w] }
    }
    
    /// Component accessor, returns the 1st component of the vector.
    #[inline(always)] pub fn x(&self) -> f64 { self[0] }

    /// Component accessor, returns the 2nd component of the vector.
    #[inline(always)] pub fn y(&self) -> f64 { self[1] }

    /// Component accessor, returns the 3rd component of the vector.
    #[inline(always)] pub fn z(&self) -> f64 { self[2] }

    /// Component accessor, returns the 4th component of the vector.
    #[inline(always)] pub fn w(&self) -> f64 { self[3] }
    
	
    /// Color-style component accessor, returns the 1st component of the vector.
    #[inline(always)] pub fn r(&self) -> f64 { self[0] }

    /// Color-style component accessor, returns the 2nd component of the vector.
    #[inline(always)] pub fn g(&self) -> f64 { self[1] }

    /// Color-style component accessor, returns the 3rd component of the vector.
    #[inline(always)] pub fn b(&self) -> f64 { self[2] }

    /// Color-style component accessor, returns the 4th component of the vector.
    #[inline(always)] pub fn a(&self) -> f64 { self[3] }
	
	/// Returns the sum of vector components.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let u = vec4!(20, 30, 40, 50);
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
    /// let u = vec4!(20, -30, 40, -50);
    /// assert_eq!(u.abs(), vec4!(20, 30, 40, 50));
    /// # }
    /// ```
    pub fn abs(&self) -> Vec4 {
        Vec4::new(self[0].abs(), self[1].abs(), self[2].abs(), self[3].abs())
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
    /// let u = vec4!(20, 30, 40, 50);
    /// assert_eq!(u.length(), ((20*20 + 30*30 + 40*40 + 50*50) as f64).sqrt());
    /// # }
    /// ```
	pub fn length(&self) -> f64 {
		self.dot(self).sqrt()
	}
}

pub trait Vec4Ops<Rhs> {
    fn less_than(&self, rhs: Rhs) -> Vec4b;

    fn less_than_equal(&self, rhs: Rhs) -> Vec4b;

    fn greater_than(&self, rhs: Rhs) -> Vec4b;

    fn greater_than_equals(&self, rhs: Rhs) -> Vec4b;

    fn equal(&self, rhs: Rhs) -> Vec4b;

    fn not_equal(&self, rhs: Rhs) -> Vec4b;

    fn approx_equal(&self, rhs: Rhs, eps: f64) -> bool;

    fn dot(&self, rhs: Rhs) -> f64;

    fn lerp(&self, rhs: Rhs, a: f64) -> Vec4;
}

impl<'a> Vec4Ops<&'a Vec4> for Vec4 {
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
    /// let u = vec4!(20, 30, 40, 50);
    /// let v = u + vec4!(-1, 0, 1, 1);
    /// assert_eq!(u.less_than(v), bvec4!(u.x() < v.x(), u.y() < v.y(), u.z() < v.z(), u.w() < v.w()));
    /// # }
    /// ```
    fn less_than(&self, rhs: &Vec4) -> Vec4b {
        Vec4b::new(self[0] < rhs[0], self[1] < rhs[1], self[2] < rhs[2], self[3] < rhs[3])
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
    /// let u = vec4!(20, 30, 40, 50);
    /// let v = u + vec4!(-1, 0, 1, 1);
    /// assert_eq!(u.less_than_equal(v), bvec4!(u.x() <= v.x(), u.y() <= v.y(), u.z() <= v.z(), u.w() <= v.w()));
    /// # }
    /// ```
    fn less_than_equal(&self, rhs: &Vec4) -> Vec4b {
        Vec4b::new(self[0] <= rhs[0], self[1] <= rhs[1], self[2] <= rhs[2], self[3] <= rhs[3])
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
    /// let u = vec4!(20, 30, 40, 50);
    /// let v = u + vec4!(-1, 0, 1, 1);
    /// assert_eq!(u.greater_than(v), bvec4!(u.x() > v.x(), u.y() > v.y(), u.z() > v.z(), u.w() > v.w()));
    /// # }
    /// ```
    fn greater_than(&self, rhs: &Vec4) -> Vec4b {
        Vec4b::new(self[0] > rhs[0], self[1] > rhs[1], self[2] > rhs[2], self[3] > rhs[3])
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
    /// let u = vec4!(20, 30, 40, 50);
    /// let v = u + vec4!(-1, 0, 1, 1);
    /// assert_eq!(u.greater_than_equals(v), bvec4!(u.x() >= v.x(), u.y() >= v.y(), u.z() >= v.z(), u.w() >= v.w()));
    /// # }
    /// ```
    fn greater_than_equals(&self, rhs: &Vec4) -> Vec4b {
        Vec4b::new(self[0] >= rhs[0], self[1] >= rhs[1], self[2] >= rhs[2], self[3] >= rhs[3])
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
    /// let u = vec4!(20, 30, 40, 50);
    /// let v = u + vec4!(-1, 0, 1, 1);
    /// assert_eq!(u.equal(v), bvec4!(u.x() == v.x(), u.y() == v.y(), u.z() == v.z(), u.w() == v.w()));
    /// # }
    /// ```
    fn equal(&self, rhs: &Vec4) -> Vec4b {
        Vec4b::new(self[0] == rhs[0], self[1] == rhs[1], self[2] == rhs[2], self[3] == rhs[3])
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
    /// let u = vec4!(20, 30, 40, 50);
    /// let v = u + vec4!(-1, 0, 1, 1);
    /// assert_eq!(u.not_equal(v), bvec4!(u.x() != v.x(), u.y() != v.y(), u.z() != v.z(), u.w() != v.w()));
    /// # }
    /// ```
    fn not_equal(&self, rhs: &Vec4) -> Vec4b {
        Vec4b::new(self[0] != rhs[0], self[1] != rhs[1], self[2] != rhs[2], self[3] != rhs[3])
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
    /// let u = vec4!(20, 30, 40, 50);
    /// assert!(u.approx_equal(u + vec4!(1e-9), 1e-8));
    /// assert!(!u.approx_equal(u + vec4!(1e-8), 1e-8));
    /// # }
    /// ```
    fn approx_equal(&self, rhs: &Vec4, eps: f64) -> bool {
    	let eps = Vec4::new(eps, eps, eps, eps);
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
    /// let u = vec4!(20, 30, 40, 50);
    /// let v = vec4!(2, 3, 4, 5);
    /// assert_eq!(u.dot(v), 20.0 * 2.0 + 30.0 * 3.0 + 40.0 * 4.0 + 50.0 * 5.0);
    /// # }
    /// ```
    fn dot(&self, rhs: &Vec4) -> f64 {
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
    /// let u = vec4!(20, 30, 40, 50);
    /// let v = vec4!(2, 3, 4, 5);
    /// let w = u.lerp(v, 0.25);
    /// assert_eq!(w, vec4!(20.0*0.75 + 2.0*0.25, 30.0*0.75 + 3.0*0.25, 40.0*0.75 + 4.0*0.25, 50.0*0.75 + 5.0*0.25));
    /// # }
    /// ```
    fn lerp(&self, rhs: &Vec4, a: f64) -> Vec4 {
    	self*(1.0 - a) + rhs*a
    }
}

impl Vec4Ops<Vec4> for Vec4 {
	/// Shorthand for `lhs.less_than(&rhs)`.
    #[inline(always)] fn less_than(&self, rhs: Vec4) -> Vec4b {
        self.less_than(&rhs)
    }

    /// Shorthand for `lhs.less_than_equal(&rhs)`.
    #[inline(always)] fn less_than_equal(&self, rhs: Vec4) -> Vec4b {
        self.less_than_equal(&rhs)
    }

    /// Shorthand for `lhs.greater_than(&rhs)`.
    #[inline(always)] fn greater_than(&self, rhs: Vec4) -> Vec4b {
        self.greater_than(&rhs)
    }

    /// Shorthand for `lhs.greater_than_equals(&rhs)`.
    #[inline(always)] fn greater_than_equals(&self, rhs: Vec4) -> Vec4b {
        self.greater_than_equals(&rhs)
    }

    /// Shorthand for `lhs.equal(&rhs)`.
    #[inline(always)] fn equal(&self, rhs: Vec4) -> Vec4b {
        self.equal(&rhs)
    }

    /// Shorthand for `lhs.not_equal(&rhs)`.
    #[inline(always)] fn not_equal(&self, rhs: Vec4) -> Vec4b {
        self.not_equal(&rhs)
    }

    /// Shorthand for `lhs.approx_equals(&rhs, eps)`.
    #[inline(always)] fn approx_equal(&self, rhs: Vec4, eps: f64) -> bool {
        self.approx_equal(&rhs, eps)
    }

    /// Shorthand for `lhs.dot(&rhs)`.
    #[inline(always)] fn dot(&self, rhs: Vec4) -> f64 {
        self.dot(&rhs)
    }

    /// Shorthand for `lhs.determinant(&rhs)`.
    #[inline(always)] fn lerp(&self, rhs: Vec4, a: f64) -> Vec4 {
        self.lerp(&rhs, a)
    }
}

impl Display for Vec4 {
    fn fmt(&self, f: &mut Formatter) -> Result {
    	write!(f, "Vec4({}, {}, {}, {})", self[0], self[1], self[2], self[3])
    }
}

impl Index<usize> for Vec4 {
    type Output = f64;
    
    /// Index notation for acessing components of a vector.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let u = vec4!(20, 30, 40, 50);
    /// assert_eq!(u, vec4!(u[0], u[1], u[2], u[3]));
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

impl<'a, 'b> Add<&'b Vec4> for &'a Vec4 {
    type Output = Vec4;

    /// Performs component-wise addition of two vectors, producing a new vector.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let u = vec4!(20, 30, 40, 50) + vec4!(2, 3, 4, 5);
    /// assert_eq!(u, vec4!(20 + 2, 30 + 3, 40 + 4, 50 + 5));
    /// # }
    /// ```
    fn add(self, rhs: &Vec4) -> Vec4 {
        Vec4::new(self[0] + rhs[0], self[1] + rhs[1], self[2] + rhs[2], self[3] + rhs[3])
    }
}

impl<'a> Add<Vec4> for &'a Vec4 {
    type Output = Vec4;
    
    /// Shorthand for `lhs + &rhs`.
    #[inline(always)] fn add(self, rhs: Vec4) -> Vec4 {
        self + &rhs
    }
}

impl<'b> Add<&'b Vec4> for Vec4 {
    type Output = Vec4;
    
    /// Shorthand for `&lhs + rhs`.
    #[inline(always)] fn add(self, rhs: &Vec4) -> Vec4 {
        &self + rhs
    }
}

impl Add<Vec4> for Vec4 {
    type Output = Vec4;
    
    /// Shorthand for `&lhs + &rhs`.
    #[inline(always)] fn add(self, rhs: Vec4) -> Vec4 {
        &self + &rhs
    }
}

impl<'a> Add<f64> for &'a Vec4 {
    type Output = Vec4;
    
    /// Adds a scalar to each component of a vector, producing a new vector.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let u = vec4!(20, 30, 40, 50) + 2.0;
    /// assert_eq!(u, vec4!(20 + 2, 30 + 2, 40 + 2, 50 + 2));
    /// # }
    /// ```
    fn add(self, rhs: f64) -> Vec4 {
        Vec4::new(self[0] + rhs, self[1] + rhs, self[2] + rhs, self[3] + rhs)
    }
}

impl Add<f64> for Vec4 {
    type Output = Vec4;
    
    /// Shorthand for `&lhs + rhs`.
    #[inline(always)] fn add(self, rhs: f64) -> Vec4 {
        &self + rhs
    }
}

impl<'a, 'b> Sub<&'b Vec4> for &'a Vec4 {
    type Output = Vec4;

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
    /// let u = vec4!(20, 30, 40, 50) - vec4!(2, 3, 4, 5);
    /// assert_eq!(u, vec4!(20 - 2, 30 - 3, 40 - 4, 50 - 5));
    /// # }
    /// ```
    fn sub(self, rhs: &Vec4) -> Vec4 {
        Vec4::new(self[0] - rhs[0], self[1] - rhs[1], self[2] - rhs[2], self[3] - rhs[3])
    }
}

impl<'a> Sub<Vec4> for &'a Vec4 {
    type Output = Vec4;
    
    /// Shorthand for `lhs - &rhs`.
    #[inline(always)] fn sub(self, rhs: Vec4) -> Vec4 {
        self - &rhs
    }
}

impl<'b> Sub<&'b Vec4> for Vec4 {
    type Output = Vec4;
    
    /// Shorthand for `&lhs - rhs`.
    #[inline(always)] fn sub(self, rhs: &Vec4) -> Vec4 {
        &self - rhs
    }
}

impl Sub<Vec4> for Vec4 {
    type Output = Vec4;
    
    /// Shorthand for `&lhs - &rhs`.
    #[inline(always)] fn sub(self, rhs: Vec4) -> Vec4 {
        &self - &rhs
    }
}

impl<'a> Sub<f64> for &'a Vec4 {
    type Output = Vec4;
    
    /// Subtracts a scalar from each component of a vector, producing a new vector.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let u = vec4!(20, 30, 40, 50) - 2.0;
    /// assert_eq!(u, vec4!(20 - 2, 30 - 2, 40 - 2, 50 - 2));
    /// # }
    /// ```
    fn sub(self, rhs: f64) -> Vec4 {
        Vec4::new(self[0] - rhs, self[1] - rhs, self[2] - rhs, self[3] - rhs)
    }
}

impl Sub<f64> for Vec4 {
    type Output = Vec4;
    
    /// Shorthand for `&lhs - rhs`.
    #[inline(always)] fn sub(self, rhs: f64) -> Vec4 {
        &self - rhs
    }
}

impl<'a, 'b> Mul<&'b Vec4> for &'a Vec4 {
    type Output = Vec4;

    /// Performs component-wise multiplication of two vectors, producing a new vector.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let u = vec4!(20, 30, 40, 50) * vec4!(2, 3, 4, 5);
    /// assert_eq!(u, vec4!(20 * 2, 30 * 3, 40 * 4, 50 * 5));
    /// # }
    /// ```
    fn mul(self, rhs: &Vec4) -> Vec4 {
        Vec4::new(self[0] * rhs[0], self[1] * rhs[1], self[2] * rhs[2], self[3] * rhs[3])
    }
}

impl<'a> Mul<Vec4> for &'a Vec4 {
    type Output = Vec4;
    
    /// Shorthand for `lhs * &rhs`.
    #[inline(always)] fn mul(self, rhs: Vec4) -> Vec4 {
        self * &rhs
    }
}

impl<'b> Mul<&'b Vec4> for Vec4 {
    type Output = Vec4;
    
    /// Shorthand for `&lhs * rhs`.
    #[inline(always)] fn mul(self, rhs: &Vec4) -> Vec4 {
        &self * rhs
    }
}

impl Mul<Vec4> for Vec4 {
    type Output = Vec4;
    
    /// Shorthand for `&lhs * &rhs`.
    #[inline(always)] fn mul(self, rhs: Vec4) -> Vec4 {
        &self * &rhs
    }
}

impl<'a> Mul<f64> for &'a Vec4 {
    type Output = Vec4;
    
    /// Multiplies each component of a vector by a scalar, producing a new vector.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let u = vec4!(20, 30, 40, 50) * 2.0;
    /// assert_eq!(u, vec4!(20 * 2, 30 * 2, 40 * 2, 50 * 2));
    /// # }
    /// ```
    fn mul(self, rhs: f64) -> Vec4 {
        Vec4::new(self[0] * rhs, self[1] * rhs, self[2] * rhs, self[3] * rhs)
    }
}

impl Mul<f64> for Vec4 {
    type Output = Vec4;
    
    /// Shorthand for `&lhs * rhs`.
    #[inline(always)] fn mul(self, rhs: f64) -> Vec4 {
        &self * rhs
    }
}

impl<'a, 'b> Div<&'b Vec4> for &'a Vec4 {
    type Output = Vec4;

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
    /// let u = vec4!(20, 30, 40, 50) / vec4!(2, 3, 4, 5);
    /// assert_eq!(u, vec4!(20 / 2, 30 / 3, 40 / 4, 50 / 5));
    /// # }
    /// ```
    fn div(self, rhs: &Vec4) -> Vec4 {
        Vec4::new(self[0] / rhs[0], self[1] / rhs[1], self[2] / rhs[2], self[3] / rhs[3])
    }
}

impl<'a> Div<Vec4> for &'a Vec4 {
    type Output = Vec4;
    
    /// Shorthand for `lhs / &rhs`.
    #[inline(always)] fn div(self, rhs: Vec4) -> Vec4 {
        self / &rhs
    }
}

impl<'b> Div<&'b Vec4> for Vec4 {
    type Output = Vec4;
    
    /// Shorthand for `&lhs / rhs`.
    #[inline(always)] fn div(self, rhs: &Vec4) -> Vec4 {
        &self / rhs
    }
}

impl Div<Vec4> for Vec4 {
    type Output = Vec4;
    
    /// Shorthand for `&lhs / &rhs`.
    #[inline(always)] fn div(self, rhs: Vec4) -> Vec4 {
        &self / &rhs
    }
}

impl<'a> Div<f64> for &'a Vec4 {
    type Output = Vec4;
    
    /// Divides each component of a vector by a scalar, producing a new vector.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let u = vec4!(20, 30, 40, 50) / 2.0;
    /// assert_eq!(u, vec4!(20 / 2, 30 / 2, 40 / 2, 50 / 2));
    /// # }
    /// ```
    fn div(self, rhs: f64) -> Vec4 {
        Vec4::new(self[0] / rhs, self[1] / rhs, self[2] / rhs, self[3] / rhs)
    }
}

impl Div<f64> for Vec4 {
    type Output = Vec4;
    
    /// Shorthand for `&lhs / rhs`.
    #[inline(always)] fn div(self, rhs: f64) -> Vec4 {
        &self / rhs
    }
}

impl<'a> Neg for &'a Vec4 {
    type Output = Vec4;
    
    /// Applies negation to each component of a vector, producing a new vector.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let u = vec4!(20, 30, 40, 50);
    /// assert_eq!(-u, vec4!(-20, -30, -40, -50));
    /// # }
    /// ```
    fn neg(self) -> Vec4 {
        Vec4::new(-self[0], -self[1], -self[2], -self[3])
    }
}

impl Neg for Vec4 {
    type Output = Vec4;
    
    /// Shorthand for `-&arg`.
    #[inline(always)] fn neg(self) -> Vec4 {
        -&self
    }
}

/// Builder macro for creating new 4-dimensional vectors with floating point components.
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
/// let u = vec4!(2.0);
/// assert_eq!(u, Vec4::new(2.0, 2.0, 2.0, 2.0));
/// # }
/// ```
/// 
/// Create a new vector with `x = 2`, `y = 3`, `z = 4`, and `w = 5`:
///
/// ```
/// #[macro_use] extern crate vexyz_math;
/// use vexyz_math::*;
///
/// # fn main() {
/// let u = vec4!(2.0, 3.0, 4.0, 5.0);
/// assert_eq!(u, Vec4::new(2.0, 3.0, 4.0, 5.0));
/// # }
/// ```
#[macro_export]
macro_rules! vec4 {
    ($s:expr) => {{
        let s = $s as f64;
        Vec4::new(s, s, s, s)
    }};
    ($x:expr, $y:expr, $z:expr, $w:expr) => {{
        Vec4::new($x as f64, $y as f64, $z as f64, $w as f64)
    }};
    ($x:expr, $y:expr, $z:expr, $w:expr,) => {{
        Vec4::new($x as f64, $y as f64, $z as f64, $w as f64)
    }};
}
