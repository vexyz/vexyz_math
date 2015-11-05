// Generated code.
use std::fmt::{Display, Formatter, Result};
use std::ops::*;
use Vec4b;

/// 4-dimensional vector with integer `x`, `y`, `z`, and `w` components.
///
/// Vectors can also represent colors with the help of
/// `r()`, `g()`, `b()`, and `a()` accessors.
///
/// Most operators and methods on vectors are performed in component-wise fashion. The notable
/// exception is vector-matrix multiplication.
#[derive(Debug, PartialEq, Copy, Clone)]
pub struct Vec4i { data: [i32; 4] }

impl Vec4i {
    /// Constructs a new `Vec4i`.
    pub fn new(x: i32, y: i32, z: i32, w: i32) -> Self {
         Vec4i { data: [x, y, z, w] }
    }
    
    /// Component accessor, returns the 1st component of the vector.
    #[inline(always)] pub fn x(&self) -> i32 { self[0] }

    /// Component accessor, returns the 2nd component of the vector.
    #[inline(always)] pub fn y(&self) -> i32 { self[1] }

    /// Component accessor, returns the 3rd component of the vector.
    #[inline(always)] pub fn z(&self) -> i32 { self[2] }

    /// Component accessor, returns the 4th component of the vector.
    #[inline(always)] pub fn w(&self) -> i32 { self[3] }
    
    
    /// Color-style component accessor, returns the 1st component of the vector.
    #[inline(always)] pub fn r(&self) -> i32 { self[0] }

    /// Color-style component accessor, returns the 2nd component of the vector.
    #[inline(always)] pub fn g(&self) -> i32 { self[1] }

    /// Color-style component accessor, returns the 3rd component of the vector.
    #[inline(always)] pub fn b(&self) -> i32 { self[2] }

    /// Color-style component accessor, returns the 4th component of the vector.
    #[inline(always)] pub fn a(&self) -> i32 { self[3] }
    
    /// Returns the sum of vector components.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let u = ivec4!(20, 30, 40, 50);
    /// assert_eq!(u.sum(), 20 + 30 + 40 + 50);
    /// # }
    /// ```
    pub fn sum(&self) -> i32 {
        self.x() + self.y() + self.z() + self.w()
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
    /// let u = ivec4!(20, -30, 40, -50);
    /// assert_eq!(u.abs(), ivec4!(20, 30, 40, 50));
    /// # }
    /// ```
    pub fn abs(&self) -> Vec4i {
        Vec4i::new(self.x().abs(), self.y().abs(), self.z().abs(), self.w().abs())
    }
}

pub trait Vec4iOps<Rhs> {
    fn less_than(&self, rhs: Rhs) -> Vec4b;

    fn less_than_equal(&self, rhs: Rhs) -> Vec4b;

    fn greater_than(&self, rhs: Rhs) -> Vec4b;

    fn greater_than_equals(&self, rhs: Rhs) -> Vec4b;

    fn equal(&self, rhs: Rhs) -> Vec4b;

    fn not_equal(&self, rhs: Rhs) -> Vec4b;

    fn dot(&self, rhs: Rhs) -> i32;
}

impl<'a> Vec4iOps<&'a Vec4i> for Vec4i {
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
    /// let u = ivec4!(20, 30, 40, 50);
    /// let v = u + ivec4!(-1, 0, 1, 1);
    /// assert_eq!(u.less_than(v), bvec4!(u.x() < v.x(), u.y() < v.y(), u.z() < v.z(), u.w() < v.w()));
    /// # }
    /// ```
    fn less_than(&self, rhs: &Vec4i) -> Vec4b {
        Vec4b::new(self.x() < rhs.x(), self.y() < rhs.y(), self.z() < rhs.z(), self.w() < rhs.w())
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
    /// let u = ivec4!(20, 30, 40, 50);
    /// let v = u + ivec4!(-1, 0, 1, 1);
    /// assert_eq!(u.less_than_equal(v), bvec4!(u.x() <= v.x(), u.y() <= v.y(), u.z() <= v.z(), u.w() <= v.w()));
    /// # }
    /// ```
    fn less_than_equal(&self, rhs: &Vec4i) -> Vec4b {
        Vec4b::new(self.x() <= rhs.x(), self.y() <= rhs.y(), self.z() <= rhs.z(), self.w() <= rhs.w())
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
    /// let u = ivec4!(20, 30, 40, 50);
    /// let v = u + ivec4!(-1, 0, 1, 1);
    /// assert_eq!(u.greater_than(v), bvec4!(u.x() > v.x(), u.y() > v.y(), u.z() > v.z(), u.w() > v.w()));
    /// # }
    /// ```
    fn greater_than(&self, rhs: &Vec4i) -> Vec4b {
        Vec4b::new(self.x() > rhs.x(), self.y() > rhs.y(), self.z() > rhs.z(), self.w() > rhs.w())
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
    /// let u = ivec4!(20, 30, 40, 50);
    /// let v = u + ivec4!(-1, 0, 1, 1);
    /// assert_eq!(u.greater_than_equals(v), bvec4!(u.x() >= v.x(), u.y() >= v.y(), u.z() >= v.z(), u.w() >= v.w()));
    /// # }
    /// ```
    fn greater_than_equals(&self, rhs: &Vec4i) -> Vec4b {
        Vec4b::new(self.x() >= rhs.x(), self.y() >= rhs.y(), self.z() >= rhs.z(), self.w() >= rhs.w())
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
    /// let u = ivec4!(20, 30, 40, 50);
    /// let v = u + ivec4!(-1, 0, 1, 1);
    /// assert_eq!(u.equal(v), bvec4!(u.x() == v.x(), u.y() == v.y(), u.z() == v.z(), u.w() == v.w()));
    /// # }
    /// ```
    fn equal(&self, rhs: &Vec4i) -> Vec4b {
        Vec4b::new(self.x() == rhs.x(), self.y() == rhs.y(), self.z() == rhs.z(), self.w() == rhs.w())
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
    /// let u = ivec4!(20, 30, 40, 50);
    /// let v = u + ivec4!(-1, 0, 1, 1);
    /// assert_eq!(u.not_equal(v), bvec4!(u.x() != v.x(), u.y() != v.y(), u.z() != v.z(), u.w() != v.w()));
    /// # }
    /// ```
    fn not_equal(&self, rhs: &Vec4i) -> Vec4b {
        Vec4b::new(self.x() != rhs.x(), self.y() != rhs.y(), self.z() != rhs.z(), self.w() != rhs.w())
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
    /// let u = ivec4!(20, 30, 40, 50);
    /// let v = ivec4!(2, 3, 4, 5);
    /// assert_eq!(u.dot(v), 20 * 2 + 30 * 3 + 40 * 4 + 50 * 5);
    /// # }
    /// ```
    fn dot(&self, rhs: &Vec4i) -> i32 {
        (self * rhs).sum()
    }
}

impl Vec4iOps<Vec4i> for Vec4i {
    /// Shorthand for `lhs.less_than(&rhs)`.
    #[inline(always)] fn less_than(&self, rhs: Vec4i) -> Vec4b {
        self.less_than(&rhs)
    }

    /// Shorthand for `lhs.less_than_equal(&rhs)`.
    #[inline(always)] fn less_than_equal(&self, rhs: Vec4i) -> Vec4b {
        self.less_than_equal(&rhs)
    }

    /// Shorthand for `lhs.greater_than(&rhs)`.
    #[inline(always)] fn greater_than(&self, rhs: Vec4i) -> Vec4b {
        self.greater_than(&rhs)
    }

    /// Shorthand for `lhs.greater_than_equals(&rhs)`.
    #[inline(always)] fn greater_than_equals(&self, rhs: Vec4i) -> Vec4b {
        self.greater_than_equals(&rhs)
    }

    /// Shorthand for `lhs.equal(&rhs)`.
    #[inline(always)] fn equal(&self, rhs: Vec4i) -> Vec4b {
        self.equal(&rhs)
    }

    /// Shorthand for `lhs.not_equal(&rhs)`.
    #[inline(always)] fn not_equal(&self, rhs: Vec4i) -> Vec4b {
        self.not_equal(&rhs)
    }

    /// Shorthand for `lhs.dot(&rhs)`.
    #[inline(always)] fn dot(&self, rhs: Vec4i) -> i32 {
        self.dot(&rhs)
    }
}

impl Display for Vec4i {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "Vec4i({}, {}, {}, {})", self[0], self[1], self[2], self[3])
    }
}

impl Index<usize> for Vec4i {
    type Output = i32;
    
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
    /// let u = ivec4!(20, 30, 40, 50);
    /// assert_eq!(u, ivec4!(u[0], u[1], u[2], u[3]));
    /// # }
    /// ```
    ///
    /// # Panics
    ///
    /// Will panic if the index is greater than 3.
    #[inline(always)] fn index<'a>(&'a self, i: usize) -> &'a i32 {
        &self.data[i]
    }
}

impl IndexMut<usize> for Vec4i {

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
    /// let mut u = ivec4!(20, 30, 40, 50);
    /// u[0] = 2; u[1] = 3; u[2] = 4; u[3] = 5;
    /// assert_eq!(u, ivec4!(2, 3, 4, 5));
    /// # }
    /// ```
    ///
    /// # Panics
    ///
    /// Will panic if the index is greater than 3.
    #[inline(always)] fn index_mut<'a>(&'a mut self, i: usize) -> &'a mut i32 {
        &mut self.data[i]
    }
}

impl<'a, 'b> Add<&'b Vec4i> for &'a Vec4i {
    type Output = Vec4i;

    /// Performs component-wise addition of two vectors, producing a new vector.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let u = ivec4!(20, 30, 40, 50) + ivec4!(2, 3, 4, 5);
    /// assert_eq!(u, ivec4!(20 + 2, 30 + 3, 40 + 4, 50 + 5));
    /// # }
    /// ```
    fn add(self, rhs: &Vec4i) -> Vec4i {
        Vec4i::new(self.x() + rhs.x(), self.y() + rhs.y(), self.z() + rhs.z(), self.w() + rhs.w())
    }
}

impl<'a> Add<Vec4i> for &'a Vec4i {
    type Output = Vec4i;
    
    /// Shorthand for `lhs + &rhs`.
    #[inline(always)] fn add(self, rhs: Vec4i) -> Vec4i {
        self + &rhs
    }
}

impl<'b> Add<&'b Vec4i> for Vec4i {
    type Output = Vec4i;
    
    /// Shorthand for `&lhs + rhs`.
    #[inline(always)] fn add(self, rhs: &Vec4i) -> Vec4i {
        &self + rhs
    }
}

impl Add<Vec4i> for Vec4i {
    type Output = Vec4i;
    
    /// Shorthand for `&lhs + &rhs`.
    #[inline(always)] fn add(self, rhs: Vec4i) -> Vec4i {
        &self + &rhs
    }
}

impl<'a> Add<i32> for &'a Vec4i {
    type Output = Vec4i;
    
    /// Adds a scalar to each component of a vector, producing a new vector.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let u = ivec4!(20, 30, 40, 50) + 2;
    /// assert_eq!(u, ivec4!(20 + 2, 30 + 2, 40 + 2, 50 + 2));
    /// # }
    /// ```
    fn add(self, rhs: i32) -> Vec4i {
        Vec4i::new(self.x() + rhs, self.y() + rhs, self.z() + rhs, self.w() + rhs)
    }
}

impl Add<i32> for Vec4i {
    type Output = Vec4i;
    
    /// Shorthand for `&lhs + rhs`.
    #[inline(always)] fn add(self, rhs: i32) -> Vec4i {
        &self + rhs
    }
}

impl<'a, 'b> Sub<&'b Vec4i> for &'a Vec4i {
    type Output = Vec4i;

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
    /// let u = ivec4!(20, 30, 40, 50) - ivec4!(2, 3, 4, 5);
    /// assert_eq!(u, ivec4!(20 - 2, 30 - 3, 40 - 4, 50 - 5));
    /// # }
    /// ```
    fn sub(self, rhs: &Vec4i) -> Vec4i {
        Vec4i::new(self.x() - rhs.x(), self.y() - rhs.y(), self.z() - rhs.z(), self.w() - rhs.w())
    }
}

impl<'a> Sub<Vec4i> for &'a Vec4i {
    type Output = Vec4i;
    
    /// Shorthand for `lhs - &rhs`.
    #[inline(always)] fn sub(self, rhs: Vec4i) -> Vec4i {
        self - &rhs
    }
}

impl<'b> Sub<&'b Vec4i> for Vec4i {
    type Output = Vec4i;
    
    /// Shorthand for `&lhs - rhs`.
    #[inline(always)] fn sub(self, rhs: &Vec4i) -> Vec4i {
        &self - rhs
    }
}

impl Sub<Vec4i> for Vec4i {
    type Output = Vec4i;
    
    /// Shorthand for `&lhs - &rhs`.
    #[inline(always)] fn sub(self, rhs: Vec4i) -> Vec4i {
        &self - &rhs
    }
}

impl<'a> Sub<i32> for &'a Vec4i {
    type Output = Vec4i;
    
    /// Subtracts a scalar from each component of a vector, producing a new vector.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let u = ivec4!(20, 30, 40, 50) - 2;
    /// assert_eq!(u, ivec4!(20 - 2, 30 - 2, 40 - 2, 50 - 2));
    /// # }
    /// ```
    fn sub(self, rhs: i32) -> Vec4i {
        Vec4i::new(self.x() - rhs, self.y() - rhs, self.z() - rhs, self.w() - rhs)
    }
}

impl Sub<i32> for Vec4i {
    type Output = Vec4i;
    
    /// Shorthand for `&lhs - rhs`.
    #[inline(always)] fn sub(self, rhs: i32) -> Vec4i {
        &self - rhs
    }
}

impl<'a, 'b> Mul<&'b Vec4i> for &'a Vec4i {
    type Output = Vec4i;

    /// Performs component-wise multiplication of two vectors, producing a new vector.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let u = ivec4!(20, 30, 40, 50) * ivec4!(2, 3, 4, 5);
    /// assert_eq!(u, ivec4!(20 * 2, 30 * 3, 40 * 4, 50 * 5));
    /// # }
    /// ```
    fn mul(self, rhs: &Vec4i) -> Vec4i {
        Vec4i::new(self.x() * rhs.x(), self.y() * rhs.y(), self.z() * rhs.z(), self.w() * rhs.w())
    }
}

impl<'a> Mul<Vec4i> for &'a Vec4i {
    type Output = Vec4i;
    
    /// Shorthand for `lhs * &rhs`.
    #[inline(always)] fn mul(self, rhs: Vec4i) -> Vec4i {
        self * &rhs
    }
}

impl<'b> Mul<&'b Vec4i> for Vec4i {
    type Output = Vec4i;
    
    /// Shorthand for `&lhs * rhs`.
    #[inline(always)] fn mul(self, rhs: &Vec4i) -> Vec4i {
        &self * rhs
    }
}

impl Mul<Vec4i> for Vec4i {
    type Output = Vec4i;
    
    /// Shorthand for `&lhs * &rhs`.
    #[inline(always)] fn mul(self, rhs: Vec4i) -> Vec4i {
        &self * &rhs
    }
}

impl<'a> Mul<i32> for &'a Vec4i {
    type Output = Vec4i;
    
    /// Multiplies each component of a vector by a scalar, producing a new vector.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let u = ivec4!(20, 30, 40, 50) * 2;
    /// assert_eq!(u, ivec4!(20 * 2, 30 * 2, 40 * 2, 50 * 2));
    /// # }
    /// ```
    fn mul(self, rhs: i32) -> Vec4i {
        Vec4i::new(self.x() * rhs, self.y() * rhs, self.z() * rhs, self.w() * rhs)
    }
}

impl Mul<i32> for Vec4i {
    type Output = Vec4i;
    
    /// Shorthand for `&lhs * rhs`.
    #[inline(always)] fn mul(self, rhs: i32) -> Vec4i {
        &self * rhs
    }
}

impl<'a, 'b> Div<&'b Vec4i> for &'a Vec4i {
    type Output = Vec4i;

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
    /// let u = ivec4!(20, 30, 40, 50) / ivec4!(2, 3, 4, 5);
    /// assert_eq!(u, ivec4!(20 / 2, 30 / 3, 40 / 4, 50 / 5));
    /// # }
    /// ```
    fn div(self, rhs: &Vec4i) -> Vec4i {
        Vec4i::new(self.x() / rhs.x(), self.y() / rhs.y(), self.z() / rhs.z(), self.w() / rhs.w())
    }
}

impl<'a> Div<Vec4i> for &'a Vec4i {
    type Output = Vec4i;
    
    /// Shorthand for `lhs / &rhs`.
    #[inline(always)] fn div(self, rhs: Vec4i) -> Vec4i {
        self / &rhs
    }
}

impl<'b> Div<&'b Vec4i> for Vec4i {
    type Output = Vec4i;
    
    /// Shorthand for `&lhs / rhs`.
    #[inline(always)] fn div(self, rhs: &Vec4i) -> Vec4i {
        &self / rhs
    }
}

impl Div<Vec4i> for Vec4i {
    type Output = Vec4i;
    
    /// Shorthand for `&lhs / &rhs`.
    #[inline(always)] fn div(self, rhs: Vec4i) -> Vec4i {
        &self / &rhs
    }
}

impl<'a> Div<i32> for &'a Vec4i {
    type Output = Vec4i;
    
    /// Divides each component of a vector by a scalar, producing a new vector.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let u = ivec4!(20, 30, 40, 50) / 2;
    /// assert_eq!(u, ivec4!(20 / 2, 30 / 2, 40 / 2, 50 / 2));
    /// # }
    /// ```
    fn div(self, rhs: i32) -> Vec4i {
        Vec4i::new(self.x() / rhs, self.y() / rhs, self.z() / rhs, self.w() / rhs)
    }
}

impl Div<i32> for Vec4i {
    type Output = Vec4i;
    
    /// Shorthand for `&lhs / rhs`.
    #[inline(always)] fn div(self, rhs: i32) -> Vec4i {
        &self / rhs
    }
}

impl<'a> Neg for &'a Vec4i {
    type Output = Vec4i;
    
    /// Applies negation to each component of a vector, producing a new vector.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let u = ivec4!(20, 30, 40, 50);
    /// assert_eq!(-u, ivec4!(-20, -30, -40, -50));
    /// # }
    /// ```
    fn neg(self) -> Vec4i {
        Vec4i::new(-self.x(), -self.y(), -self.z(), -self.w())
    }
}

impl Neg for Vec4i {
    type Output = Vec4i;
    
    /// Shorthand for `-&arg`.
    #[inline(always)] fn neg(self) -> Vec4i {
        -&self
    }
}

/// Builder macro for creating new 4-dimensional vectors with integer components.
///
/// # Examples
///
/// Create a new vector with all components set to `2`:
///
/// ```
/// #[macro_use] extern crate vexyz_math;
/// use vexyz_math::*;
///
/// # fn main() {
/// let u = ivec4!(2);
/// assert_eq!(u, Vec4i::new(2, 2, 2, 2));
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
/// let u = ivec4!(2, 3, 4, 5);
/// assert_eq!(u, Vec4i::new(2, 3, 4, 5));
/// # }
/// ```
#[macro_export]
macro_rules! ivec4 {
    ($s:expr) => {{
        let s = $s as i32;
        Vec4i::new(s, s, s, s)
    }};
    ($x:expr, $y:expr, $z:expr, $w:expr) => {{
        Vec4i::new($x as i32, $y as i32, $z as i32, $w as i32)
    }};
    ($x:expr, $y:expr, $z:expr, $w:expr,) => {{
        Vec4i::new($x as i32, $y as i32, $z as i32, $w as i32)
    }};
}
