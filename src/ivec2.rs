// Generated code.
use std::fmt::{Display, Formatter, Result};
use std::ops::*;
use Vec2b;

/// 2-dimensional vector with integer `x`, and `y` components.
///
/// Vectors can also represent colors with the help of
/// `r()`, and `g()` accessors.
///
/// Most operators and methods on vectors are performed in component-wise fashion. The notable
/// exception is vector-matrix multiplication.
#[derive(Debug, PartialEq, Copy, Clone)]
pub struct Vec2i { data: [i32; 2] }

impl Vec2i {
    /// Constructs a new `Vec2i`.
    pub fn new(x: i32, y: i32) -> Self {
         Vec2i { data: [x, y] }
    }
    
    /// Component accessor, returns the 1st component of the vector.
    #[inline(always)] pub fn x(&self) -> i32 { self[0] }

    /// Component accessor, returns the 2nd component of the vector.
    #[inline(always)] pub fn y(&self) -> i32 { self[1] }
    
	
    /// Color-style component accessor, returns the 1st component of the vector.
    #[inline(always)] pub fn r(&self) -> i32 { self[0] }

    /// Color-style component accessor, returns the 2nd component of the vector.
    #[inline(always)] pub fn g(&self) -> i32 { self[1] }
	
	/// Returns the sum of vector components.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let u = ivec2!(20, 30);
    /// assert_eq!(u.sum(), 20 + 30);
    /// # }
    /// ```
    pub fn sum(&self) -> i32 {
        self[0] + self[1]
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
    /// let u = ivec2!(20, -30);
    /// assert_eq!(u.abs(), ivec2!(20, 30));
    /// # }
    /// ```
    pub fn abs(&self) -> Vec2i {
        Vec2i::new(self[0].abs(), self[1].abs())
    }
}

pub trait Vec2iOps<Rhs> {
    fn less_than(&self, rhs: Rhs) -> Vec2b;

    fn less_than_equal(&self, rhs: Rhs) -> Vec2b;

    fn greater_than(&self, rhs: Rhs) -> Vec2b;

    fn greater_than_equals(&self, rhs: Rhs) -> Vec2b;

    fn equal(&self, rhs: Rhs) -> Vec2b;

    fn not_equal(&self, rhs: Rhs) -> Vec2b;

    fn dot(&self, rhs: Rhs) -> i32;
}

impl<'a> Vec2iOps<&'a Vec2i> for Vec2i {
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
    /// let u = ivec2!(20, 30);
    /// let v = u + ivec2!(-1, 0);
    /// assert_eq!(u.less_than(v), bvec2!(u.x() < v.x(), u.y() < v.y()));
    /// # }
    /// ```
    fn less_than(&self, rhs: &Vec2i) -> Vec2b {
        Vec2b::new(self[0] < rhs[0], self[1] < rhs[1])
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
    /// let u = ivec2!(20, 30);
    /// let v = u + ivec2!(-1, 0);
    /// assert_eq!(u.less_than_equal(v), bvec2!(u.x() <= v.x(), u.y() <= v.y()));
    /// # }
    /// ```
    fn less_than_equal(&self, rhs: &Vec2i) -> Vec2b {
        Vec2b::new(self[0] <= rhs[0], self[1] <= rhs[1])
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
    /// let u = ivec2!(20, 30);
    /// let v = u + ivec2!(-1, 0);
    /// assert_eq!(u.greater_than(v), bvec2!(u.x() > v.x(), u.y() > v.y()));
    /// # }
    /// ```
    fn greater_than(&self, rhs: &Vec2i) -> Vec2b {
        Vec2b::new(self[0] > rhs[0], self[1] > rhs[1])
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
    /// let u = ivec2!(20, 30);
    /// let v = u + ivec2!(-1, 0);
    /// assert_eq!(u.greater_than_equals(v), bvec2!(u.x() >= v.x(), u.y() >= v.y()));
    /// # }
    /// ```
    fn greater_than_equals(&self, rhs: &Vec2i) -> Vec2b {
        Vec2b::new(self[0] >= rhs[0], self[1] >= rhs[1])
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
    /// let u = ivec2!(20, 30);
    /// let v = u + ivec2!(-1, 0);
    /// assert_eq!(u.equal(v), bvec2!(u.x() == v.x(), u.y() == v.y()));
    /// # }
    /// ```
    fn equal(&self, rhs: &Vec2i) -> Vec2b {
        Vec2b::new(self[0] == rhs[0], self[1] == rhs[1])
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
    /// let u = ivec2!(20, 30);
    /// let v = u + ivec2!(-1, 0);
    /// assert_eq!(u.not_equal(v), bvec2!(u.x() != v.x(), u.y() != v.y()));
    /// # }
    /// ```
    fn not_equal(&self, rhs: &Vec2i) -> Vec2b {
        Vec2b::new(self[0] != rhs[0], self[1] != rhs[1])
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
    /// let u = ivec2!(20, 30);
    /// let v = ivec2!(2, 3);
    /// assert_eq!(u.dot(v), 20 * 2 + 30 * 3);
    /// # }
    /// ```
    fn dot(&self, rhs: &Vec2i) -> i32 {
        (self * rhs).sum()
    }
}

impl Vec2iOps<Vec2i> for Vec2i {
	/// Shorthand for `lhs.less_than(&rhs)`.
    #[inline(always)] fn less_than(&self, rhs: Vec2i) -> Vec2b {
        self.less_than(&rhs)
    }

    /// Shorthand for `lhs.less_than_equal(&rhs)`.
    #[inline(always)] fn less_than_equal(&self, rhs: Vec2i) -> Vec2b {
        self.less_than_equal(&rhs)
    }

    /// Shorthand for `lhs.greater_than(&rhs)`.
    #[inline(always)] fn greater_than(&self, rhs: Vec2i) -> Vec2b {
        self.greater_than(&rhs)
    }

    /// Shorthand for `lhs.greater_than_equals(&rhs)`.
    #[inline(always)] fn greater_than_equals(&self, rhs: Vec2i) -> Vec2b {
        self.greater_than_equals(&rhs)
    }

    /// Shorthand for `lhs.equal(&rhs)`.
    #[inline(always)] fn equal(&self, rhs: Vec2i) -> Vec2b {
        self.equal(&rhs)
    }

    /// Shorthand for `lhs.not_equal(&rhs)`.
    #[inline(always)] fn not_equal(&self, rhs: Vec2i) -> Vec2b {
        self.not_equal(&rhs)
    }

    /// Shorthand for `lhs.dot(&rhs)`.
    #[inline(always)] fn dot(&self, rhs: Vec2i) -> i32 {
        self.dot(&rhs)
    }
}

impl Display for Vec2i {
    fn fmt(&self, f: &mut Formatter) -> Result {
    	write!(f, "Vec2i({}, {})", self[0], self[1])
    }
}

impl Index<usize> for Vec2i {
    type Output = i32;
    
    /// Index notation for acessing components of a vector.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let u = ivec2!(20, 30);
    /// assert_eq!(u, ivec2!(u[0], u[1]));
    /// # }
    /// ```
    ///
    /// # Panics
    ///
    /// Will panic if the index is greater than 1.
    #[inline(always)] fn index<'a>(&'a self, i: usize) -> &'a i32 {
        &self.data[i]
    }
}

impl<'a, 'b> Add<&'b Vec2i> for &'a Vec2i {
    type Output = Vec2i;

    /// Performs component-wise addition of two vectors, producing a new vector.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let u = ivec2!(20, 30) + ivec2!(2, 3);
    /// assert_eq!(u, ivec2!(20 + 2, 30 + 3));
    /// # }
    /// ```
    fn add(self, rhs: &Vec2i) -> Vec2i {
        Vec2i::new(self[0] + rhs[0], self[1] + rhs[1])
    }
}

impl<'a> Add<Vec2i> for &'a Vec2i {
    type Output = Vec2i;
    
    /// Shorthand for `lhs + &rhs`.
    #[inline(always)] fn add(self, rhs: Vec2i) -> Vec2i {
        self + &rhs
    }
}

impl<'b> Add<&'b Vec2i> for Vec2i {
    type Output = Vec2i;
    
    /// Shorthand for `&lhs + rhs`.
    #[inline(always)] fn add(self, rhs: &Vec2i) -> Vec2i {
        &self + rhs
    }
}

impl Add<Vec2i> for Vec2i {
    type Output = Vec2i;
    
    /// Shorthand for `&lhs + &rhs`.
    #[inline(always)] fn add(self, rhs: Vec2i) -> Vec2i {
        &self + &rhs
    }
}

impl<'a> Add<i32> for &'a Vec2i {
    type Output = Vec2i;
    
    /// Adds a scalar to each component of a vector, producing a new vector.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let u = ivec2!(20, 30) + 2;
    /// assert_eq!(u, ivec2!(20 + 2, 30 + 2));
    /// # }
    /// ```
    fn add(self, rhs: i32) -> Vec2i {
        Vec2i::new(self[0] + rhs, self[1] + rhs)
    }
}

impl Add<i32> for Vec2i {
    type Output = Vec2i;
    
    /// Shorthand for `&lhs + rhs`.
    #[inline(always)] fn add(self, rhs: i32) -> Vec2i {
        &self + rhs
    }
}

impl<'a, 'b> Sub<&'b Vec2i> for &'a Vec2i {
    type Output = Vec2i;

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
    /// let u = ivec2!(20, 30) - ivec2!(2, 3);
    /// assert_eq!(u, ivec2!(20 - 2, 30 - 3));
    /// # }
    /// ```
    fn sub(self, rhs: &Vec2i) -> Vec2i {
        Vec2i::new(self[0] - rhs[0], self[1] - rhs[1])
    }
}

impl<'a> Sub<Vec2i> for &'a Vec2i {
    type Output = Vec2i;
    
    /// Shorthand for `lhs - &rhs`.
    #[inline(always)] fn sub(self, rhs: Vec2i) -> Vec2i {
        self - &rhs
    }
}

impl<'b> Sub<&'b Vec2i> for Vec2i {
    type Output = Vec2i;
    
    /// Shorthand for `&lhs - rhs`.
    #[inline(always)] fn sub(self, rhs: &Vec2i) -> Vec2i {
        &self - rhs
    }
}

impl Sub<Vec2i> for Vec2i {
    type Output = Vec2i;
    
    /// Shorthand for `&lhs - &rhs`.
    #[inline(always)] fn sub(self, rhs: Vec2i) -> Vec2i {
        &self - &rhs
    }
}

impl<'a> Sub<i32> for &'a Vec2i {
    type Output = Vec2i;
    
    /// Subtracts a scalar from each component of a vector, producing a new vector.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let u = ivec2!(20, 30) - 2;
    /// assert_eq!(u, ivec2!(20 - 2, 30 - 2));
    /// # }
    /// ```
    fn sub(self, rhs: i32) -> Vec2i {
        Vec2i::new(self[0] - rhs, self[1] - rhs)
    }
}

impl Sub<i32> for Vec2i {
    type Output = Vec2i;
    
    /// Shorthand for `&lhs - rhs`.
    #[inline(always)] fn sub(self, rhs: i32) -> Vec2i {
        &self - rhs
    }
}

impl<'a, 'b> Mul<&'b Vec2i> for &'a Vec2i {
    type Output = Vec2i;

    /// Performs component-wise multiplication of two vectors, producing a new vector.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let u = ivec2!(20, 30) * ivec2!(2, 3);
    /// assert_eq!(u, ivec2!(20 * 2, 30 * 3));
    /// # }
    /// ```
    fn mul(self, rhs: &Vec2i) -> Vec2i {
        Vec2i::new(self[0] * rhs[0], self[1] * rhs[1])
    }
}

impl<'a> Mul<Vec2i> for &'a Vec2i {
    type Output = Vec2i;
    
    /// Shorthand for `lhs * &rhs`.
    #[inline(always)] fn mul(self, rhs: Vec2i) -> Vec2i {
        self * &rhs
    }
}

impl<'b> Mul<&'b Vec2i> for Vec2i {
    type Output = Vec2i;
    
    /// Shorthand for `&lhs * rhs`.
    #[inline(always)] fn mul(self, rhs: &Vec2i) -> Vec2i {
        &self * rhs
    }
}

impl Mul<Vec2i> for Vec2i {
    type Output = Vec2i;
    
    /// Shorthand for `&lhs * &rhs`.
    #[inline(always)] fn mul(self, rhs: Vec2i) -> Vec2i {
        &self * &rhs
    }
}

impl<'a> Mul<i32> for &'a Vec2i {
    type Output = Vec2i;
    
    /// Multiplies each component of a vector by a scalar, producing a new vector.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let u = ivec2!(20, 30) * 2;
    /// assert_eq!(u, ivec2!(20 * 2, 30 * 2));
    /// # }
    /// ```
    fn mul(self, rhs: i32) -> Vec2i {
        Vec2i::new(self[0] * rhs, self[1] * rhs)
    }
}

impl Mul<i32> for Vec2i {
    type Output = Vec2i;
    
    /// Shorthand for `&lhs * rhs`.
    #[inline(always)] fn mul(self, rhs: i32) -> Vec2i {
        &self * rhs
    }
}

impl<'a, 'b> Div<&'b Vec2i> for &'a Vec2i {
    type Output = Vec2i;

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
    /// let u = ivec2!(20, 30) / ivec2!(2, 3);
    /// assert_eq!(u, ivec2!(20 / 2, 30 / 3));
    /// # }
    /// ```
    fn div(self, rhs: &Vec2i) -> Vec2i {
        Vec2i::new(self[0] / rhs[0], self[1] / rhs[1])
    }
}

impl<'a> Div<Vec2i> for &'a Vec2i {
    type Output = Vec2i;
    
    /// Shorthand for `lhs / &rhs`.
    #[inline(always)] fn div(self, rhs: Vec2i) -> Vec2i {
        self / &rhs
    }
}

impl<'b> Div<&'b Vec2i> for Vec2i {
    type Output = Vec2i;
    
    /// Shorthand for `&lhs / rhs`.
    #[inline(always)] fn div(self, rhs: &Vec2i) -> Vec2i {
        &self / rhs
    }
}

impl Div<Vec2i> for Vec2i {
    type Output = Vec2i;
    
    /// Shorthand for `&lhs / &rhs`.
    #[inline(always)] fn div(self, rhs: Vec2i) -> Vec2i {
        &self / &rhs
    }
}

impl<'a> Div<i32> for &'a Vec2i {
    type Output = Vec2i;
    
    /// Divides each component of a vector by a scalar, producing a new vector.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let u = ivec2!(20, 30) / 2;
    /// assert_eq!(u, ivec2!(20 / 2, 30 / 2));
    /// # }
    /// ```
    fn div(self, rhs: i32) -> Vec2i {
        Vec2i::new(self[0] / rhs, self[1] / rhs)
    }
}

impl Div<i32> for Vec2i {
    type Output = Vec2i;
    
    /// Shorthand for `&lhs / rhs`.
    #[inline(always)] fn div(self, rhs: i32) -> Vec2i {
        &self / rhs
    }
}

impl<'a> Neg for &'a Vec2i {
    type Output = Vec2i;
    
    /// Applies negation to each component of a vector, producing a new vector.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let u = ivec2!(20, 30);
    /// assert_eq!(-u, ivec2!(-20, -30));
    /// # }
    /// ```
    fn neg(self) -> Vec2i {
        Vec2i::new(-self[0], -self[1])
    }
}

impl Neg for Vec2i {
    type Output = Vec2i;
    
    /// Shorthand for `-&arg`.
    #[inline(always)] fn neg(self) -> Vec2i {
        -&self
    }
}

/// Builder macro for creating new 2-dimensional vectors with integer components.
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
/// let u = ivec2!(2);
/// assert_eq!(u, Vec2i::new(2, 2));
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
/// let u = ivec2!(2, 3);
/// assert_eq!(u, Vec2i::new(2, 3));
/// # }
/// ```
#[macro_export]
macro_rules! ivec2 {
    ($s:expr) => {{
        let s = $s as i32;
        Vec2i::new(s, s)
    }};
    ($x:expr, $y:expr) => {{
        Vec2i::new($x as i32, $y as i32)
    }};
    ($x:expr, $y:expr,) => {{
        Vec2i::new($x as i32, $y as i32)
    }};
}
