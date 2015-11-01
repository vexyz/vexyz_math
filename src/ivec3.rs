// Generated code.
use std::fmt::{Display, Formatter, Result};
use std::ops::*;
use Vec3b;

/// 3-dimensional vector with integer `x`, `y`, and `z` components.
///
/// Vectors can also represent colors with the help of
/// `r()`, `g()`, and `b()` accessors.
///
/// Most operators and methods on vectors are performed in component-wise fashion. The notable
/// exception is vector-matrix multiplication.
#[derive(Debug, PartialEq, Copy, Clone)]
pub struct Vec3i { data: [i32; 3] }

impl Vec3i {
    /// Constructs a new `Vec3i`.
    pub fn new(x: i32, y: i32, z: i32) -> Self {
         Vec3i { data: [x, y, z] }
    }
    
    /// Component accessor, returns the 1st component of the vector.
    #[inline(always)] pub fn x(&self) -> i32 { self[0] }

    /// Component accessor, returns the 2nd component of the vector.
    #[inline(always)] pub fn y(&self) -> i32 { self[1] }

    /// Component accessor, returns the 3rd component of the vector.
    #[inline(always)] pub fn z(&self) -> i32 { self[2] }
    
    
    /// Color-style component accessor, returns the 1st component of the vector.
    #[inline(always)] pub fn r(&self) -> i32 { self[0] }

    /// Color-style component accessor, returns the 2nd component of the vector.
    #[inline(always)] pub fn g(&self) -> i32 { self[1] }

    /// Color-style component accessor, returns the 3rd component of the vector.
    #[inline(always)] pub fn b(&self) -> i32 { self[2] }
    
    /// Returns the sum of vector components.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let u = ivec3!(20, 30, 40);
    /// assert_eq!(u.sum(), 20 + 30 + 40);
    /// # }
    /// ```
    pub fn sum(&self) -> i32 {
        self[0] + self[1] + self[2]
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
    /// let u = ivec3!(20, -30, 40);
    /// assert_eq!(u.abs(), ivec3!(20, 30, 40));
    /// # }
    /// ```
    pub fn abs(&self) -> Vec3i {
        Vec3i::new(self[0].abs(), self[1].abs(), self[2].abs())
    }
}

pub trait Vec3iOps<Rhs> {
    fn less_than(&self, rhs: Rhs) -> Vec3b;

    fn less_than_equal(&self, rhs: Rhs) -> Vec3b;

    fn greater_than(&self, rhs: Rhs) -> Vec3b;

    fn greater_than_equals(&self, rhs: Rhs) -> Vec3b;

    fn equal(&self, rhs: Rhs) -> Vec3b;

    fn not_equal(&self, rhs: Rhs) -> Vec3b;

    fn dot(&self, rhs: Rhs) -> i32;
}

impl<'a> Vec3iOps<&'a Vec3i> for Vec3i {
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
    /// let u = ivec3!(20, 30, 40);
    /// let v = u + ivec3!(-1, 0, 1);
    /// assert_eq!(u.less_than(v), bvec3!(u.x() < v.x(), u.y() < v.y(), u.z() < v.z()));
    /// # }
    /// ```
    fn less_than(&self, rhs: &Vec3i) -> Vec3b {
        Vec3b::new(self[0] < rhs[0], self[1] < rhs[1], self[2] < rhs[2])
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
    /// let u = ivec3!(20, 30, 40);
    /// let v = u + ivec3!(-1, 0, 1);
    /// assert_eq!(u.less_than_equal(v), bvec3!(u.x() <= v.x(), u.y() <= v.y(), u.z() <= v.z()));
    /// # }
    /// ```
    fn less_than_equal(&self, rhs: &Vec3i) -> Vec3b {
        Vec3b::new(self[0] <= rhs[0], self[1] <= rhs[1], self[2] <= rhs[2])
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
    /// let u = ivec3!(20, 30, 40);
    /// let v = u + ivec3!(-1, 0, 1);
    /// assert_eq!(u.greater_than(v), bvec3!(u.x() > v.x(), u.y() > v.y(), u.z() > v.z()));
    /// # }
    /// ```
    fn greater_than(&self, rhs: &Vec3i) -> Vec3b {
        Vec3b::new(self[0] > rhs[0], self[1] > rhs[1], self[2] > rhs[2])
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
    /// let u = ivec3!(20, 30, 40);
    /// let v = u + ivec3!(-1, 0, 1);
    /// assert_eq!(u.greater_than_equals(v), bvec3!(u.x() >= v.x(), u.y() >= v.y(), u.z() >= v.z()));
    /// # }
    /// ```
    fn greater_than_equals(&self, rhs: &Vec3i) -> Vec3b {
        Vec3b::new(self[0] >= rhs[0], self[1] >= rhs[1], self[2] >= rhs[2])
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
    /// let u = ivec3!(20, 30, 40);
    /// let v = u + ivec3!(-1, 0, 1);
    /// assert_eq!(u.equal(v), bvec3!(u.x() == v.x(), u.y() == v.y(), u.z() == v.z()));
    /// # }
    /// ```
    fn equal(&self, rhs: &Vec3i) -> Vec3b {
        Vec3b::new(self[0] == rhs[0], self[1] == rhs[1], self[2] == rhs[2])
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
    /// let u = ivec3!(20, 30, 40);
    /// let v = u + ivec3!(-1, 0, 1);
    /// assert_eq!(u.not_equal(v), bvec3!(u.x() != v.x(), u.y() != v.y(), u.z() != v.z()));
    /// # }
    /// ```
    fn not_equal(&self, rhs: &Vec3i) -> Vec3b {
        Vec3b::new(self[0] != rhs[0], self[1] != rhs[1], self[2] != rhs[2])
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
    /// let u = ivec3!(20, 30, 40);
    /// let v = ivec3!(2, 3, 4);
    /// assert_eq!(u.dot(v), 20 * 2 + 30 * 3 + 40 * 4);
    /// # }
    /// ```
    fn dot(&self, rhs: &Vec3i) -> i32 {
        (self * rhs).sum()
    }
}

impl Vec3iOps<Vec3i> for Vec3i {
    /// Shorthand for `lhs.less_than(&rhs)`.
    #[inline(always)] fn less_than(&self, rhs: Vec3i) -> Vec3b {
        self.less_than(&rhs)
    }

    /// Shorthand for `lhs.less_than_equal(&rhs)`.
    #[inline(always)] fn less_than_equal(&self, rhs: Vec3i) -> Vec3b {
        self.less_than_equal(&rhs)
    }

    /// Shorthand for `lhs.greater_than(&rhs)`.
    #[inline(always)] fn greater_than(&self, rhs: Vec3i) -> Vec3b {
        self.greater_than(&rhs)
    }

    /// Shorthand for `lhs.greater_than_equals(&rhs)`.
    #[inline(always)] fn greater_than_equals(&self, rhs: Vec3i) -> Vec3b {
        self.greater_than_equals(&rhs)
    }

    /// Shorthand for `lhs.equal(&rhs)`.
    #[inline(always)] fn equal(&self, rhs: Vec3i) -> Vec3b {
        self.equal(&rhs)
    }

    /// Shorthand for `lhs.not_equal(&rhs)`.
    #[inline(always)] fn not_equal(&self, rhs: Vec3i) -> Vec3b {
        self.not_equal(&rhs)
    }

    /// Shorthand for `lhs.dot(&rhs)`.
    #[inline(always)] fn dot(&self, rhs: Vec3i) -> i32 {
        self.dot(&rhs)
    }
}

impl Display for Vec3i {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "Vec3i({}, {}, {})", self[0], self[1], self[2])
    }
}

impl Index<usize> for Vec3i {
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
    /// let u = ivec3!(20, 30, 40);
    /// assert_eq!(u, ivec3!(u[0], u[1], u[2]));
    /// # }
    /// ```
    ///
    /// # Panics
    ///
    /// Will panic if the index is greater than 2.
    #[inline(always)] fn index<'a>(&'a self, i: usize) -> &'a i32 {
        &self.data[i]
    }
}

impl<'a, 'b> Add<&'b Vec3i> for &'a Vec3i {
    type Output = Vec3i;

    /// Performs component-wise addition of two vectors, producing a new vector.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let u = ivec3!(20, 30, 40) + ivec3!(2, 3, 4);
    /// assert_eq!(u, ivec3!(20 + 2, 30 + 3, 40 + 4));
    /// # }
    /// ```
    fn add(self, rhs: &Vec3i) -> Vec3i {
        Vec3i::new(self[0] + rhs[0], self[1] + rhs[1], self[2] + rhs[2])
    }
}

impl<'a> Add<Vec3i> for &'a Vec3i {
    type Output = Vec3i;
    
    /// Shorthand for `lhs + &rhs`.
    #[inline(always)] fn add(self, rhs: Vec3i) -> Vec3i {
        self + &rhs
    }
}

impl<'b> Add<&'b Vec3i> for Vec3i {
    type Output = Vec3i;
    
    /// Shorthand for `&lhs + rhs`.
    #[inline(always)] fn add(self, rhs: &Vec3i) -> Vec3i {
        &self + rhs
    }
}

impl Add<Vec3i> for Vec3i {
    type Output = Vec3i;
    
    /// Shorthand for `&lhs + &rhs`.
    #[inline(always)] fn add(self, rhs: Vec3i) -> Vec3i {
        &self + &rhs
    }
}

impl<'a> Add<i32> for &'a Vec3i {
    type Output = Vec3i;
    
    /// Adds a scalar to each component of a vector, producing a new vector.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let u = ivec3!(20, 30, 40) + 2;
    /// assert_eq!(u, ivec3!(20 + 2, 30 + 2, 40 + 2));
    /// # }
    /// ```
    fn add(self, rhs: i32) -> Vec3i {
        Vec3i::new(self[0] + rhs, self[1] + rhs, self[2] + rhs)
    }
}

impl Add<i32> for Vec3i {
    type Output = Vec3i;
    
    /// Shorthand for `&lhs + rhs`.
    #[inline(always)] fn add(self, rhs: i32) -> Vec3i {
        &self + rhs
    }
}

impl<'a, 'b> Sub<&'b Vec3i> for &'a Vec3i {
    type Output = Vec3i;

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
    /// let u = ivec3!(20, 30, 40) - ivec3!(2, 3, 4);
    /// assert_eq!(u, ivec3!(20 - 2, 30 - 3, 40 - 4));
    /// # }
    /// ```
    fn sub(self, rhs: &Vec3i) -> Vec3i {
        Vec3i::new(self[0] - rhs[0], self[1] - rhs[1], self[2] - rhs[2])
    }
}

impl<'a> Sub<Vec3i> for &'a Vec3i {
    type Output = Vec3i;
    
    /// Shorthand for `lhs - &rhs`.
    #[inline(always)] fn sub(self, rhs: Vec3i) -> Vec3i {
        self - &rhs
    }
}

impl<'b> Sub<&'b Vec3i> for Vec3i {
    type Output = Vec3i;
    
    /// Shorthand for `&lhs - rhs`.
    #[inline(always)] fn sub(self, rhs: &Vec3i) -> Vec3i {
        &self - rhs
    }
}

impl Sub<Vec3i> for Vec3i {
    type Output = Vec3i;
    
    /// Shorthand for `&lhs - &rhs`.
    #[inline(always)] fn sub(self, rhs: Vec3i) -> Vec3i {
        &self - &rhs
    }
}

impl<'a> Sub<i32> for &'a Vec3i {
    type Output = Vec3i;
    
    /// Subtracts a scalar from each component of a vector, producing a new vector.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let u = ivec3!(20, 30, 40) - 2;
    /// assert_eq!(u, ivec3!(20 - 2, 30 - 2, 40 - 2));
    /// # }
    /// ```
    fn sub(self, rhs: i32) -> Vec3i {
        Vec3i::new(self[0] - rhs, self[1] - rhs, self[2] - rhs)
    }
}

impl Sub<i32> for Vec3i {
    type Output = Vec3i;
    
    /// Shorthand for `&lhs - rhs`.
    #[inline(always)] fn sub(self, rhs: i32) -> Vec3i {
        &self - rhs
    }
}

impl<'a, 'b> Mul<&'b Vec3i> for &'a Vec3i {
    type Output = Vec3i;

    /// Performs component-wise multiplication of two vectors, producing a new vector.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let u = ivec3!(20, 30, 40) * ivec3!(2, 3, 4);
    /// assert_eq!(u, ivec3!(20 * 2, 30 * 3, 40 * 4));
    /// # }
    /// ```
    fn mul(self, rhs: &Vec3i) -> Vec3i {
        Vec3i::new(self[0] * rhs[0], self[1] * rhs[1], self[2] * rhs[2])
    }
}

impl<'a> Mul<Vec3i> for &'a Vec3i {
    type Output = Vec3i;
    
    /// Shorthand for `lhs * &rhs`.
    #[inline(always)] fn mul(self, rhs: Vec3i) -> Vec3i {
        self * &rhs
    }
}

impl<'b> Mul<&'b Vec3i> for Vec3i {
    type Output = Vec3i;
    
    /// Shorthand for `&lhs * rhs`.
    #[inline(always)] fn mul(self, rhs: &Vec3i) -> Vec3i {
        &self * rhs
    }
}

impl Mul<Vec3i> for Vec3i {
    type Output = Vec3i;
    
    /// Shorthand for `&lhs * &rhs`.
    #[inline(always)] fn mul(self, rhs: Vec3i) -> Vec3i {
        &self * &rhs
    }
}

impl<'a> Mul<i32> for &'a Vec3i {
    type Output = Vec3i;
    
    /// Multiplies each component of a vector by a scalar, producing a new vector.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let u = ivec3!(20, 30, 40) * 2;
    /// assert_eq!(u, ivec3!(20 * 2, 30 * 2, 40 * 2));
    /// # }
    /// ```
    fn mul(self, rhs: i32) -> Vec3i {
        Vec3i::new(self[0] * rhs, self[1] * rhs, self[2] * rhs)
    }
}

impl Mul<i32> for Vec3i {
    type Output = Vec3i;
    
    /// Shorthand for `&lhs * rhs`.
    #[inline(always)] fn mul(self, rhs: i32) -> Vec3i {
        &self * rhs
    }
}

impl<'a, 'b> Div<&'b Vec3i> for &'a Vec3i {
    type Output = Vec3i;

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
    /// let u = ivec3!(20, 30, 40) / ivec3!(2, 3, 4);
    /// assert_eq!(u, ivec3!(20 / 2, 30 / 3, 40 / 4));
    /// # }
    /// ```
    fn div(self, rhs: &Vec3i) -> Vec3i {
        Vec3i::new(self[0] / rhs[0], self[1] / rhs[1], self[2] / rhs[2])
    }
}

impl<'a> Div<Vec3i> for &'a Vec3i {
    type Output = Vec3i;
    
    /// Shorthand for `lhs / &rhs`.
    #[inline(always)] fn div(self, rhs: Vec3i) -> Vec3i {
        self / &rhs
    }
}

impl<'b> Div<&'b Vec3i> for Vec3i {
    type Output = Vec3i;
    
    /// Shorthand for `&lhs / rhs`.
    #[inline(always)] fn div(self, rhs: &Vec3i) -> Vec3i {
        &self / rhs
    }
}

impl Div<Vec3i> for Vec3i {
    type Output = Vec3i;
    
    /// Shorthand for `&lhs / &rhs`.
    #[inline(always)] fn div(self, rhs: Vec3i) -> Vec3i {
        &self / &rhs
    }
}

impl<'a> Div<i32> for &'a Vec3i {
    type Output = Vec3i;
    
    /// Divides each component of a vector by a scalar, producing a new vector.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let u = ivec3!(20, 30, 40) / 2;
    /// assert_eq!(u, ivec3!(20 / 2, 30 / 2, 40 / 2));
    /// # }
    /// ```
    fn div(self, rhs: i32) -> Vec3i {
        Vec3i::new(self[0] / rhs, self[1] / rhs, self[2] / rhs)
    }
}

impl Div<i32> for Vec3i {
    type Output = Vec3i;
    
    /// Shorthand for `&lhs / rhs`.
    #[inline(always)] fn div(self, rhs: i32) -> Vec3i {
        &self / rhs
    }
}

impl<'a> Neg for &'a Vec3i {
    type Output = Vec3i;
    
    /// Applies negation to each component of a vector, producing a new vector.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let u = ivec3!(20, 30, 40);
    /// assert_eq!(-u, ivec3!(-20, -30, -40));
    /// # }
    /// ```
    fn neg(self) -> Vec3i {
        Vec3i::new(-self[0], -self[1], -self[2])
    }
}

impl Neg for Vec3i {
    type Output = Vec3i;
    
    /// Shorthand for `-&arg`.
    #[inline(always)] fn neg(self) -> Vec3i {
        -&self
    }
}

/// Builder macro for creating new 3-dimensional vectors with integer components.
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
/// let u = ivec3!(2);
/// assert_eq!(u, Vec3i::new(2, 2, 2));
/// # }
/// ```
/// 
/// Create a new vector with `x = 2`, `y = 3`, and `z = 4`:
///
/// ```
/// #[macro_use] extern crate vexyz_math;
/// use vexyz_math::*;
///
/// # fn main() {
/// let u = ivec3!(2, 3, 4);
/// assert_eq!(u, Vec3i::new(2, 3, 4));
/// # }
/// ```
#[macro_export]
macro_rules! ivec3 {
    ($s:expr) => {{
        let s = $s as i32;
        Vec3i::new(s, s, s)
    }};
    ($x:expr, $y:expr, $z:expr) => {{
        Vec3i::new($x as i32, $y as i32, $z as i32)
    }};
    ($x:expr, $y:expr, $z:expr,) => {{
        Vec3i::new($x as i32, $y as i32, $z as i32)
    }};
}
