// Generated code.
use std::ops::*;

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
        self[0] + self[1] + self[2] + self[3]
    }
}

pub trait Vec4iOps<Rhs> {
    fn dot(&self, rhs: Rhs) -> i32;
}

impl<'a> Vec4iOps<&'a Vec4i> for Vec4i {
    /// Returns dot product of two vectors.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let u = ivec4!(20, 30, 40, 50).dot(ivec4!(2, 3, 4, 5));
    /// assert_eq!(u, 20 * 2 + 30 * 3 + 40 * 4 + 50 * 5);
    /// # }
    /// ```
    fn dot(&self, rhs: &Vec4i) -> i32 {
        (self * rhs).sum()
    }
}

impl Vec4iOps<Vec4i> for Vec4i {
	/// Shorthand for `lhs.dot(&rhs)`.
    fn dot(&self, rhs: Vec4i) -> i32 {
        self.dot(&rhs)
    }
}

impl Index<usize> for Vec4i {
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

impl<'a, 'b> Add<&'b Vec4i> for &'a Vec4i {
    type Output = Vec4i;

    /// Performs component-wise addition of two vectors producing a new vector.
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
        Vec4i::new(self[0] + rhs[0], self[1] + rhs[1], self[2] + rhs[2], self[3] + rhs[3])
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
    
    /// Adds a scalar to each component of a vector producing a new vector.
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
        Vec4i::new(self[0] + rhs, self[1] + rhs, self[2] + rhs, self[3] + rhs)
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
    /// corresponding component of the `lhs` vector producing a new vector.
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
        Vec4i::new(self[0] - rhs[0], self[1] - rhs[1], self[2] - rhs[2], self[3] - rhs[3])
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
    
    /// Subtracts a scalar from each component of a vector producing a new vector.
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
        Vec4i::new(self[0] - rhs, self[1] - rhs, self[2] - rhs, self[3] - rhs)
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

    /// Performs component-wise multiplication of two vectors producing a new vector.
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
        Vec4i::new(self[0] * rhs[0], self[1] * rhs[1], self[2] * rhs[2], self[3] * rhs[3])
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
    
    /// Multiplies each component of a vector by a scalar producing a new vector.
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
        Vec4i::new(self[0] * rhs, self[1] * rhs, self[2] * rhs, self[3] * rhs)
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
    /// corresponding component of the `rhs` vector producing a new vector.
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
        Vec4i::new(self[0] / rhs[0], self[1] / rhs[1], self[2] / rhs[2], self[3] / rhs[3])
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
    
    /// Divides each component of a vector by a scalar producing a new vector.
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
        Vec4i::new(self[0] / rhs, self[1] / rhs, self[2] / rhs, self[3] / rhs)
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
    
    /// Applies negation to each component of a vector producing a new vector.
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
        Vec4i::new(-self[0], -self[1], -self[2], -self[3])
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
