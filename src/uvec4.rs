// Generated code.
use std::ops::*;

/// 4-dimensional vector with pointer-sized unsigned integer `x`, `y`, `z`, and `w` components.
///
/// Vectors can also represent colors with the help of
/// `r()`, `g()`, `b()`, and `a()` accessors.
///
/// Most operators and methods on vectors are performed in component-wise fashion. The notable
/// exception is vector-matrix multiplication.
#[derive(Debug, PartialEq, Copy, Clone)]
pub struct Vec4u { data: [usize; 4] }

impl Vec4u {
    /// Constructs a new `Vec4u`.
    pub fn new(x: usize, y: usize, z: usize, w: usize) -> Self {
         Vec4u { data: [x, y, z, w] }
    }
    
    /// Component accessor, returns the 1st component of the vector.
    #[inline(always)] pub fn x(&self) -> usize { self[0] }

    /// Component accessor, returns the 2nd component of the vector.
    #[inline(always)] pub fn y(&self) -> usize { self[1] }

    /// Component accessor, returns the 3rd component of the vector.
    #[inline(always)] pub fn z(&self) -> usize { self[2] }

    /// Component accessor, returns the 4th component of the vector.
    #[inline(always)] pub fn w(&self) -> usize { self[3] }
    
	
    /// Color-style component accessor, returns the 1st component of the vector.
    #[inline(always)] pub fn r(&self) -> usize { self[0] }

    /// Color-style component accessor, returns the 2nd component of the vector.
    #[inline(always)] pub fn g(&self) -> usize { self[1] }

    /// Color-style component accessor, returns the 3rd component of the vector.
    #[inline(always)] pub fn b(&self) -> usize { self[2] }

    /// Color-style component accessor, returns the 4th component of the vector.
    #[inline(always)] pub fn a(&self) -> usize { self[3] }
	
	/// Returns the sum of vector components.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let u = uvec4!(20, 30, 40, 50);
    /// assert_eq!(u.sum(), 20 + 30 + 40 + 50);
    /// # }
    /// ```
    pub fn sum(&self) -> usize {
        self[0] + self[1] + self[2] + self[3]
    }
}

pub trait Vec4uOps<Rhs> {
    fn dot(&self, rhs: Rhs) -> usize;
}

impl<'a> Vec4uOps<&'a Vec4u> for Vec4u {
    /// Returns dot product of two vectors.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let u = uvec4!(20, 30, 40, 50).dot(uvec4!(2, 3, 4, 5));
    /// assert_eq!(u, 20 * 2 + 30 * 3 + 40 * 4 + 50 * 5);
    /// # }
    /// ```
    fn dot(&self, rhs: &Vec4u) -> usize {
        (self * rhs).sum()
    }
}

impl Vec4uOps<Vec4u> for Vec4u {
	/// Shorthand for `lhs.dot(&rhs)`.
    fn dot(&self, rhs: Vec4u) -> usize {
        self.dot(&rhs)
    }
}

impl Index<usize> for Vec4u {
    type Output = usize;
    
    /// Index notation for acessing components of a vector.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let u = uvec4!(20, 30, 40, 50);
    /// assert_eq!(u, uvec4!(u[0], u[1], u[2], u[3]));
    /// # }
    /// ```
    ///
    /// # Panics
    ///
    /// Will panic if the index is greater than 3.
    #[inline(always)] fn index<'a>(&'a self, i: usize) -> &'a usize {
        &self.data[i]
    }
}

impl<'a, 'b> Add<&'b Vec4u> for &'a Vec4u {
    type Output = Vec4u;

    /// Performs component-wise addition of two vectors producing a new vector.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let u = uvec4!(20, 30, 40, 50) + uvec4!(2, 3, 4, 5);
    /// assert_eq!(u, uvec4!(20 + 2, 30 + 3, 40 + 4, 50 + 5));
    /// # }
    /// ```
    fn add(self, rhs: &Vec4u) -> Vec4u {
        Vec4u::new(self[0] + rhs[0], self[1] + rhs[1], self[2] + rhs[2], self[3] + rhs[3])
    }
}

impl<'a> Add<Vec4u> for &'a Vec4u {
    type Output = Vec4u;
    
    /// Shorthand for `lhs + &rhs`.
    #[inline(always)] fn add(self, rhs: Vec4u) -> Vec4u {
        self + &rhs
    }
}

impl<'b> Add<&'b Vec4u> for Vec4u {
    type Output = Vec4u;
    
    /// Shorthand for `&lhs + rhs`.
    #[inline(always)] fn add(self, rhs: &Vec4u) -> Vec4u {
        &self + rhs
    }
}

impl Add<Vec4u> for Vec4u {
    type Output = Vec4u;
    
    /// Shorthand for `&lhs + &rhs`.
    #[inline(always)] fn add(self, rhs: Vec4u) -> Vec4u {
        &self + &rhs
    }
}

impl<'a> Add<usize> for &'a Vec4u {
    type Output = Vec4u;
    
    /// Adds a scalar to each component of a vector producing a new vector.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let u = uvec4!(20, 30, 40, 50) + 2;
    /// assert_eq!(u, uvec4!(20 + 2, 30 + 2, 40 + 2, 50 + 2));
    /// # }
    /// ```
    fn add(self, rhs: usize) -> Vec4u {
        Vec4u::new(self[0] + rhs, self[1] + rhs, self[2] + rhs, self[3] + rhs)
    }
}

impl Add<usize> for Vec4u {
    type Output = Vec4u;
    
    /// Shorthand for `&lhs + rhs`.
    #[inline(always)] fn add(self, rhs: usize) -> Vec4u {
        &self + rhs
    }
}

impl<'a, 'b> Sub<&'b Vec4u> for &'a Vec4u {
    type Output = Vec4u;

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
    /// let u = uvec4!(20, 30, 40, 50) - uvec4!(2, 3, 4, 5);
    /// assert_eq!(u, uvec4!(20 - 2, 30 - 3, 40 - 4, 50 - 5));
    /// # }
    /// ```
    fn sub(self, rhs: &Vec4u) -> Vec4u {
        Vec4u::new(self[0] - rhs[0], self[1] - rhs[1], self[2] - rhs[2], self[3] - rhs[3])
    }
}

impl<'a> Sub<Vec4u> for &'a Vec4u {
    type Output = Vec4u;
    
    /// Shorthand for `lhs - &rhs`.
    #[inline(always)] fn sub(self, rhs: Vec4u) -> Vec4u {
        self - &rhs
    }
}

impl<'b> Sub<&'b Vec4u> for Vec4u {
    type Output = Vec4u;
    
    /// Shorthand for `&lhs - rhs`.
    #[inline(always)] fn sub(self, rhs: &Vec4u) -> Vec4u {
        &self - rhs
    }
}

impl Sub<Vec4u> for Vec4u {
    type Output = Vec4u;
    
    /// Shorthand for `&lhs - &rhs`.
    #[inline(always)] fn sub(self, rhs: Vec4u) -> Vec4u {
        &self - &rhs
    }
}

impl<'a> Sub<usize> for &'a Vec4u {
    type Output = Vec4u;
    
    /// Subtracts a scalar from each component of a vector producing a new vector.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let u = uvec4!(20, 30, 40, 50) - 2;
    /// assert_eq!(u, uvec4!(20 - 2, 30 - 2, 40 - 2, 50 - 2));
    /// # }
    /// ```
    fn sub(self, rhs: usize) -> Vec4u {
        Vec4u::new(self[0] - rhs, self[1] - rhs, self[2] - rhs, self[3] - rhs)
    }
}

impl Sub<usize> for Vec4u {
    type Output = Vec4u;
    
    /// Shorthand for `&lhs - rhs`.
    #[inline(always)] fn sub(self, rhs: usize) -> Vec4u {
        &self - rhs
    }
}

impl<'a, 'b> Mul<&'b Vec4u> for &'a Vec4u {
    type Output = Vec4u;

    /// Performs component-wise multiplication of two vectors producing a new vector.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let u = uvec4!(20, 30, 40, 50) * uvec4!(2, 3, 4, 5);
    /// assert_eq!(u, uvec4!(20 * 2, 30 * 3, 40 * 4, 50 * 5));
    /// # }
    /// ```
    fn mul(self, rhs: &Vec4u) -> Vec4u {
        Vec4u::new(self[0] * rhs[0], self[1] * rhs[1], self[2] * rhs[2], self[3] * rhs[3])
    }
}

impl<'a> Mul<Vec4u> for &'a Vec4u {
    type Output = Vec4u;
    
    /// Shorthand for `lhs * &rhs`.
    #[inline(always)] fn mul(self, rhs: Vec4u) -> Vec4u {
        self * &rhs
    }
}

impl<'b> Mul<&'b Vec4u> for Vec4u {
    type Output = Vec4u;
    
    /// Shorthand for `&lhs * rhs`.
    #[inline(always)] fn mul(self, rhs: &Vec4u) -> Vec4u {
        &self * rhs
    }
}

impl Mul<Vec4u> for Vec4u {
    type Output = Vec4u;
    
    /// Shorthand for `&lhs * &rhs`.
    #[inline(always)] fn mul(self, rhs: Vec4u) -> Vec4u {
        &self * &rhs
    }
}

impl<'a> Mul<usize> for &'a Vec4u {
    type Output = Vec4u;
    
    /// Multiplies each component of a vector by a scalar producing a new vector.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let u = uvec4!(20, 30, 40, 50) * 2;
    /// assert_eq!(u, uvec4!(20 * 2, 30 * 2, 40 * 2, 50 * 2));
    /// # }
    /// ```
    fn mul(self, rhs: usize) -> Vec4u {
        Vec4u::new(self[0] * rhs, self[1] * rhs, self[2] * rhs, self[3] * rhs)
    }
}

impl Mul<usize> for Vec4u {
    type Output = Vec4u;
    
    /// Shorthand for `&lhs * rhs`.
    #[inline(always)] fn mul(self, rhs: usize) -> Vec4u {
        &self * rhs
    }
}

impl<'a, 'b> Div<&'b Vec4u> for &'a Vec4u {
    type Output = Vec4u;

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
    /// let u = uvec4!(20, 30, 40, 50) / uvec4!(2, 3, 4, 5);
    /// assert_eq!(u, uvec4!(20 / 2, 30 / 3, 40 / 4, 50 / 5));
    /// # }
    /// ```
    fn div(self, rhs: &Vec4u) -> Vec4u {
        Vec4u::new(self[0] / rhs[0], self[1] / rhs[1], self[2] / rhs[2], self[3] / rhs[3])
    }
}

impl<'a> Div<Vec4u> for &'a Vec4u {
    type Output = Vec4u;
    
    /// Shorthand for `lhs / &rhs`.
    #[inline(always)] fn div(self, rhs: Vec4u) -> Vec4u {
        self / &rhs
    }
}

impl<'b> Div<&'b Vec4u> for Vec4u {
    type Output = Vec4u;
    
    /// Shorthand for `&lhs / rhs`.
    #[inline(always)] fn div(self, rhs: &Vec4u) -> Vec4u {
        &self / rhs
    }
}

impl Div<Vec4u> for Vec4u {
    type Output = Vec4u;
    
    /// Shorthand for `&lhs / &rhs`.
    #[inline(always)] fn div(self, rhs: Vec4u) -> Vec4u {
        &self / &rhs
    }
}

impl<'a> Div<usize> for &'a Vec4u {
    type Output = Vec4u;
    
    /// Divides each component of a vector by a scalar producing a new vector.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let u = uvec4!(20, 30, 40, 50) / 2;
    /// assert_eq!(u, uvec4!(20 / 2, 30 / 2, 40 / 2, 50 / 2));
    /// # }
    /// ```
    fn div(self, rhs: usize) -> Vec4u {
        Vec4u::new(self[0] / rhs, self[1] / rhs, self[2] / rhs, self[3] / rhs)
    }
}

impl Div<usize> for Vec4u {
    type Output = Vec4u;
    
    /// Shorthand for `&lhs / rhs`.
    #[inline(always)] fn div(self, rhs: usize) -> Vec4u {
        &self / rhs
    }
}

/// Builder macro for creating new 4-dimensional vectors with pointer-sized unsigned integer components.
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
/// let u = uvec4!(2);
/// assert_eq!(u, Vec4u::new(2, 2, 2, 2));
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
/// let u = uvec4!(2, 3, 4, 5);
/// assert_eq!(u, Vec4u::new(2, 3, 4, 5));
/// # }
/// ```
#[macro_export]
macro_rules! uvec4 {
    ($s:expr) => {{
        let s = $s as usize;
        Vec4u::new(s, s, s, s)
    }};
    ($x:expr, $y:expr, $z:expr, $w:expr) => {{
        Vec4u::new($x as usize, $y as usize, $z as usize, $w as usize)
    }};
    ($x:expr, $y:expr, $z:expr, $w:expr,) => {{
        Vec4u::new($x as usize, $y as usize, $z as usize, $w as usize)
    }};
}
