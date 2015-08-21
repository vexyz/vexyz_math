// Generated code.
use std::ops::*;

/// 2-dimensional vector with pointer-sized unsigned integer `x`, and `y` components.
///
/// Vectors can also represent colors with the help of
/// `r()`, and `g()` accessors.
///
/// Most operators and methods on vectors are performed in component-wise fashion. The notable
/// exception is vector-matrix multiplication.
#[derive(Debug, PartialEq, Copy, Clone)]
pub struct Vec2u { data: [usize; 2] }

impl Vec2u {
    /// Constructs a new `Vec2u`.
    pub fn new(x: usize, y: usize) -> Self {
         Vec2u { data: [x, y] }
    }
    
    /// Component accessor, returns the 1st component of the vector.
    #[inline(always)] pub fn x(&self) -> usize { self[0] }

    /// Component accessor, returns the 2nd component of the vector.
    #[inline(always)] pub fn y(&self) -> usize { self[1] }
    
	
    /// Color-style component accessor, returns the 1st component of the vector.
    #[inline(always)] pub fn r(&self) -> usize { self[0] }

    /// Color-style component accessor, returns the 2nd component of the vector.
    #[inline(always)] pub fn g(&self) -> usize { self[1] }
	
	/// Returns the sum of vector components.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let u = uvec2!(20, 30);
    /// assert_eq!(u.sum(), 20 + 30);
    /// # }
    /// ```
    pub fn sum(&self) -> usize {
        self[0] + self[1]
    }
}

pub trait Vec2uOps<Rhs> {
    fn dot(&self, rhs: Rhs) -> usize;
}

impl<'a> Vec2uOps<&'a Vec2u> for Vec2u {
    /// Returns dot product of two vectors.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let u = uvec2!(20, 30).dot(uvec2!(2, 3));
    /// assert_eq!(u, 20 * 2 + 30 * 3);
    /// # }
    /// ```
    fn dot(&self, rhs: &Vec2u) -> usize {
        (self * rhs).sum()
    }
}

impl Vec2uOps<Vec2u> for Vec2u {
	/// Shorthand for `lhs.dot(&rhs)`.
    fn dot(&self, rhs: Vec2u) -> usize {
        self.dot(&rhs)
    }
}

impl Index<usize> for Vec2u {
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
    /// let u = uvec2!(20, 30);
    /// assert_eq!(u, uvec2!(u[0], u[1]));
    /// # }
    /// ```
    ///
    /// # Panics
    ///
    /// Will panic if the index is greater than 1.
    #[inline(always)] fn index<'a>(&'a self, i: usize) -> &'a usize {
        &self.data[i]
    }
}

impl<'a, 'b> Add<&'b Vec2u> for &'a Vec2u {
    type Output = Vec2u;

    /// Performs component-wise addition of two vectors producing a new vector.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let u = uvec2!(20, 30) + uvec2!(2, 3);
    /// assert_eq!(u, uvec2!(20 + 2, 30 + 3));
    /// # }
    /// ```
    fn add(self, rhs: &Vec2u) -> Vec2u {
        Vec2u::new(self[0] + rhs[0], self[1] + rhs[1])
    }
}

impl<'a> Add<Vec2u> for &'a Vec2u {
    type Output = Vec2u;
    
    /// Shorthand for `lhs + &rhs`.
    #[inline(always)] fn add(self, rhs: Vec2u) -> Vec2u {
        self + &rhs
    }
}

impl<'b> Add<&'b Vec2u> for Vec2u {
    type Output = Vec2u;
    
    /// Shorthand for `&lhs + rhs`.
    #[inline(always)] fn add(self, rhs: &Vec2u) -> Vec2u {
        &self + rhs
    }
}

impl Add<Vec2u> for Vec2u {
    type Output = Vec2u;
    
    /// Shorthand for `&lhs + &rhs`.
    #[inline(always)] fn add(self, rhs: Vec2u) -> Vec2u {
        &self + &rhs
    }
}

impl<'a> Add<usize> for &'a Vec2u {
    type Output = Vec2u;
    
    /// Adds a scalar to each component of a vector producing a new vector.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let u = uvec2!(20, 30) + 2;
    /// assert_eq!(u, uvec2!(20 + 2, 30 + 2));
    /// # }
    /// ```
    fn add(self, rhs: usize) -> Vec2u {
        Vec2u::new(self[0] + rhs, self[1] + rhs)
    }
}

impl Add<usize> for Vec2u {
    type Output = Vec2u;
    
    /// Shorthand for `&lhs + rhs`.
    #[inline(always)] fn add(self, rhs: usize) -> Vec2u {
        &self + rhs
    }
}

impl<'a, 'b> Sub<&'b Vec2u> for &'a Vec2u {
    type Output = Vec2u;

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
    /// let u = uvec2!(20, 30) - uvec2!(2, 3);
    /// assert_eq!(u, uvec2!(20 - 2, 30 - 3));
    /// # }
    /// ```
    fn sub(self, rhs: &Vec2u) -> Vec2u {
        Vec2u::new(self[0] - rhs[0], self[1] - rhs[1])
    }
}

impl<'a> Sub<Vec2u> for &'a Vec2u {
    type Output = Vec2u;
    
    /// Shorthand for `lhs - &rhs`.
    #[inline(always)] fn sub(self, rhs: Vec2u) -> Vec2u {
        self - &rhs
    }
}

impl<'b> Sub<&'b Vec2u> for Vec2u {
    type Output = Vec2u;
    
    /// Shorthand for `&lhs - rhs`.
    #[inline(always)] fn sub(self, rhs: &Vec2u) -> Vec2u {
        &self - rhs
    }
}

impl Sub<Vec2u> for Vec2u {
    type Output = Vec2u;
    
    /// Shorthand for `&lhs - &rhs`.
    #[inline(always)] fn sub(self, rhs: Vec2u) -> Vec2u {
        &self - &rhs
    }
}

impl<'a> Sub<usize> for &'a Vec2u {
    type Output = Vec2u;
    
    /// Subtracts a scalar from each component of a vector producing a new vector.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let u = uvec2!(20, 30) - 2;
    /// assert_eq!(u, uvec2!(20 - 2, 30 - 2));
    /// # }
    /// ```
    fn sub(self, rhs: usize) -> Vec2u {
        Vec2u::new(self[0] - rhs, self[1] - rhs)
    }
}

impl Sub<usize> for Vec2u {
    type Output = Vec2u;
    
    /// Shorthand for `&lhs - rhs`.
    #[inline(always)] fn sub(self, rhs: usize) -> Vec2u {
        &self - rhs
    }
}

impl<'a, 'b> Mul<&'b Vec2u> for &'a Vec2u {
    type Output = Vec2u;

    /// Performs component-wise multiplication of two vectors producing a new vector.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let u = uvec2!(20, 30) * uvec2!(2, 3);
    /// assert_eq!(u, uvec2!(20 * 2, 30 * 3));
    /// # }
    /// ```
    fn mul(self, rhs: &Vec2u) -> Vec2u {
        Vec2u::new(self[0] * rhs[0], self[1] * rhs[1])
    }
}

impl<'a> Mul<Vec2u> for &'a Vec2u {
    type Output = Vec2u;
    
    /// Shorthand for `lhs * &rhs`.
    #[inline(always)] fn mul(self, rhs: Vec2u) -> Vec2u {
        self * &rhs
    }
}

impl<'b> Mul<&'b Vec2u> for Vec2u {
    type Output = Vec2u;
    
    /// Shorthand for `&lhs * rhs`.
    #[inline(always)] fn mul(self, rhs: &Vec2u) -> Vec2u {
        &self * rhs
    }
}

impl Mul<Vec2u> for Vec2u {
    type Output = Vec2u;
    
    /// Shorthand for `&lhs * &rhs`.
    #[inline(always)] fn mul(self, rhs: Vec2u) -> Vec2u {
        &self * &rhs
    }
}

impl<'a> Mul<usize> for &'a Vec2u {
    type Output = Vec2u;
    
    /// Multiplies each component of a vector by a scalar producing a new vector.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let u = uvec2!(20, 30) * 2;
    /// assert_eq!(u, uvec2!(20 * 2, 30 * 2));
    /// # }
    /// ```
    fn mul(self, rhs: usize) -> Vec2u {
        Vec2u::new(self[0] * rhs, self[1] * rhs)
    }
}

impl Mul<usize> for Vec2u {
    type Output = Vec2u;
    
    /// Shorthand for `&lhs * rhs`.
    #[inline(always)] fn mul(self, rhs: usize) -> Vec2u {
        &self * rhs
    }
}

impl<'a, 'b> Div<&'b Vec2u> for &'a Vec2u {
    type Output = Vec2u;

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
    /// let u = uvec2!(20, 30) / uvec2!(2, 3);
    /// assert_eq!(u, uvec2!(20 / 2, 30 / 3));
    /// # }
    /// ```
    fn div(self, rhs: &Vec2u) -> Vec2u {
        Vec2u::new(self[0] / rhs[0], self[1] / rhs[1])
    }
}

impl<'a> Div<Vec2u> for &'a Vec2u {
    type Output = Vec2u;
    
    /// Shorthand for `lhs / &rhs`.
    #[inline(always)] fn div(self, rhs: Vec2u) -> Vec2u {
        self / &rhs
    }
}

impl<'b> Div<&'b Vec2u> for Vec2u {
    type Output = Vec2u;
    
    /// Shorthand for `&lhs / rhs`.
    #[inline(always)] fn div(self, rhs: &Vec2u) -> Vec2u {
        &self / rhs
    }
}

impl Div<Vec2u> for Vec2u {
    type Output = Vec2u;
    
    /// Shorthand for `&lhs / &rhs`.
    #[inline(always)] fn div(self, rhs: Vec2u) -> Vec2u {
        &self / &rhs
    }
}

impl<'a> Div<usize> for &'a Vec2u {
    type Output = Vec2u;
    
    /// Divides each component of a vector by a scalar producing a new vector.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let u = uvec2!(20, 30) / 2;
    /// assert_eq!(u, uvec2!(20 / 2, 30 / 2));
    /// # }
    /// ```
    fn div(self, rhs: usize) -> Vec2u {
        Vec2u::new(self[0] / rhs, self[1] / rhs)
    }
}

impl Div<usize> for Vec2u {
    type Output = Vec2u;
    
    /// Shorthand for `&lhs / rhs`.
    #[inline(always)] fn div(self, rhs: usize) -> Vec2u {
        &self / rhs
    }
}

/// Builder macro for creating new 2-dimensional vectors with pointer-sized unsigned integer components.
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
/// let u = uvec2!(2);
/// assert_eq!(u, Vec2u::new(2, 2));
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
/// let u = uvec2!(2, 3);
/// assert_eq!(u, Vec2u::new(2, 3));
/// # }
/// ```
#[macro_export]
macro_rules! uvec2 {
    ($s:expr) => {{
        let s = $s as usize;
        Vec2u::new(s, s)
    }};
    ($x:expr, $y:expr) => {{
        Vec2u::new($x as usize, $y as usize)
    }};
    ($x:expr, $y:expr,) => {{
        Vec2u::new($x as usize, $y as usize)
    }};
}
