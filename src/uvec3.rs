// Generated code.
use std::ops::*;

/// 3-dimensional vector with pointer-sized unsigned integer `x`, `y`, and `z` components.
///
/// Vectors can also represent colors with the help of
/// `r()`, `g()`, and `b()` accessors.
///
/// Most operators and methods on vectors are performed in component-wise fashion. The notable
/// exception is vector-matrix multiplication.
#[derive(Debug, PartialEq, Copy, Clone)]
pub struct Vec3u { data: [usize; 3] }

impl Vec3u {
    /// Constructs a new `Vec3u`.
    pub fn new(x: usize, y: usize, z: usize) -> Self {
         Vec3u { data: [x, y, z] }
    }
    
    /// Component accessor, returns the 1st component of the vector.
    #[inline(always)] pub fn x(&self) -> usize { self[0] }

    /// Component accessor, returns the 2nd component of the vector.
    #[inline(always)] pub fn y(&self) -> usize { self[1] }

    /// Component accessor, returns the 3rd component of the vector.
    #[inline(always)] pub fn z(&self) -> usize { self[2] }
    
	
    /// Color-style component accessor, returns the 1st component of the vector.
    #[inline(always)] pub fn r(&self) -> usize { self[0] }

    /// Color-style component accessor, returns the 2nd component of the vector.
    #[inline(always)] pub fn g(&self) -> usize { self[1] }

    /// Color-style component accessor, returns the 3rd component of the vector.
    #[inline(always)] pub fn b(&self) -> usize { self[2] }
	
	/// Returns the sum of vector components.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let u = uvec3!(20, 30, 40);
    /// assert_eq!(u.sum(), 20 + 30 + 40);
    /// # }
    /// ```
    pub fn sum(&self) -> usize {
        self[0] + self[1] + self[2]
    }
}

pub trait Vec3uOps<Rhs> {
    fn dot(&self, rhs: Rhs) -> usize;
}

impl<'a> Vec3uOps<&'a Vec3u> for Vec3u {
    /// Returns dot product of two vectors.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let u = uvec3!(20, 30, 40).dot(uvec3!(2, 3, 4));
    /// assert_eq!(u, 20 * 2 + 30 * 3 + 40 * 4);
    /// # }
    /// ```
    fn dot(&self, rhs: &Vec3u) -> usize {
        (self * rhs).sum()
    }
}

impl Vec3uOps<Vec3u> for Vec3u {
	/// Shorthand for `lhs.dot(&rhs)`.
    fn dot(&self, rhs: Vec3u) -> usize {
        self.dot(&rhs)
    }
}

impl Index<usize> for Vec3u {
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
    /// let u = uvec3!(20, 30, 40);
    /// assert_eq!(u, uvec3!(u[0], u[1], u[2]));
    /// # }
    /// ```
    ///
    /// # Panics
    ///
    /// Will panic if the index is greater than 2.
    #[inline(always)] fn index<'a>(&'a self, i: usize) -> &'a usize {
        &self.data[i]
    }
}

impl<'a, 'b> Add<&'b Vec3u> for &'a Vec3u {
    type Output = Vec3u;

    /// Performs component-wise addition of two vectors producing a new vector.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let u = uvec3!(20, 30, 40) + uvec3!(2, 3, 4);
    /// assert_eq!(u, uvec3!(20 + 2, 30 + 3, 40 + 4));
    /// # }
    /// ```
    fn add(self, rhs: &Vec3u) -> Vec3u {
        Vec3u::new(self[0] + rhs[0], self[1] + rhs[1], self[2] + rhs[2])
    }
}

impl<'a> Add<Vec3u> for &'a Vec3u {
    type Output = Vec3u;
    
    /// Shorthand for `lhs + &rhs`.
    #[inline(always)] fn add(self, rhs: Vec3u) -> Vec3u {
        self + &rhs
    }
}

impl<'b> Add<&'b Vec3u> for Vec3u {
    type Output = Vec3u;
    
    /// Shorthand for `&lhs + rhs`.
    #[inline(always)] fn add(self, rhs: &Vec3u) -> Vec3u {
        &self + rhs
    }
}

impl Add<Vec3u> for Vec3u {
    type Output = Vec3u;
    
    /// Shorthand for `&lhs + &rhs`.
    #[inline(always)] fn add(self, rhs: Vec3u) -> Vec3u {
        &self + &rhs
    }
}

impl<'a> Add<usize> for &'a Vec3u {
    type Output = Vec3u;
    
    /// Adds a scalar to each component of a vector producing a new vector.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let u = uvec3!(20, 30, 40) + 2;
    /// assert_eq!(u, uvec3!(20 + 2, 30 + 2, 40 + 2));
    /// # }
    /// ```
    fn add(self, rhs: usize) -> Vec3u {
        Vec3u::new(self[0] + rhs, self[1] + rhs, self[2] + rhs)
    }
}

impl Add<usize> for Vec3u {
    type Output = Vec3u;
    
    /// Shorthand for `&lhs + rhs`.
    #[inline(always)] fn add(self, rhs: usize) -> Vec3u {
        &self + rhs
    }
}

impl<'a, 'b> Sub<&'b Vec3u> for &'a Vec3u {
    type Output = Vec3u;

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
    /// let u = uvec3!(20, 30, 40) - uvec3!(2, 3, 4);
    /// assert_eq!(u, uvec3!(20 - 2, 30 - 3, 40 - 4));
    /// # }
    /// ```
    fn sub(self, rhs: &Vec3u) -> Vec3u {
        Vec3u::new(self[0] - rhs[0], self[1] - rhs[1], self[2] - rhs[2])
    }
}

impl<'a> Sub<Vec3u> for &'a Vec3u {
    type Output = Vec3u;
    
    /// Shorthand for `lhs - &rhs`.
    #[inline(always)] fn sub(self, rhs: Vec3u) -> Vec3u {
        self - &rhs
    }
}

impl<'b> Sub<&'b Vec3u> for Vec3u {
    type Output = Vec3u;
    
    /// Shorthand for `&lhs - rhs`.
    #[inline(always)] fn sub(self, rhs: &Vec3u) -> Vec3u {
        &self - rhs
    }
}

impl Sub<Vec3u> for Vec3u {
    type Output = Vec3u;
    
    /// Shorthand for `&lhs - &rhs`.
    #[inline(always)] fn sub(self, rhs: Vec3u) -> Vec3u {
        &self - &rhs
    }
}

impl<'a> Sub<usize> for &'a Vec3u {
    type Output = Vec3u;
    
    /// Subtracts a scalar from each component of a vector producing a new vector.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let u = uvec3!(20, 30, 40) - 2;
    /// assert_eq!(u, uvec3!(20 - 2, 30 - 2, 40 - 2));
    /// # }
    /// ```
    fn sub(self, rhs: usize) -> Vec3u {
        Vec3u::new(self[0] - rhs, self[1] - rhs, self[2] - rhs)
    }
}

impl Sub<usize> for Vec3u {
    type Output = Vec3u;
    
    /// Shorthand for `&lhs - rhs`.
    #[inline(always)] fn sub(self, rhs: usize) -> Vec3u {
        &self - rhs
    }
}

impl<'a, 'b> Mul<&'b Vec3u> for &'a Vec3u {
    type Output = Vec3u;

    /// Performs component-wise multiplication of two vectors producing a new vector.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let u = uvec3!(20, 30, 40) * uvec3!(2, 3, 4);
    /// assert_eq!(u, uvec3!(20 * 2, 30 * 3, 40 * 4));
    /// # }
    /// ```
    fn mul(self, rhs: &Vec3u) -> Vec3u {
        Vec3u::new(self[0] * rhs[0], self[1] * rhs[1], self[2] * rhs[2])
    }
}

impl<'a> Mul<Vec3u> for &'a Vec3u {
    type Output = Vec3u;
    
    /// Shorthand for `lhs * &rhs`.
    #[inline(always)] fn mul(self, rhs: Vec3u) -> Vec3u {
        self * &rhs
    }
}

impl<'b> Mul<&'b Vec3u> for Vec3u {
    type Output = Vec3u;
    
    /// Shorthand for `&lhs * rhs`.
    #[inline(always)] fn mul(self, rhs: &Vec3u) -> Vec3u {
        &self * rhs
    }
}

impl Mul<Vec3u> for Vec3u {
    type Output = Vec3u;
    
    /// Shorthand for `&lhs * &rhs`.
    #[inline(always)] fn mul(self, rhs: Vec3u) -> Vec3u {
        &self * &rhs
    }
}

impl<'a> Mul<usize> for &'a Vec3u {
    type Output = Vec3u;
    
    /// Multiplies each component of a vector by a scalar producing a new vector.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let u = uvec3!(20, 30, 40) * 2;
    /// assert_eq!(u, uvec3!(20 * 2, 30 * 2, 40 * 2));
    /// # }
    /// ```
    fn mul(self, rhs: usize) -> Vec3u {
        Vec3u::new(self[0] * rhs, self[1] * rhs, self[2] * rhs)
    }
}

impl Mul<usize> for Vec3u {
    type Output = Vec3u;
    
    /// Shorthand for `&lhs * rhs`.
    #[inline(always)] fn mul(self, rhs: usize) -> Vec3u {
        &self * rhs
    }
}

impl<'a, 'b> Div<&'b Vec3u> for &'a Vec3u {
    type Output = Vec3u;

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
    /// let u = uvec3!(20, 30, 40) / uvec3!(2, 3, 4);
    /// assert_eq!(u, uvec3!(20 / 2, 30 / 3, 40 / 4));
    /// # }
    /// ```
    fn div(self, rhs: &Vec3u) -> Vec3u {
        Vec3u::new(self[0] / rhs[0], self[1] / rhs[1], self[2] / rhs[2])
    }
}

impl<'a> Div<Vec3u> for &'a Vec3u {
    type Output = Vec3u;
    
    /// Shorthand for `lhs / &rhs`.
    #[inline(always)] fn div(self, rhs: Vec3u) -> Vec3u {
        self / &rhs
    }
}

impl<'b> Div<&'b Vec3u> for Vec3u {
    type Output = Vec3u;
    
    /// Shorthand for `&lhs / rhs`.
    #[inline(always)] fn div(self, rhs: &Vec3u) -> Vec3u {
        &self / rhs
    }
}

impl Div<Vec3u> for Vec3u {
    type Output = Vec3u;
    
    /// Shorthand for `&lhs / &rhs`.
    #[inline(always)] fn div(self, rhs: Vec3u) -> Vec3u {
        &self / &rhs
    }
}

impl<'a> Div<usize> for &'a Vec3u {
    type Output = Vec3u;
    
    /// Divides each component of a vector by a scalar producing a new vector.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let u = uvec3!(20, 30, 40) / 2;
    /// assert_eq!(u, uvec3!(20 / 2, 30 / 2, 40 / 2));
    /// # }
    /// ```
    fn div(self, rhs: usize) -> Vec3u {
        Vec3u::new(self[0] / rhs, self[1] / rhs, self[2] / rhs)
    }
}

impl Div<usize> for Vec3u {
    type Output = Vec3u;
    
    /// Shorthand for `&lhs / rhs`.
    #[inline(always)] fn div(self, rhs: usize) -> Vec3u {
        &self / rhs
    }
}

/// Builder macro for creating new 3-dimensional vectors with pointer-sized unsigned integer components.
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
/// let u = uvec3!(2);
/// assert_eq!(u, Vec3u::new(2, 2, 2));
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
/// let u = uvec3!(2, 3, 4);
/// assert_eq!(u, Vec3u::new(2, 3, 4));
/// # }
/// ```
#[macro_export]
macro_rules! uvec3 {
    ($s:expr) => {{
        let s = $s as usize;
        Vec3u::new(s, s, s)
    }};
    ($x:expr, $y:expr, $z:expr) => {{
        Vec3u::new($x as usize, $y as usize, $z as usize)
    }};
    ($x:expr, $y:expr, $z:expr,) => {{
        Vec3u::new($x as usize, $y as usize, $z as usize)
    }};
}
