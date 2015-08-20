// Generated code.
use std::ops::*;

/// 2-dimensional vector with floating point `x`, and `y` components.
///
/// Vectors can also represent colors with the help of
/// `r()`, and `g()` accessors.
///
/// Most operators and methods on vectors are performed in component-wise fashion. The notable
/// exception is vector-matrix multiplication.
#[derive(Debug, PartialEq, Copy, Clone)]
pub struct Vec2 { data: [f64; 2] }

impl Vec2 {
    /// Constructs a new `Vec2`.
    pub fn new(x: f64, y: f64) -> Self {
         Vec2 { data: [x, y] }
    }
    
    /// Component accessor, returns the 1st component of the vector.
    #[inline(always)] pub fn x(&self) -> f64 { self[0] }

    /// Component accessor, returns the 2nd component of the vector.
    #[inline(always)] pub fn y(&self) -> f64 { self[1] }
    
	
    /// Color-style component accessor, returns the 1st component of the vector.
    #[inline(always)] pub fn r(&self) -> f64 { self[0] }

    /// Color-style component accessor, returns the 2nd component of the vector.
    #[inline(always)] pub fn g(&self) -> f64 { self[1] }
	
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
    pub fn sum(&self) -> f64 {
        self[0] + self[1]
    }
}

pub trait Vec2Ops<Rhs> {
    fn dot(&self, rhs: Rhs) -> f64;
}

impl<'a> Vec2Ops<&'a Vec2> for Vec2 {
    /// Returns dot product of two vectors.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let u = vec2!(20, 30).dot(vec2!(2, 3));
    /// assert_eq!(u, 20.0 * 2.0 + 30.0 * 3.0);
    /// # }
    /// ```
    fn dot(&self, rhs: &Vec2) -> f64 {
        (self * rhs).sum()
    }
}

impl Vec2Ops<Vec2> for Vec2 {
	/// Shorthand for `lhs.dot(&rhs)`.
    fn dot(&self, rhs: Vec2) -> f64 {
        self.dot(&rhs)
    }
}

impl Index<usize> for Vec2 {
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
    /// let u = vec2!(20, 30);
    /// assert_eq!(u, vec2!(u[0], u[1]));
    /// # }
    /// ```
    ///
    /// # Panics
    ///
    /// Will panic if the index is greater than 1.
    #[inline(always)] fn index<'a>(&'a self, i: usize) -> &'a f64 {
        &self.data[i]
    }
}

impl<'a, 'b> Add<&'b Vec2> for &'a Vec2 {
    type Output = Vec2;

    /// Performs component-wise addition of two vectors producing a new vector.
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
        Vec2::new(self[0] + rhs[0], self[1] + rhs[1])
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

impl<'a> Add<f64> for &'a Vec2 {
    type Output = Vec2;
    
    /// Adds a scalar to each component of a vector producing a new vector.
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
    fn add(self, rhs: f64) -> Vec2 {
        Vec2::new(self[0] + rhs, self[1] + rhs)
    }
}

impl Add<f64> for Vec2 {
    type Output = Vec2;
    
    /// Shorthand for `&lhs + rhs`.
    #[inline(always)] fn add(self, rhs: f64) -> Vec2 {
        &self + rhs
    }
}

impl<'a, 'b> Sub<&'b Vec2> for &'a Vec2 {
    type Output = Vec2;

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
    /// let u = vec2!(20, 30) - vec2!(2, 3);
    /// assert_eq!(u, vec2!(20 - 2, 30 - 3));
    /// # }
    /// ```
    fn sub(self, rhs: &Vec2) -> Vec2 {
        Vec2::new(self[0] - rhs[0], self[1] - rhs[1])
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

impl<'a> Sub<f64> for &'a Vec2 {
    type Output = Vec2;
    
    /// Subtracts a scalar from each component of a vector producing a new vector.
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
    fn sub(self, rhs: f64) -> Vec2 {
        Vec2::new(self[0] - rhs, self[1] - rhs)
    }
}

impl Sub<f64> for Vec2 {
    type Output = Vec2;
    
    /// Shorthand for `&lhs - rhs`.
    #[inline(always)] fn sub(self, rhs: f64) -> Vec2 {
        &self - rhs
    }
}

impl<'a, 'b> Mul<&'b Vec2> for &'a Vec2 {
    type Output = Vec2;

    /// Performs component-wise multiplication of two vectors producing a new vector.
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
        Vec2::new(self[0] * rhs[0], self[1] * rhs[1])
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

impl<'a> Mul<f64> for &'a Vec2 {
    type Output = Vec2;
    
    /// Multiplies each component of a vector by a scalar producing a new vector.
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
    fn mul(self, rhs: f64) -> Vec2 {
        Vec2::new(self[0] * rhs, self[1] * rhs)
    }
}

impl Mul<f64> for Vec2 {
    type Output = Vec2;
    
    /// Shorthand for `&lhs * rhs`.
    #[inline(always)] fn mul(self, rhs: f64) -> Vec2 {
        &self * rhs
    }
}

impl<'a, 'b> Div<&'b Vec2> for &'a Vec2 {
    type Output = Vec2;

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
    /// let u = vec2!(20, 30) / vec2!(2, 3);
    /// assert_eq!(u, vec2!(20 / 2, 30 / 3));
    /// # }
    /// ```
    fn div(self, rhs: &Vec2) -> Vec2 {
        Vec2::new(self[0] / rhs[0], self[1] / rhs[1])
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

impl<'a> Div<f64> for &'a Vec2 {
    type Output = Vec2;
    
    /// Divides each component of a vector by a scalar producing a new vector.
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
    fn div(self, rhs: f64) -> Vec2 {
        Vec2::new(self[0] / rhs, self[1] / rhs)
    }
}

impl Div<f64> for Vec2 {
    type Output = Vec2;
    
    /// Shorthand for `&lhs / rhs`.
    #[inline(always)] fn div(self, rhs: f64) -> Vec2 {
        &self / rhs
    }
}

impl<'a> Neg for &'a Vec2 {
    type Output = Vec2;
    
    /// Applies negation to each component of a vector producing a new vector.
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
        Vec2::new(-self[0], -self[1])
    }
}

impl Neg for Vec2 {
    type Output = Vec2;
    
    /// Shorthand for `-&arg`.
    #[inline(always)] fn neg(self) -> Vec2 {
        -&self
    }
}

/// Builder macro for creating new 2-dimensional vectors with floating point components.
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
        let s = $s as f64;
        Vec2::new(s, s)
    }};
    ($x:expr, $y:expr) => {{
        Vec2::new($x as f64, $y as f64)
    }};
    ($x:expr, $y:expr,) => {{
        Vec2::new($x as f64, $y as f64)
    }};
}
