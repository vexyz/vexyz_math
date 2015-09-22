// Generated code.
use vec2::*;
use std::fmt::{Display, Formatter, Result};
use std::ops::*;

/// Column major matrix with 2 columns and 2 rows.
///
/// Most operators and methods on matrices are performed in component-wise fashion. The notable
/// exceptions are matrix-matrix and matrix-vector multiplications.
#[derive(Debug, PartialEq, Copy, Clone)]
pub struct Mat2 { cols: [Vec2; 2] }

impl Mat2 {
    /// Constructs a new `Mat2`.
    pub fn new(col0: Vec2, col1: Vec2) -> Self {
         Mat2 { cols: [col0, col1] }
    }
    
    /// Returns transpose of the matrix.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let a = mat2!(
    ///     0.1, 0.2,
    ///     1.1, 1.2,
    /// );
    /// let b = mat2!(
    ///     0.1, 1.1,
    ///     0.2, 1.2,
    /// );
    /// assert_eq!(a.transpose(), b);
    /// # }
    /// ```
    pub fn transpose(&self) -> Mat2 {
        Mat2::new(
            Vec2::new(self[0][0], self[1][0]),
            Vec2::new(self[0][1], self[1][1]),
        )
    }
}

pub trait Mat2Mat2Ops<Rhs> {
    fn lerp(&self, rhs: Rhs, a: f64) -> Mat2;
}

impl<'a> Mat2Mat2Ops<&'a Mat2> for Mat2 {
    /// Computes linear interpolation `self*(1 - a) + rhs*a` producing a new matrix.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let a = mat2!(
    ///     0.1, 0.2,
    ///     1.1, 1.2,
    /// );
    /// let b = mat2!(
    ///     0.5, 0.6,
    ///     1.5, 1.6,
    /// );
    /// let c = a*(1.0 - 0.25) + b*0.25;
    /// assert_eq!(a.lerp(b, 0.25), c);
    /// # }
    /// ```
    fn lerp(&self, rhs: &Mat2, a: f64) -> Mat2 {
    	let b = 1.0 - a;
        Mat2::new(
            self[0]*b + rhs[0]*a, self[1]*b + rhs[1]*a
        )
    }
}

impl Mat2Mat2Ops<Mat2> for Mat2 {
	/// Shorthand for `lhs.determinant(&rhs)`.
    #[inline(always)] fn lerp(&self, rhs: Mat2, a: f64) -> Mat2 {
        self.lerp(&rhs, a)
    }
}

impl Display for Mat2 {
    fn fmt(&self, f: &mut Formatter) -> Result {
    	write!(f, "Mat2({}, {})", self[0], self[1])
    }
}

impl Index<usize> for Mat2 {
    type Output = Vec2;
    
    /// Index notation for accessing matrix columns.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let m = mat2!(
    ///     vec2!(0.1, 0.2),
    ///     vec2!(1.1, 1.2),
    /// );
    /// assert_eq!(m, mat2!(m[0], m[1]));
    /// # }
    /// ```
    ///
    /// # Panics
    ///
    /// Will panic if the index is greater than 1.
    #[inline(always)] fn index<'a>(&'a self, i: usize) -> &'a Vec2 {
        &self.cols[i]
    }
}

impl<'a, 'b> Add<&'b Mat2> for &'a Mat2 {
    type Output = Mat2;
    
    /// Performs component-wise addition of two matrices producing a new matrix.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let a = mat2!(
    ///     0.1, 0.2,
    ///     1.1, 1.2,
    /// );
    /// let b = mat2!(
    ///     0.5, 0.6,
    ///     1.5, 1.6,
    /// );
    /// let c = mat2!(
    ///     0.1 + 0.5, 0.2 + 0.6,
    ///     1.1 + 1.5, 1.2 + 1.6,
    /// );
    /// assert_eq!(a + b, c);
    /// # }
    /// ```
    fn add(self, rhs: &Mat2) -> Mat2 {
        Mat2::new(
            self[0] + rhs[0], self[1] + rhs[1]
        )
    }
}

impl<'a> Add<Mat2> for &'a Mat2 {
    type Output = Mat2;
    
    /// Shorthand for `lhs + &rhs`.
    #[inline(always)] fn add(self, rhs: Mat2) -> Mat2 {
        self + &rhs
    }
}

impl<'b> Add<&'b Mat2> for Mat2 {
    type Output = Mat2;
    
    /// Shorthand for `&lhs + rhs`.
    #[inline(always)] fn add(self, rhs: &Mat2) -> Mat2 {
        &self + rhs
    }
}

impl Add<Mat2> for Mat2 {
    type Output = Mat2;
    
    /// Shorthand for `&lhs + &rhs`.
    #[inline(always)] fn add(self, rhs: Mat2) -> Mat2 {
        &self + &rhs
    }
}

impl<'a> Add<f64> for &'a Mat2 {
    type Output = Mat2;
    
    /// Adds a scalar to each component of a matrix producing a new matrix.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let a = mat2!(
    ///     0.1, 0.2,
    ///     1.1, 1.2,
    /// );
    /// let b = 2.0;
    /// let c = mat2!(
    ///     0.1 + 2.0, 0.2 + 2.0,
    ///     1.1 + 2.0, 1.2 + 2.0,
    /// );
    /// assert_eq!(a + b, c);
    /// # }
    /// ```
    fn add(self, rhs: f64) -> Mat2 {
        Mat2::new(self[0] + rhs, self[1] + rhs)
    }
}

impl Add<f64> for Mat2 {
    type Output = Mat2;
    
    /// Shorthand for `&lhs + rhs`.
    #[inline(always)] fn add(self, rhs: f64) -> Mat2 {
        &self + rhs
    }
}

impl<'a, 'b> Sub<&'b Mat2> for &'a Mat2 {
    type Output = Mat2;
    
    /// Subtracts each component of the `rhs` matrix from the 
    /// corresponding component of the `lhs` matrix producing a new matrix.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let a = mat2!(
    ///     0.1, 0.2,
    ///     1.1, 1.2,
    /// );
    /// let b = mat2!(
    ///     0.5, 0.6,
    ///     1.5, 1.6,
    /// );
    /// let c = mat2!(
    ///     0.1 - 0.5, 0.2 - 0.6,
    ///     1.1 - 1.5, 1.2 - 1.6,
    /// );
    /// assert_eq!(a - b, c);
    /// # }
    /// ```
    fn sub(self, rhs: &Mat2) -> Mat2 {
        Mat2::new(
            self[0] - rhs[0], self[1] - rhs[1]
        )
    }
}

impl<'a> Sub<Mat2> for &'a Mat2 {
    type Output = Mat2;
    
    /// Shorthand for `lhs - &rhs`.
    #[inline(always)] fn sub(self, rhs: Mat2) -> Mat2 {
        self - &rhs
    }
}

impl<'b> Sub<&'b Mat2> for Mat2 {
    type Output = Mat2;
    
    /// Shorthand for `&lhs - rhs`.
    #[inline(always)] fn sub(self, rhs: &Mat2) -> Mat2 {
        &self - rhs
    }
}

impl Sub<Mat2> for Mat2 {
    type Output = Mat2;
    
    /// Shorthand for `&lhs - &rhs`.
    #[inline(always)] fn sub(self, rhs: Mat2) -> Mat2 {
        &self - &rhs
    }
}

impl<'a> Sub<f64> for &'a Mat2 {
    type Output = Mat2;
    
    /// Subtracts a scalar from each component of a matrix producing a new matrix.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let a = mat2!(
    ///     0.1, 0.2,
    ///     1.1, 1.2,
    /// );
    /// let b = 2.0;
    /// let c = mat2!(
    ///     0.1 - 2.0, 0.2 - 2.0,
    ///     1.1 - 2.0, 1.2 - 2.0,
    /// );
    /// assert_eq!(a - b, c);
    /// # }
    /// ```
    fn sub(self, rhs: f64) -> Mat2 {
        Mat2::new(self[0] - rhs, self[1] - rhs)
    }
}

impl Sub<f64> for Mat2 {
    type Output = Mat2;
    
    /// Shorthand for `&lhs - rhs`.
    #[inline(always)] fn sub(self, rhs: f64) -> Mat2 {
        &self - rhs
    }
}

impl<'a, 'b> Mul<&'b Mat2> for &'a Mat2 {
    type Output = Mat2;
    
    /// Performs algebraic multiplication of two matrices producing a new matrix.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let a = mat2!(
    ///     0.1, 0.2,
    ///     1.1, 1.2,
    /// );
    /// let b = mat2!(
    ///     0.5, 0.6,
    ///     1.5, 1.6,
    /// );
    /// let c = mat2!(
    ///     0.1*0.5 + 1.1*0.6,
    ///     0.2*0.5 + 1.2*0.6,
    ///
    ///     0.1*1.5 + 1.1*1.6,
    ///     0.2*1.5 + 1.2*1.6,
    /// );
    /// assert_eq!(a * b, c);
    /// # }
    /// ```
    fn mul(self, rhs: &Mat2) -> Mat2 {
        let t = self.transpose();
        Mat2::new(
            Vec2::new(t[0].dot(rhs[0]), t[1].dot(rhs[0])),
            Vec2::new(t[0].dot(rhs[1]), t[1].dot(rhs[1])),
        )
    }
}

impl<'a> Mul<Mat2> for &'a Mat2 {
    type Output = Mat2;
    
    /// Shorthand for `lhs * &rhs`.
    #[inline(always)] fn mul(self, rhs: Mat2) -> Mat2 {
        self * &rhs
    }
}

impl<'b> Mul<&'b Mat2> for Mat2 {
    type Output = Mat2;
    
    /// Shorthand for `&lhs * rhs`.
    #[inline(always)] fn mul(self, rhs: &Mat2) -> Mat2 {
        &self * rhs
    }
}

impl Mul<Mat2> for Mat2 {
    type Output = Mat2;
    
    /// Shorthand for `&lhs * &rhs`.
    #[inline(always)] fn mul(self, rhs: Mat2) -> Mat2 {
        &self * &rhs
    }
}

impl<'a, 'b> Mul<&'b Vec2> for &'a Mat2 {
    type Output = Vec2;
    
    /// Performs algebraic multiplication of a matrix by a vector, producing a new vector.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let a = mat2!(
    ///     0.1, 0.2,
    ///     1.1, 1.2,
    /// );
    /// let u = vec2!(0.5, 0.6);
    /// let v = vec2!(
    ///     0.1*0.5 + 1.1*0.6,
    ///     0.2*0.5 + 1.2*0.6,
    /// );
    /// assert_eq!(a * u, v);
    /// # }
    /// ```
    fn mul(self, rhs: &Vec2) -> Vec2 {
        let t = self.transpose();
        Vec2::new(
            t[0].dot(rhs), t[1].dot(rhs),
        )
    }
}

impl<'a> Mul<Vec2> for &'a Mat2 {
    type Output = Vec2;
    
    /// Shorthand for `lhs * &rhs`.
    #[inline(always)] fn mul(self, rhs: Vec2) -> Vec2 {
        self * &rhs
    }
}

impl<'b> Mul<&'b Vec2> for Mat2 {
    type Output = Vec2;
    
    /// Shorthand for `&lhs * rhs`.
    #[inline(always)] fn mul(self, rhs: &Vec2) -> Vec2 {
        &self * rhs
    }
}

impl Mul<Vec2> for Mat2 {
    type Output = Vec2;
    
    /// Shorthand for `&lhs * &rhs`.
    #[inline(always)] fn mul(self, rhs: Vec2) -> Vec2 {
        &self * &rhs
    }
}

impl<'a> Mul<f64> for &'a Mat2 {
    type Output = Mat2;
    
    /// Multiplies each component of a matrix by a scalar producing a new matrix.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let a = mat2!(
    ///     0.1, 0.2,
    ///     1.1, 1.2,
    /// );
    /// let b = 2.0;
    /// let c = mat2!(
    ///     0.1 * 2.0, 0.2 * 2.0,
    ///     1.1 * 2.0, 1.2 * 2.0,
    /// );
    /// assert_eq!(a * b, c);
    /// # }
    /// ```
    fn mul(self, rhs: f64) -> Mat2 {
        Mat2::new(self[0] * rhs, self[1] * rhs)
    }
}

impl Mul<f64> for Mat2 {
    type Output = Mat2;
    
    /// Shorthand for `&lhs * rhs`.
    #[inline(always)] fn mul(self, rhs: f64) -> Mat2 {
        &self * rhs
    }
}

impl<'a, 'b> Div<&'b Mat2> for &'a Mat2 {
    type Output = Mat2;
    
    /// Divides each component of the `lhs` matrix by the 
    /// corresponding component of the `rhs` matrix producing a new matrix.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let a = mat2!(
    ///     0.1, 0.2,
    ///     1.1, 1.2,
    /// );
    /// let b = mat2!(
    ///     0.5, 0.6,
    ///     1.5, 1.6,
    /// );
    /// let c = mat2!(
    ///     0.1 / 0.5, 0.2 / 0.6,
    ///     1.1 / 1.5, 1.2 / 1.6,
    /// );
    /// assert_eq!(a / b, c);
    /// # }
    /// ```
    fn div(self, rhs: &Mat2) -> Mat2 {
        Mat2::new(
            self[0] / rhs[0], self[1] / rhs[1]
        )
    }
}

impl<'a> Div<Mat2> for &'a Mat2 {
    type Output = Mat2;
    
    /// Shorthand for `lhs / &rhs`.
    #[inline(always)] fn div(self, rhs: Mat2) -> Mat2 {
        self / &rhs
    }
}

impl<'b> Div<&'b Mat2> for Mat2 {
    type Output = Mat2;
    
    /// Shorthand for `&lhs / rhs`.
    #[inline(always)] fn div(self, rhs: &Mat2) -> Mat2 {
        &self / rhs
    }
}

impl Div<Mat2> for Mat2 {
    type Output = Mat2;
    
    /// Shorthand for `&lhs / &rhs`.
    #[inline(always)] fn div(self, rhs: Mat2) -> Mat2 {
        &self / &rhs
    }
}

impl<'a> Div<f64> for &'a Mat2 {
    type Output = Mat2;
    
    /// Divides each component of a {doc_name} by a scalar producing a new matrix.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let a = mat2!(
    ///     0.1, 0.2,
    ///     1.1, 1.2,
    /// );
    /// let b = 2.0;
    /// let c = mat2!(
    ///     0.1 / 2.0, 0.2 / 2.0,
    ///     1.1 / 2.0, 1.2 / 2.0,
    /// );
    /// assert_eq!(a / b, c);
    /// # }
    /// ```
    fn div(self, rhs: f64) -> Mat2 {
        Mat2::new(self[0] / rhs, self[1] / rhs)
    }
}

impl Div<f64> for Mat2 {
    type Output = Mat2;
    
    /// Shorthand for `&lhs / rhs`.
    #[inline(always)] fn div(self, rhs: f64) -> Mat2 {
        &self / rhs
    }
}

impl<'a> Neg for &'a Mat2 {
    type Output = Mat2;
    
    /// Applies negation to each component of a matrix producing a new matrix.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let a = -mat2!(
    ///     0.1, 0.2,
    ///     1.1, 1.2,
    /// );
    /// let b = mat2!(
    ///     -0.1, -0.2,
    ///     -1.1, -1.2,
    /// );
    /// assert_eq!(a, b);
    /// # }
    /// ```
    fn neg(self) -> Mat2 {
        Mat2::new(-self[0], -self[1])
    }
}

impl Neg for Mat2 {
    type Output = Mat2;
    
    /// Shorthand for `-&arg`.
    #[inline(always)] fn neg(self) -> Mat2 {
        -&self
    }
}

/// Builder macro for creating new 2x2 column major matrices.
///
/// # Examples
///
/// Create a new column major matrix with `0.5` on diagonal:
///
/// ```
/// #[macro_use] extern crate vexyz_math;
/// use vexyz_math::*;
///
/// # fn main() {
/// let a = mat2!(0.5);
/// let b = Mat2::new(
///     vec2!(0.5, 0.0),
///     vec2!(0.0, 0.5),
/// );
/// assert_eq!(a, b);
/// # }
/// ```
///
/// Create a new column major matrix with
///     `column0 = vec2(0.5, 0.6)`,
///     `column1 = vec2(1.5, 1.6)`:
///
/// ```
/// #[macro_use] extern crate vexyz_math;
/// use vexyz_math::*;
///
/// # fn main() {
/// let a = mat2!(
///     vec2!(0.5, 0.6),
///     vec2!(1.5, 1.6),
/// );
/// let b = Mat2::new(
///     vec2!(0.5, 0.6),
///     vec2!(1.5, 1.6),
/// );
/// assert_eq!(a, b);
/// # }
/// ```
///
/// Create a new column major matrix with
///     `column0 = vec2(0.5, 0.6)`,
///     `column1 = vec2(1.5, 1.6)`:
///
/// ```
/// #[macro_use] extern crate vexyz_math;
/// use vexyz_math::*;
///
/// # fn main() {
/// let a = mat2!(
///     0.5, 0.6,
///     1.5, 1.6,
/// );
/// let b = Mat2::new(
///     vec2!(0.5, 0.6),
///     vec2!(1.5, 1.6),
/// );
/// assert_eq!(a, b);
/// # }
/// ```
#[macro_export]
macro_rules! mat2 {
    ($s:expr) => {{
        let s = $s as f64;
        Mat2::new(
            vec2!(s, 0),
            vec2!(0, s),
        )
    }};
    ($col0:expr, $col1:expr) => {{
        Mat2::new($col0, $col1)
    }};
    ($col0:expr, $col1:expr,) => {{
        Mat2::new($col0, $col1)
    }};
    ($m00:expr, $m01:expr,
     $m10:expr, $m11:expr) => {{
        Mat2::new(
            vec2!($m00 as f64, $m01 as f64),
            vec2!($m10 as f64, $m11 as f64),
        )
    }};
    ($m00:expr, $m01:expr,
     $m10:expr, $m11:expr,) => {{
        Mat2::new(
            vec2!($m00 as f64, $m01 as f64),
            vec2!($m10 as f64, $m11 as f64),
        )
    }};
}
