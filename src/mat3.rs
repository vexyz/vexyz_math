// Generated code.
use vec3::*;
use std::fmt::{Display, Formatter, Result};
use std::ops::*;

/// Column major matrix with 3 columns and 3 rows.
///
/// Most operators and methods on matrices are performed in component-wise fashion. The notable
/// exceptions are matrix-matrix and matrix-vector multiplications.
#[derive(Debug, PartialEq, Copy, Clone)]
pub struct Mat3 { cols: [Vec3; 3] }

impl Mat3 {
    /// Constructs a new `Mat3`.
    pub fn new(col0: Vec3, col1: Vec3, col2: Vec3) -> Self {
         Mat3 { cols: [col0, col1, col2] }
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
    /// let a = mat3!(
    ///     11.0, 12.0, 13.0,
    ///     21.0, 22.0, 23.0,
    ///     31.0, 32.0, 33.0,
    /// );
    /// let b = mat3!(
    ///     11.0, 21.0, 31.0,
    ///     12.0, 22.0, 32.0,
    ///     13.0, 23.0, 33.0,
    /// );
    /// assert_eq!(a.transpose(), b);
    /// # }
    /// ```
    pub fn transpose(&self) -> Mat3 {
        Mat3::new(
            Vec3::new(self[0].x(), self[1].x(), self[2].x()),
            Vec3::new(self[0].y(), self[1].y(), self[2].y()),
            Vec3::new(self[0].z(), self[1].z(), self[2].z()),
        )
    }
}

pub trait Mat3Mat3Ops<Rhs> {
    fn lerp(&self, rhs: Rhs, a: f32) -> Mat3;
}

impl<'a> Mat3Mat3Ops<&'a Mat3> for Mat3 {
    /// Computes linear interpolation `self*(1 - a) + rhs*a` producing a new matrix.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let a = mat3!(
    ///     11.0, 12.0, 13.0,
    ///     21.0, 22.0, 23.0,
    ///     31.0, 32.0, 33.0,
    /// );
    /// let b = mat3!(
    ///     15.0, 16.0, 17.0,
    ///     25.0, 26.0, 27.0,
    ///     35.0, 36.0, 37.0,
    /// );
    /// let c = a*(1.0 - 0.25) + b*0.25;
    /// assert_eq!(a.lerp(b, 0.25), c);
    /// # }
    /// ```
    fn lerp(&self, rhs: &Mat3, a: f32) -> Mat3 {
        let b = 1.0 - a;
        Mat3::new(
            self[0]*b + rhs[0]*a, self[1]*b + rhs[1]*a, self[2]*b + rhs[2]*a
        )
    }
}

impl Mat3Mat3Ops<Mat3> for Mat3 {
    /// Shorthand for `lhs.determinant(&rhs)`.
    #[inline(always)] fn lerp(&self, rhs: Mat3, a: f32) -> Mat3 {
        self.lerp(&rhs, a)
    }
}

impl Display for Mat3 {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "Mat3({}, {}, {})", self[0], self[1], self[2])
    }
}

impl Index<usize> for Mat3 {
    type Output = Vec3;
    
    /// Index notation for accessing matrix columns.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let m = mat3!(
    ///     vec3!(11.0, 12.0, 13.0),
    ///     vec3!(21.0, 22.0, 23.0),
    ///     vec3!(31.0, 32.0, 33.0),
    /// );
    /// assert_eq!(m, mat3!(m[0], m[1], m[2]));
    /// # }
    /// ```
    ///
    /// # Panics
    ///
    /// Will panic if the index is greater than 2.
    #[inline(always)] fn index<'a>(&'a self, i: usize) -> &'a Vec3 {
        &self.cols[i]
    }
}

impl IndexMut<usize> for Mat3 {
    
    /// Index notation for mutating matrix columns.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let mut a = mat3!(
    ///     vec3!(11.0, 12.0, 13.0),
    ///     vec3!(21.0, 22.0, 23.0),
    ///     vec3!(31.0, 32.0, 33.0),
    /// );
    /// a[0] = vec3!(15.0, 16.0, 17.0);
    /// a[1] = vec3!(25.0, 26.0, 27.0);
    /// a[2] = vec3!(35.0, 36.0, 37.0);
    ///
    /// let b = mat3!(
    ///     vec3!(15.0, 16.0, 17.0),
    ///     vec3!(25.0, 26.0, 27.0),
    ///     vec3!(35.0, 36.0, 37.0),
    /// );
    /// assert_eq!(a, b);
    /// # }
    /// ```
    ///
    /// # Panics
    ///
    /// Will panic if the index is greater than 2.
    #[inline(always)] fn index_mut<'a>(&'a mut self, i: usize) -> &'a mut Vec3 {
        &mut self.cols[i]
    }
}

impl<'a, 'b> Add<&'b Mat3> for &'a Mat3 {
    type Output = Mat3;
    
    /// Performs component-wise addition of two matrices producing a new matrix.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let a = mat3!(
    ///     11.0, 12.0, 13.0,
    ///     21.0, 22.0, 23.0,
    ///     31.0, 32.0, 33.0,
    /// );
    /// let b = mat3!(
    ///     15.0, 16.0, 17.0,
    ///     25.0, 26.0, 27.0,
    ///     35.0, 36.0, 37.0,
    /// );
    /// let c = mat3!(
    ///     11.0 + 15.0, 12.0 + 16.0, 13.0 + 17.0,
    ///     21.0 + 25.0, 22.0 + 26.0, 23.0 + 27.0,
    ///     31.0 + 35.0, 32.0 + 36.0, 33.0 + 37.0,
    /// );
    /// assert_eq!(a + b, c);
    /// # }
    /// ```
    fn add(self, rhs: &Mat3) -> Mat3 {
        Mat3::new(
            self[0] + rhs[0], self[1] + rhs[1], self[2] + rhs[2]
        )
    }
}

impl<'a> Add<Mat3> for &'a Mat3 {
    type Output = Mat3;
    
    /// Shorthand for `lhs + &rhs`.
    #[inline(always)] fn add(self, rhs: Mat3) -> Mat3 {
        self + &rhs
    }
}

impl<'b> Add<&'b Mat3> for Mat3 {
    type Output = Mat3;
    
    /// Shorthand for `&lhs + rhs`.
    #[inline(always)] fn add(self, rhs: &Mat3) -> Mat3 {
        &self + rhs
    }
}

impl Add<Mat3> for Mat3 {
    type Output = Mat3;
    
    /// Shorthand for `&lhs + &rhs`.
    #[inline(always)] fn add(self, rhs: Mat3) -> Mat3 {
        &self + &rhs
    }
}

impl<'a> Add<f32> for &'a Mat3 {
    type Output = Mat3;
    
    /// Adds a scalar to each component of a matrix producing a new matrix.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let a = mat3!(
    ///     11.0, 12.0, 13.0,
    ///     21.0, 22.0, 23.0,
    ///     31.0, 32.0, 33.0,
    /// );
    /// let b = 2.0;
    /// let c = mat3!(
    ///     11.0 + 2.0, 12.0 + 2.0, 13.0 + 2.0,
    ///     21.0 + 2.0, 22.0 + 2.0, 23.0 + 2.0,
    ///     31.0 + 2.0, 32.0 + 2.0, 33.0 + 2.0,
    /// );
    /// assert_eq!(a + b, c);
    /// # }
    /// ```
    fn add(self, rhs: f32) -> Mat3 {
        Mat3::new(self[0] + rhs, self[1] + rhs, self[2] + rhs)
    }
}

impl Add<f32> for Mat3 {
    type Output = Mat3;
    
    /// Shorthand for `&lhs + rhs`.
    #[inline(always)] fn add(self, rhs: f32) -> Mat3 {
        &self + rhs
    }
}

impl<'a, 'b> Sub<&'b Mat3> for &'a Mat3 {
    type Output = Mat3;
    
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
    /// let a = mat3!(
    ///     11.0, 12.0, 13.0,
    ///     21.0, 22.0, 23.0,
    ///     31.0, 32.0, 33.0,
    /// );
    /// let b = mat3!(
    ///     15.0, 16.0, 17.0,
    ///     25.0, 26.0, 27.0,
    ///     35.0, 36.0, 37.0,
    /// );
    /// let c = mat3!(
    ///     11.0 - 15.0, 12.0 - 16.0, 13.0 - 17.0,
    ///     21.0 - 25.0, 22.0 - 26.0, 23.0 - 27.0,
    ///     31.0 - 35.0, 32.0 - 36.0, 33.0 - 37.0,
    /// );
    /// assert_eq!(a - b, c);
    /// # }
    /// ```
    fn sub(self, rhs: &Mat3) -> Mat3 {
        Mat3::new(
            self[0] - rhs[0], self[1] - rhs[1], self[2] - rhs[2]
        )
    }
}

impl<'a> Sub<Mat3> for &'a Mat3 {
    type Output = Mat3;
    
    /// Shorthand for `lhs - &rhs`.
    #[inline(always)] fn sub(self, rhs: Mat3) -> Mat3 {
        self - &rhs
    }
}

impl<'b> Sub<&'b Mat3> for Mat3 {
    type Output = Mat3;
    
    /// Shorthand for `&lhs - rhs`.
    #[inline(always)] fn sub(self, rhs: &Mat3) -> Mat3 {
        &self - rhs
    }
}

impl Sub<Mat3> for Mat3 {
    type Output = Mat3;
    
    /// Shorthand for `&lhs - &rhs`.
    #[inline(always)] fn sub(self, rhs: Mat3) -> Mat3 {
        &self - &rhs
    }
}

impl<'a> Sub<f32> for &'a Mat3 {
    type Output = Mat3;
    
    /// Subtracts a scalar from each component of a matrix producing a new matrix.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let a = mat3!(
    ///     11.0, 12.0, 13.0,
    ///     21.0, 22.0, 23.0,
    ///     31.0, 32.0, 33.0,
    /// );
    /// let b = 2.0;
    /// let c = mat3!(
    ///     11.0 - 2.0, 12.0 - 2.0, 13.0 - 2.0,
    ///     21.0 - 2.0, 22.0 - 2.0, 23.0 - 2.0,
    ///     31.0 - 2.0, 32.0 - 2.0, 33.0 - 2.0,
    /// );
    /// assert_eq!(a - b, c);
    /// # }
    /// ```
    fn sub(self, rhs: f32) -> Mat3 {
        Mat3::new(self[0] - rhs, self[1] - rhs, self[2] - rhs)
    }
}

impl Sub<f32> for Mat3 {
    type Output = Mat3;
    
    /// Shorthand for `&lhs - rhs`.
    #[inline(always)] fn sub(self, rhs: f32) -> Mat3 {
        &self - rhs
    }
}

impl<'a, 'b> Mul<&'b Mat3> for &'a Mat3 {
    type Output = Mat3;
    
    /// Performs algebraic multiplication of two matrices producing a new matrix.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let a = mat3!(
    ///     11.0, 12.0, 13.0,
    ///     21.0, 22.0, 23.0,
    ///     31.0, 32.0, 33.0,
    /// );
    /// let b = mat3!(
    ///     15.0, 16.0, 17.0,
    ///     25.0, 26.0, 27.0,
    ///     35.0, 36.0, 37.0,
    /// );
    /// let c = mat3!(
    ///     11.0*15.0 + 21.0*16.0 + 31.0*17.0,
    ///     12.0*15.0 + 22.0*16.0 + 32.0*17.0,
    ///     13.0*15.0 + 23.0*16.0 + 33.0*17.0,
    ///
    ///     11.0*25.0 + 21.0*26.0 + 31.0*27.0,
    ///     12.0*25.0 + 22.0*26.0 + 32.0*27.0,
    ///     13.0*25.0 + 23.0*26.0 + 33.0*27.0,
    ///
    ///     11.0*35.0 + 21.0*36.0 + 31.0*37.0,
    ///     12.0*35.0 + 22.0*36.0 + 32.0*37.0,
    ///     13.0*35.0 + 23.0*36.0 + 33.0*37.0,
    /// );
    /// assert_eq!(a * b, c);
    /// # }
    /// ```
    fn mul(self, rhs: &Mat3) -> Mat3 {
        let t = self.transpose();
        Mat3::new(
            Vec3::new(t[0].dot(rhs[0]), t[1].dot(rhs[0]), t[2].dot(rhs[0])),
            Vec3::new(t[0].dot(rhs[1]), t[1].dot(rhs[1]), t[2].dot(rhs[1])),
            Vec3::new(t[0].dot(rhs[2]), t[1].dot(rhs[2]), t[2].dot(rhs[2])),
        )
    }
}

impl<'a> Mul<Mat3> for &'a Mat3 {
    type Output = Mat3;
    
    /// Shorthand for `lhs * &rhs`.
    #[inline(always)] fn mul(self, rhs: Mat3) -> Mat3 {
        self * &rhs
    }
}

impl<'b> Mul<&'b Mat3> for Mat3 {
    type Output = Mat3;
    
    /// Shorthand for `&lhs * rhs`.
    #[inline(always)] fn mul(self, rhs: &Mat3) -> Mat3 {
        &self * rhs
    }
}

impl Mul<Mat3> for Mat3 {
    type Output = Mat3;
    
    /// Shorthand for `&lhs * &rhs`.
    #[inline(always)] fn mul(self, rhs: Mat3) -> Mat3 {
        &self * &rhs
    }
}

impl<'a, 'b> Mul<&'b Vec3> for &'a Mat3 {
    type Output = Vec3;
    
    /// Performs algebraic multiplication of a matrix by a vector, producing a new vector.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let a = mat3!(
    ///     11.0, 12.0, 13.0,
    ///     21.0, 22.0, 23.0,
    ///     31.0, 32.0, 33.0,
    /// );
    /// let u = vec3!(15.0, 16.0, 17.0);
    /// let v = vec3!(
    ///     11.0*15.0 + 21.0*16.0 + 31.0*17.0,
    ///     12.0*15.0 + 22.0*16.0 + 32.0*17.0,
    ///     13.0*15.0 + 23.0*16.0 + 33.0*17.0,
    /// );
    /// assert_eq!(a * u, v);
    /// # }
    /// ```
    fn mul(self, rhs: &Vec3) -> Vec3 {
        let t = self.transpose();
        Vec3::new(
            t[0].dot(rhs), t[1].dot(rhs), t[2].dot(rhs),
        )
    }
}

impl<'a> Mul<Vec3> for &'a Mat3 {
    type Output = Vec3;
    
    /// Shorthand for `lhs * &rhs`.
    #[inline(always)] fn mul(self, rhs: Vec3) -> Vec3 {
        self * &rhs
    }
}

impl<'b> Mul<&'b Vec3> for Mat3 {
    type Output = Vec3;
    
    /// Shorthand for `&lhs * rhs`.
    #[inline(always)] fn mul(self, rhs: &Vec3) -> Vec3 {
        &self * rhs
    }
}

impl Mul<Vec3> for Mat3 {
    type Output = Vec3;
    
    /// Shorthand for `&lhs * &rhs`.
    #[inline(always)] fn mul(self, rhs: Vec3) -> Vec3 {
        &self * &rhs
    }
}

impl<'a> Mul<f32> for &'a Mat3 {
    type Output = Mat3;
    
    /// Multiplies each component of a matrix by a scalar producing a new matrix.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let a = mat3!(
    ///     11.0, 12.0, 13.0,
    ///     21.0, 22.0, 23.0,
    ///     31.0, 32.0, 33.0,
    /// );
    /// let b = 2.0;
    /// let c = mat3!(
    ///     11.0 * 2.0, 12.0 * 2.0, 13.0 * 2.0,
    ///     21.0 * 2.0, 22.0 * 2.0, 23.0 * 2.0,
    ///     31.0 * 2.0, 32.0 * 2.0, 33.0 * 2.0,
    /// );
    /// assert_eq!(a * b, c);
    /// # }
    /// ```
    fn mul(self, rhs: f32) -> Mat3 {
        Mat3::new(self[0] * rhs, self[1] * rhs, self[2] * rhs)
    }
}

impl Mul<f32> for Mat3 {
    type Output = Mat3;
    
    /// Shorthand for `&lhs * rhs`.
    #[inline(always)] fn mul(self, rhs: f32) -> Mat3 {
        &self * rhs
    }
}

impl<'a, 'b> Div<&'b Mat3> for &'a Mat3 {
    type Output = Mat3;
    
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
    /// let a = mat3!(
    ///     11.0, 12.0, 13.0,
    ///     21.0, 22.0, 23.0,
    ///     31.0, 32.0, 33.0,
    /// );
    /// let b = mat3!(
    ///     15.0, 16.0, 17.0,
    ///     25.0, 26.0, 27.0,
    ///     35.0, 36.0, 37.0,
    /// );
    /// let c = mat3!(
    ///     11.0 / 15.0, 12.0 / 16.0, 13.0 / 17.0,
    ///     21.0 / 25.0, 22.0 / 26.0, 23.0 / 27.0,
    ///     31.0 / 35.0, 32.0 / 36.0, 33.0 / 37.0,
    /// );
    /// assert_eq!(a / b, c);
    /// # }
    /// ```
    fn div(self, rhs: &Mat3) -> Mat3 {
        Mat3::new(
            self[0] / rhs[0], self[1] / rhs[1], self[2] / rhs[2]
        )
    }
}

impl<'a> Div<Mat3> for &'a Mat3 {
    type Output = Mat3;
    
    /// Shorthand for `lhs / &rhs`.
    #[inline(always)] fn div(self, rhs: Mat3) -> Mat3 {
        self / &rhs
    }
}

impl<'b> Div<&'b Mat3> for Mat3 {
    type Output = Mat3;
    
    /// Shorthand for `&lhs / rhs`.
    #[inline(always)] fn div(self, rhs: &Mat3) -> Mat3 {
        &self / rhs
    }
}

impl Div<Mat3> for Mat3 {
    type Output = Mat3;
    
    /// Shorthand for `&lhs / &rhs`.
    #[inline(always)] fn div(self, rhs: Mat3) -> Mat3 {
        &self / &rhs
    }
}

impl<'a> Div<f32> for &'a Mat3 {
    type Output = Mat3;
    
    /// Divides each component of a {doc_name} by a scalar producing a new matrix.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let a = mat3!(
    ///     11.0, 12.0, 13.0,
    ///     21.0, 22.0, 23.0,
    ///     31.0, 32.0, 33.0,
    /// );
    /// let b = 2.0;
    /// let c = mat3!(
    ///     11.0 / 2.0, 12.0 / 2.0, 13.0 / 2.0,
    ///     21.0 / 2.0, 22.0 / 2.0, 23.0 / 2.0,
    ///     31.0 / 2.0, 32.0 / 2.0, 33.0 / 2.0,
    /// );
    /// assert_eq!(a / b, c);
    /// # }
    /// ```
    fn div(self, rhs: f32) -> Mat3 {
        Mat3::new(self[0] / rhs, self[1] / rhs, self[2] / rhs)
    }
}

impl Div<f32> for Mat3 {
    type Output = Mat3;
    
    /// Shorthand for `&lhs / rhs`.
    #[inline(always)] fn div(self, rhs: f32) -> Mat3 {
        &self / rhs
    }
}

impl<'a> Neg for &'a Mat3 {
    type Output = Mat3;
    
    /// Applies negation to each component of a matrix producing a new matrix.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let a = -mat3!(
    ///     11.0, 12.0, 13.0,
    ///     21.0, 22.0, 23.0,
    ///     31.0, 32.0, 33.0,
    /// );
    /// let b = mat3!(
    ///     -11.0, -12.0, -13.0,
    ///     -21.0, -22.0, -23.0,
    ///     -31.0, -32.0, -33.0,
    /// );
    /// assert_eq!(a, b);
    /// # }
    /// ```
    fn neg(self) -> Mat3 {
        Mat3::new(-self[0], -self[1], -self[2])
    }
}

impl Neg for Mat3 {
    type Output = Mat3;
    
    /// Shorthand for `-&arg`.
    #[inline(always)] fn neg(self) -> Mat3 {
        -&self
    }
}

/// Builder macro for creating new 3x3 column major matrices.
///
/// # Examples
///
/// Create a new column major matrix with `15.0` on diagonal:
///
/// ```
/// #[macro_use] extern crate vexyz_math;
/// use vexyz_math::*;
///
/// # fn main() {
/// let a = mat3!(15.0);
/// let b = Mat3::new(
///     vec3!(15.0, 0.0, 0.0),
///     vec3!(0.0, 15.0, 0.0),
///     vec3!(0.0, 0.0, 15.0),
/// );
/// assert_eq!(a, b);
/// # }
/// ```
///
/// Create a new column major matrix with
///     `column0 = vec3(15.0, 16.0, 17.0)`,
///     `column1 = vec3(25.0, 26.0, 27.0)`,
///     `column2 = vec3(35.0, 36.0, 37.0)`:
///
/// ```
/// #[macro_use] extern crate vexyz_math;
/// use vexyz_math::*;
///
/// # fn main() {
/// let a = mat3!(
///     vec3!(15.0, 16.0, 17.0),
///     vec3!(25.0, 26.0, 27.0),
///     vec3!(35.0, 36.0, 37.0),
/// );
/// let b = Mat3::new(
///     vec3!(15.0, 16.0, 17.0),
///     vec3!(25.0, 26.0, 27.0),
///     vec3!(35.0, 36.0, 37.0),
/// );
/// assert_eq!(a, b);
/// # }
/// ```
///
/// Create a new column major matrix with
///     `column0 = vec3(15.0, 16.0, 17.0)`,
///     `column1 = vec3(25.0, 26.0, 27.0)`,
///     `column2 = vec3(35.0, 36.0, 37.0)`:
///
/// ```
/// #[macro_use] extern crate vexyz_math;
/// use vexyz_math::*;
///
/// # fn main() {
/// let a = mat3!(
///     15.0, 16.0, 17.0,
///     25.0, 26.0, 27.0,
///     35.0, 36.0, 37.0,
/// );
/// let b = Mat3::new(
///     vec3!(15.0, 16.0, 17.0),
///     vec3!(25.0, 26.0, 27.0),
///     vec3!(35.0, 36.0, 37.0),
/// );
/// assert_eq!(a, b);
/// # }
/// ```
#[macro_export]
macro_rules! mat3 {
    ($s:expr) => {{
        let s = $s as f32;
        Mat3::new(
            vec3!(s, 0, 0),
            vec3!(0, s, 0),
            vec3!(0, 0, s),
        )
    }};
    ($col0:expr, $col1:expr, $col2:expr) => {{
        Mat3::new($col0, $col1, $col2)
    }};
    ($col0:expr, $col1:expr, $col2:expr,) => {{
        Mat3::new($col0, $col1, $col2)
    }};
    ($m00:expr, $m01:expr, $m02:expr,
     $m10:expr, $m11:expr, $m12:expr,
     $m20:expr, $m21:expr, $m22:expr) => {{
        Mat3::new(
            vec3!($m00 as f32, $m01 as f32, $m02 as f32),
            vec3!($m10 as f32, $m11 as f32, $m12 as f32),
            vec3!($m20 as f32, $m21 as f32, $m22 as f32),
        )
    }};
    ($m00:expr, $m01:expr, $m02:expr,
     $m10:expr, $m11:expr, $m12:expr,
     $m20:expr, $m21:expr, $m22:expr,) => {{
        Mat3::new(
            vec3!($m00 as f32, $m01 as f32, $m02 as f32),
            vec3!($m10 as f32, $m11 as f32, $m12 as f32),
            vec3!($m20 as f32, $m21 as f32, $m22 as f32),
        )
    }};
}
