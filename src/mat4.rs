// Generated code.
use vec4::*;
use std::fmt::{Display, Formatter, Result};
use std::ops::*;

/// Column major matrix with 4 columns and 4 rows.
///
/// Most operators and methods on matrices are performed in component-wise fashion. The notable
/// exceptions are matrix-matrix and matrix-vector multiplications.
#[derive(Debug, PartialEq, Copy, Clone)]
pub struct Mat4 { cols: [Vec4; 4] }

impl Mat4 {
    /// Constructs a new `Mat4`.
    pub fn new(col0: Vec4, col1: Vec4, col2: Vec4, col3: Vec4) -> Self {
         Mat4 { cols: [col0, col1, col2, col3] }
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
    /// let a = mat4!(
    ///     11.0, 12.0, 13.0, 14.0,
    ///     21.0, 22.0, 23.0, 24.0,
    ///     31.0, 32.0, 33.0, 34.0,
    ///     41.0, 42.0, 43.0, 44.0,
    /// );
    /// let b = mat4!(
    ///     11.0, 21.0, 31.0, 41.0,
    ///     12.0, 22.0, 32.0, 42.0,
    ///     13.0, 23.0, 33.0, 43.0,
    ///     14.0, 24.0, 34.0, 44.0,
    /// );
    /// assert_eq!(a.transpose(), b);
    /// # }
    /// ```
    pub fn transpose(&self) -> Mat4 {
        Mat4::new(
            Vec4::new(self[0].x(), self[1].x(), self[2].x(), self[3].x()),
            Vec4::new(self[0].y(), self[1].y(), self[2].y(), self[3].y()),
            Vec4::new(self[0].z(), self[1].z(), self[2].z(), self[3].z()),
            Vec4::new(self[0].w(), self[1].w(), self[2].w(), self[3].w()),
        )
    }
}

pub trait Mat4Mat4Ops<Rhs> {
    fn lerp(&self, rhs: Rhs, a: f32) -> Mat4;
}

impl<'a> Mat4Mat4Ops<&'a Mat4> for Mat4 {
    /// Computes linear interpolation `self*(1 - a) + rhs*a` producing a new matrix.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let a = mat4!(
    ///     11.0, 12.0, 13.0, 14.0,
    ///     21.0, 22.0, 23.0, 24.0,
    ///     31.0, 32.0, 33.0, 34.0,
    ///     41.0, 42.0, 43.0, 44.0,
    /// );
    /// let b = mat4!(
    ///     15.0, 16.0, 17.0, 18.0,
    ///     25.0, 26.0, 27.0, 28.0,
    ///     35.0, 36.0, 37.0, 38.0,
    ///     45.0, 46.0, 47.0, 48.0,
    /// );
    /// let c = a*(1.0 - 0.25) + b*0.25;
    /// assert_eq!(a.lerp(b, 0.25), c);
    /// # }
    /// ```
    fn lerp(&self, rhs: &Mat4, a: f32) -> Mat4 {
        let b = 1.0 - a;
        Mat4::new(
            self[0]*b + rhs[0]*a, self[1]*b + rhs[1]*a, self[2]*b + rhs[2]*a, self[3]*b + rhs[3]*a
        )
    }
}

impl Mat4Mat4Ops<Mat4> for Mat4 {
    /// Shorthand for `lhs.determinant(&rhs)`.
    #[inline(always)] fn lerp(&self, rhs: Mat4, a: f32) -> Mat4 {
        self.lerp(&rhs, a)
    }
}

impl Display for Mat4 {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "Mat4({}, {}, {}, {})", self[0], self[1], self[2], self[3])
    }
}

impl Index<usize> for Mat4 {
    type Output = Vec4;
    
    /// Index notation for accessing matrix columns.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let m = mat4!(
    ///     vec4!(11.0, 12.0, 13.0, 14.0),
    ///     vec4!(21.0, 22.0, 23.0, 24.0),
    ///     vec4!(31.0, 32.0, 33.0, 34.0),
    ///     vec4!(41.0, 42.0, 43.0, 44.0),
    /// );
    /// assert_eq!(m, mat4!(m[0], m[1], m[2], m[3]));
    /// # }
    /// ```
    ///
    /// # Panics
    ///
    /// Will panic if the index is greater than 3.
    #[inline(always)] fn index<'a>(&'a self, i: usize) -> &'a Vec4 {
        &self.cols[i]
    }
}

impl IndexMut<usize> for Mat4 {
    
    /// Index notation for mutating matrix columns.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let mut a = mat4!(
    ///     vec4!(11.0, 12.0, 13.0, 14.0),
    ///     vec4!(21.0, 22.0, 23.0, 24.0),
    ///     vec4!(31.0, 32.0, 33.0, 34.0),
    ///     vec4!(41.0, 42.0, 43.0, 44.0),
    /// );
    /// a[0] = vec4!(15.0, 16.0, 17.0, 18.0);
    /// a[1] = vec4!(25.0, 26.0, 27.0, 28.0);
    /// a[2] = vec4!(35.0, 36.0, 37.0, 38.0);
    /// a[3] = vec4!(45.0, 46.0, 47.0, 48.0);
    ///
    /// let b = mat4!(
    ///     vec4!(15.0, 16.0, 17.0, 18.0),
    ///     vec4!(25.0, 26.0, 27.0, 28.0),
    ///     vec4!(35.0, 36.0, 37.0, 38.0),
    ///     vec4!(45.0, 46.0, 47.0, 48.0),
    /// );
    /// assert_eq!(a, b);
    /// # }
    /// ```
    ///
    /// # Panics
    ///
    /// Will panic if the index is greater than 3.
    #[inline(always)] fn index_mut<'a>(&'a mut self, i: usize) -> &'a mut Vec4 {
        &mut self.cols[i]
    }
}

impl<'a, 'b> Add<&'b Mat4> for &'a Mat4 {
    type Output = Mat4;
    
    /// Performs component-wise addition of two matrices producing a new matrix.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let a = mat4!(
    ///     11.0, 12.0, 13.0, 14.0,
    ///     21.0, 22.0, 23.0, 24.0,
    ///     31.0, 32.0, 33.0, 34.0,
    ///     41.0, 42.0, 43.0, 44.0,
    /// );
    /// let b = mat4!(
    ///     15.0, 16.0, 17.0, 18.0,
    ///     25.0, 26.0, 27.0, 28.0,
    ///     35.0, 36.0, 37.0, 38.0,
    ///     45.0, 46.0, 47.0, 48.0,
    /// );
    /// let c = mat4!(
    ///     11.0 + 15.0, 12.0 + 16.0, 13.0 + 17.0, 14.0 + 18.0,
    ///     21.0 + 25.0, 22.0 + 26.0, 23.0 + 27.0, 24.0 + 28.0,
    ///     31.0 + 35.0, 32.0 + 36.0, 33.0 + 37.0, 34.0 + 38.0,
    ///     41.0 + 45.0, 42.0 + 46.0, 43.0 + 47.0, 44.0 + 48.0,
    /// );
    /// assert_eq!(a + b, c);
    /// # }
    /// ```
    fn add(self, rhs: &Mat4) -> Mat4 {
        Mat4::new(
            self[0] + rhs[0], self[1] + rhs[1], self[2] + rhs[2], self[3] + rhs[3]
        )
    }
}

impl<'a> Add<Mat4> for &'a Mat4 {
    type Output = Mat4;
    
    /// Shorthand for `lhs + &rhs`.
    #[inline(always)] fn add(self, rhs: Mat4) -> Mat4 {
        self + &rhs
    }
}

impl<'b> Add<&'b Mat4> for Mat4 {
    type Output = Mat4;
    
    /// Shorthand for `&lhs + rhs`.
    #[inline(always)] fn add(self, rhs: &Mat4) -> Mat4 {
        &self + rhs
    }
}

impl Add<Mat4> for Mat4 {
    type Output = Mat4;
    
    /// Shorthand for `&lhs + &rhs`.
    #[inline(always)] fn add(self, rhs: Mat4) -> Mat4 {
        &self + &rhs
    }
}

impl<'a> Add<f32> for &'a Mat4 {
    type Output = Mat4;
    
    /// Adds a scalar to each component of a matrix producing a new matrix.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let a = mat4!(
    ///     11.0, 12.0, 13.0, 14.0,
    ///     21.0, 22.0, 23.0, 24.0,
    ///     31.0, 32.0, 33.0, 34.0,
    ///     41.0, 42.0, 43.0, 44.0,
    /// );
    /// let b = 2.0;
    /// let c = mat4!(
    ///     11.0 + 2.0, 12.0 + 2.0, 13.0 + 2.0, 14.0 + 2.0,
    ///     21.0 + 2.0, 22.0 + 2.0, 23.0 + 2.0, 24.0 + 2.0,
    ///     31.0 + 2.0, 32.0 + 2.0, 33.0 + 2.0, 34.0 + 2.0,
    ///     41.0 + 2.0, 42.0 + 2.0, 43.0 + 2.0, 44.0 + 2.0,
    /// );
    /// assert_eq!(a + b, c);
    /// # }
    /// ```
    fn add(self, rhs: f32) -> Mat4 {
        Mat4::new(self[0] + rhs, self[1] + rhs, self[2] + rhs, self[3] + rhs)
    }
}

impl Add<f32> for Mat4 {
    type Output = Mat4;
    
    /// Shorthand for `&lhs + rhs`.
    #[inline(always)] fn add(self, rhs: f32) -> Mat4 {
        &self + rhs
    }
}

impl<'a, 'b> Sub<&'b Mat4> for &'a Mat4 {
    type Output = Mat4;
    
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
    /// let a = mat4!(
    ///     11.0, 12.0, 13.0, 14.0,
    ///     21.0, 22.0, 23.0, 24.0,
    ///     31.0, 32.0, 33.0, 34.0,
    ///     41.0, 42.0, 43.0, 44.0,
    /// );
    /// let b = mat4!(
    ///     15.0, 16.0, 17.0, 18.0,
    ///     25.0, 26.0, 27.0, 28.0,
    ///     35.0, 36.0, 37.0, 38.0,
    ///     45.0, 46.0, 47.0, 48.0,
    /// );
    /// let c = mat4!(
    ///     11.0 - 15.0, 12.0 - 16.0, 13.0 - 17.0, 14.0 - 18.0,
    ///     21.0 - 25.0, 22.0 - 26.0, 23.0 - 27.0, 24.0 - 28.0,
    ///     31.0 - 35.0, 32.0 - 36.0, 33.0 - 37.0, 34.0 - 38.0,
    ///     41.0 - 45.0, 42.0 - 46.0, 43.0 - 47.0, 44.0 - 48.0,
    /// );
    /// assert_eq!(a - b, c);
    /// # }
    /// ```
    fn sub(self, rhs: &Mat4) -> Mat4 {
        Mat4::new(
            self[0] - rhs[0], self[1] - rhs[1], self[2] - rhs[2], self[3] - rhs[3]
        )
    }
}

impl<'a> Sub<Mat4> for &'a Mat4 {
    type Output = Mat4;
    
    /// Shorthand for `lhs - &rhs`.
    #[inline(always)] fn sub(self, rhs: Mat4) -> Mat4 {
        self - &rhs
    }
}

impl<'b> Sub<&'b Mat4> for Mat4 {
    type Output = Mat4;
    
    /// Shorthand for `&lhs - rhs`.
    #[inline(always)] fn sub(self, rhs: &Mat4) -> Mat4 {
        &self - rhs
    }
}

impl Sub<Mat4> for Mat4 {
    type Output = Mat4;
    
    /// Shorthand for `&lhs - &rhs`.
    #[inline(always)] fn sub(self, rhs: Mat4) -> Mat4 {
        &self - &rhs
    }
}

impl<'a> Sub<f32> for &'a Mat4 {
    type Output = Mat4;
    
    /// Subtracts a scalar from each component of a matrix producing a new matrix.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let a = mat4!(
    ///     11.0, 12.0, 13.0, 14.0,
    ///     21.0, 22.0, 23.0, 24.0,
    ///     31.0, 32.0, 33.0, 34.0,
    ///     41.0, 42.0, 43.0, 44.0,
    /// );
    /// let b = 2.0;
    /// let c = mat4!(
    ///     11.0 - 2.0, 12.0 - 2.0, 13.0 - 2.0, 14.0 - 2.0,
    ///     21.0 - 2.0, 22.0 - 2.0, 23.0 - 2.0, 24.0 - 2.0,
    ///     31.0 - 2.0, 32.0 - 2.0, 33.0 - 2.0, 34.0 - 2.0,
    ///     41.0 - 2.0, 42.0 - 2.0, 43.0 - 2.0, 44.0 - 2.0,
    /// );
    /// assert_eq!(a - b, c);
    /// # }
    /// ```
    fn sub(self, rhs: f32) -> Mat4 {
        Mat4::new(self[0] - rhs, self[1] - rhs, self[2] - rhs, self[3] - rhs)
    }
}

impl Sub<f32> for Mat4 {
    type Output = Mat4;
    
    /// Shorthand for `&lhs - rhs`.
    #[inline(always)] fn sub(self, rhs: f32) -> Mat4 {
        &self - rhs
    }
}

impl<'a, 'b> Mul<&'b Mat4> for &'a Mat4 {
    type Output = Mat4;
    
    /// Performs algebraic multiplication of two matrices producing a new matrix.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let a = mat4!(
    ///     11.0, 12.0, 13.0, 14.0,
    ///     21.0, 22.0, 23.0, 24.0,
    ///     31.0, 32.0, 33.0, 34.0,
    ///     41.0, 42.0, 43.0, 44.0,
    /// );
    /// let b = mat4!(
    ///     15.0, 16.0, 17.0, 18.0,
    ///     25.0, 26.0, 27.0, 28.0,
    ///     35.0, 36.0, 37.0, 38.0,
    ///     45.0, 46.0, 47.0, 48.0,
    /// );
    /// let c = mat4!(
    ///     11.0*15.0 + 21.0*16.0 + 31.0*17.0 + 41.0*18.0,
    ///     12.0*15.0 + 22.0*16.0 + 32.0*17.0 + 42.0*18.0,
    ///     13.0*15.0 + 23.0*16.0 + 33.0*17.0 + 43.0*18.0,
    ///     14.0*15.0 + 24.0*16.0 + 34.0*17.0 + 44.0*18.0,
    ///
    ///     11.0*25.0 + 21.0*26.0 + 31.0*27.0 + 41.0*28.0,
    ///     12.0*25.0 + 22.0*26.0 + 32.0*27.0 + 42.0*28.0,
    ///     13.0*25.0 + 23.0*26.0 + 33.0*27.0 + 43.0*28.0,
    ///     14.0*25.0 + 24.0*26.0 + 34.0*27.0 + 44.0*28.0,
    ///
    ///     11.0*35.0 + 21.0*36.0 + 31.0*37.0 + 41.0*38.0,
    ///     12.0*35.0 + 22.0*36.0 + 32.0*37.0 + 42.0*38.0,
    ///     13.0*35.0 + 23.0*36.0 + 33.0*37.0 + 43.0*38.0,
    ///     14.0*35.0 + 24.0*36.0 + 34.0*37.0 + 44.0*38.0,
    ///
    ///     11.0*45.0 + 21.0*46.0 + 31.0*47.0 + 41.0*48.0,
    ///     12.0*45.0 + 22.0*46.0 + 32.0*47.0 + 42.0*48.0,
    ///     13.0*45.0 + 23.0*46.0 + 33.0*47.0 + 43.0*48.0,
    ///     14.0*45.0 + 24.0*46.0 + 34.0*47.0 + 44.0*48.0,
    /// );
    /// assert_eq!(a * b, c);
    /// # }
    /// ```
    fn mul(self, rhs: &Mat4) -> Mat4 {
        let t = self.transpose();
        Mat4::new(
            Vec4::new(t[0].dot(rhs[0]), t[1].dot(rhs[0]), t[2].dot(rhs[0]), t[3].dot(rhs[0])),
            Vec4::new(t[0].dot(rhs[1]), t[1].dot(rhs[1]), t[2].dot(rhs[1]), t[3].dot(rhs[1])),
            Vec4::new(t[0].dot(rhs[2]), t[1].dot(rhs[2]), t[2].dot(rhs[2]), t[3].dot(rhs[2])),
            Vec4::new(t[0].dot(rhs[3]), t[1].dot(rhs[3]), t[2].dot(rhs[3]), t[3].dot(rhs[3])),
        )
    }
}

impl<'a> Mul<Mat4> for &'a Mat4 {
    type Output = Mat4;
    
    /// Shorthand for `lhs * &rhs`.
    #[inline(always)] fn mul(self, rhs: Mat4) -> Mat4 {
        self * &rhs
    }
}

impl<'b> Mul<&'b Mat4> for Mat4 {
    type Output = Mat4;
    
    /// Shorthand for `&lhs * rhs`.
    #[inline(always)] fn mul(self, rhs: &Mat4) -> Mat4 {
        &self * rhs
    }
}

impl Mul<Mat4> for Mat4 {
    type Output = Mat4;
    
    /// Shorthand for `&lhs * &rhs`.
    #[inline(always)] fn mul(self, rhs: Mat4) -> Mat4 {
        &self * &rhs
    }
}

impl<'a, 'b> Mul<&'b Vec4> for &'a Mat4 {
    type Output = Vec4;
    
    /// Performs algebraic multiplication of a matrix by a vector, producing a new vector.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let a = mat4!(
    ///     11.0, 12.0, 13.0, 14.0,
    ///     21.0, 22.0, 23.0, 24.0,
    ///     31.0, 32.0, 33.0, 34.0,
    ///     41.0, 42.0, 43.0, 44.0,
    /// );
    /// let u = vec4!(15.0, 16.0, 17.0, 18.0);
    /// let v = vec4!(
    ///     11.0*15.0 + 21.0*16.0 + 31.0*17.0 + 41.0*18.0,
    ///     12.0*15.0 + 22.0*16.0 + 32.0*17.0 + 42.0*18.0,
    ///     13.0*15.0 + 23.0*16.0 + 33.0*17.0 + 43.0*18.0,
    ///     14.0*15.0 + 24.0*16.0 + 34.0*17.0 + 44.0*18.0,
    /// );
    /// assert_eq!(a * u, v);
    /// # }
    /// ```
    fn mul(self, rhs: &Vec4) -> Vec4 {
        let t = self.transpose();
        Vec4::new(
            t[0].dot(rhs), t[1].dot(rhs), t[2].dot(rhs), t[3].dot(rhs),
        )
    }
}

impl<'a> Mul<Vec4> for &'a Mat4 {
    type Output = Vec4;
    
    /// Shorthand for `lhs * &rhs`.
    #[inline(always)] fn mul(self, rhs: Vec4) -> Vec4 {
        self * &rhs
    }
}

impl<'b> Mul<&'b Vec4> for Mat4 {
    type Output = Vec4;
    
    /// Shorthand for `&lhs * rhs`.
    #[inline(always)] fn mul(self, rhs: &Vec4) -> Vec4 {
        &self * rhs
    }
}

impl Mul<Vec4> for Mat4 {
    type Output = Vec4;
    
    /// Shorthand for `&lhs * &rhs`.
    #[inline(always)] fn mul(self, rhs: Vec4) -> Vec4 {
        &self * &rhs
    }
}

impl<'a> Mul<f32> for &'a Mat4 {
    type Output = Mat4;
    
    /// Multiplies each component of a matrix by a scalar producing a new matrix.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let a = mat4!(
    ///     11.0, 12.0, 13.0, 14.0,
    ///     21.0, 22.0, 23.0, 24.0,
    ///     31.0, 32.0, 33.0, 34.0,
    ///     41.0, 42.0, 43.0, 44.0,
    /// );
    /// let b = 2.0;
    /// let c = mat4!(
    ///     11.0 * 2.0, 12.0 * 2.0, 13.0 * 2.0, 14.0 * 2.0,
    ///     21.0 * 2.0, 22.0 * 2.0, 23.0 * 2.0, 24.0 * 2.0,
    ///     31.0 * 2.0, 32.0 * 2.0, 33.0 * 2.0, 34.0 * 2.0,
    ///     41.0 * 2.0, 42.0 * 2.0, 43.0 * 2.0, 44.0 * 2.0,
    /// );
    /// assert_eq!(a * b, c);
    /// # }
    /// ```
    fn mul(self, rhs: f32) -> Mat4 {
        Mat4::new(self[0] * rhs, self[1] * rhs, self[2] * rhs, self[3] * rhs)
    }
}

impl Mul<f32> for Mat4 {
    type Output = Mat4;
    
    /// Shorthand for `&lhs * rhs`.
    #[inline(always)] fn mul(self, rhs: f32) -> Mat4 {
        &self * rhs
    }
}

impl<'a, 'b> Div<&'b Mat4> for &'a Mat4 {
    type Output = Mat4;
    
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
    /// let a = mat4!(
    ///     11.0, 12.0, 13.0, 14.0,
    ///     21.0, 22.0, 23.0, 24.0,
    ///     31.0, 32.0, 33.0, 34.0,
    ///     41.0, 42.0, 43.0, 44.0,
    /// );
    /// let b = mat4!(
    ///     15.0, 16.0, 17.0, 18.0,
    ///     25.0, 26.0, 27.0, 28.0,
    ///     35.0, 36.0, 37.0, 38.0,
    ///     45.0, 46.0, 47.0, 48.0,
    /// );
    /// let c = mat4!(
    ///     11.0 / 15.0, 12.0 / 16.0, 13.0 / 17.0, 14.0 / 18.0,
    ///     21.0 / 25.0, 22.0 / 26.0, 23.0 / 27.0, 24.0 / 28.0,
    ///     31.0 / 35.0, 32.0 / 36.0, 33.0 / 37.0, 34.0 / 38.0,
    ///     41.0 / 45.0, 42.0 / 46.0, 43.0 / 47.0, 44.0 / 48.0,
    /// );
    /// assert_eq!(a / b, c);
    /// # }
    /// ```
    fn div(self, rhs: &Mat4) -> Mat4 {
        Mat4::new(
            self[0] / rhs[0], self[1] / rhs[1], self[2] / rhs[2], self[3] / rhs[3]
        )
    }
}

impl<'a> Div<Mat4> for &'a Mat4 {
    type Output = Mat4;
    
    /// Shorthand for `lhs / &rhs`.
    #[inline(always)] fn div(self, rhs: Mat4) -> Mat4 {
        self / &rhs
    }
}

impl<'b> Div<&'b Mat4> for Mat4 {
    type Output = Mat4;
    
    /// Shorthand for `&lhs / rhs`.
    #[inline(always)] fn div(self, rhs: &Mat4) -> Mat4 {
        &self / rhs
    }
}

impl Div<Mat4> for Mat4 {
    type Output = Mat4;
    
    /// Shorthand for `&lhs / &rhs`.
    #[inline(always)] fn div(self, rhs: Mat4) -> Mat4 {
        &self / &rhs
    }
}

impl<'a> Div<f32> for &'a Mat4 {
    type Output = Mat4;
    
    /// Divides each component of a {doc_name} by a scalar producing a new matrix.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let a = mat4!(
    ///     11.0, 12.0, 13.0, 14.0,
    ///     21.0, 22.0, 23.0, 24.0,
    ///     31.0, 32.0, 33.0, 34.0,
    ///     41.0, 42.0, 43.0, 44.0,
    /// );
    /// let b = 2.0;
    /// let c = mat4!(
    ///     11.0 / 2.0, 12.0 / 2.0, 13.0 / 2.0, 14.0 / 2.0,
    ///     21.0 / 2.0, 22.0 / 2.0, 23.0 / 2.0, 24.0 / 2.0,
    ///     31.0 / 2.0, 32.0 / 2.0, 33.0 / 2.0, 34.0 / 2.0,
    ///     41.0 / 2.0, 42.0 / 2.0, 43.0 / 2.0, 44.0 / 2.0,
    /// );
    /// assert_eq!(a / b, c);
    /// # }
    /// ```
    fn div(self, rhs: f32) -> Mat4 {
        Mat4::new(self[0] / rhs, self[1] / rhs, self[2] / rhs, self[3] / rhs)
    }
}

impl Div<f32> for Mat4 {
    type Output = Mat4;
    
    /// Shorthand for `&lhs / rhs`.
    #[inline(always)] fn div(self, rhs: f32) -> Mat4 {
        &self / rhs
    }
}

impl<'a> Neg for &'a Mat4 {
    type Output = Mat4;
    
    /// Applies negation to each component of a matrix producing a new matrix.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let a = -mat4!(
    ///     11.0, 12.0, 13.0, 14.0,
    ///     21.0, 22.0, 23.0, 24.0,
    ///     31.0, 32.0, 33.0, 34.0,
    ///     41.0, 42.0, 43.0, 44.0,
    /// );
    /// let b = mat4!(
    ///     -11.0, -12.0, -13.0, -14.0,
    ///     -21.0, -22.0, -23.0, -24.0,
    ///     -31.0, -32.0, -33.0, -34.0,
    ///     -41.0, -42.0, -43.0, -44.0,
    /// );
    /// assert_eq!(a, b);
    /// # }
    /// ```
    fn neg(self) -> Mat4 {
        Mat4::new(-self[0], -self[1], -self[2], -self[3])
    }
}

impl Neg for Mat4 {
    type Output = Mat4;
    
    /// Shorthand for `-&arg`.
    #[inline(always)] fn neg(self) -> Mat4 {
        -&self
    }
}

/// Builder macro for creating new 4x4 column major matrices.
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
/// let a = mat4!(15.0);
/// let b = Mat4::new(
///     vec4!(15.0, 0.0, 0.0, 0.0),
///     vec4!(0.0, 15.0, 0.0, 0.0),
///     vec4!(0.0, 0.0, 15.0, 0.0),
///     vec4!(0.0, 0.0, 0.0, 15.0),
/// );
/// assert_eq!(a, b);
/// # }
/// ```
///
/// Create a new column major matrix with
///     `column0 = vec4(15.0, 16.0, 17.0, 18.0)`,
///     `column1 = vec4(25.0, 26.0, 27.0, 28.0)`,
///     `column2 = vec4(35.0, 36.0, 37.0, 38.0)`,
///     `column3 = vec4(45.0, 46.0, 47.0, 48.0)`:
///
/// ```
/// #[macro_use] extern crate vexyz_math;
/// use vexyz_math::*;
///
/// # fn main() {
/// let a = mat4!(
///     vec4!(15.0, 16.0, 17.0, 18.0),
///     vec4!(25.0, 26.0, 27.0, 28.0),
///     vec4!(35.0, 36.0, 37.0, 38.0),
///     vec4!(45.0, 46.0, 47.0, 48.0),
/// );
/// let b = Mat4::new(
///     vec4!(15.0, 16.0, 17.0, 18.0),
///     vec4!(25.0, 26.0, 27.0, 28.0),
///     vec4!(35.0, 36.0, 37.0, 38.0),
///     vec4!(45.0, 46.0, 47.0, 48.0),
/// );
/// assert_eq!(a, b);
/// # }
/// ```
///
/// Create a new column major matrix with
///     `column0 = vec4(15.0, 16.0, 17.0, 18.0)`,
///     `column1 = vec4(25.0, 26.0, 27.0, 28.0)`,
///     `column2 = vec4(35.0, 36.0, 37.0, 38.0)`,
///     `column3 = vec4(45.0, 46.0, 47.0, 48.0)`:
///
/// ```
/// #[macro_use] extern crate vexyz_math;
/// use vexyz_math::*;
///
/// # fn main() {
/// let a = mat4!(
///     15.0, 16.0, 17.0, 18.0,
///     25.0, 26.0, 27.0, 28.0,
///     35.0, 36.0, 37.0, 38.0,
///     45.0, 46.0, 47.0, 48.0,
/// );
/// let b = Mat4::new(
///     vec4!(15.0, 16.0, 17.0, 18.0),
///     vec4!(25.0, 26.0, 27.0, 28.0),
///     vec4!(35.0, 36.0, 37.0, 38.0),
///     vec4!(45.0, 46.0, 47.0, 48.0),
/// );
/// assert_eq!(a, b);
/// # }
/// ```
#[macro_export]
macro_rules! mat4 {
    ($s:expr) => {{
        let s = $s as f32;
        Mat4::new(
            vec4!(s, 0, 0, 0),
            vec4!(0, s, 0, 0),
            vec4!(0, 0, s, 0),
            vec4!(0, 0, 0, s),
        )
    }};
    ($col0:expr, $col1:expr, $col2:expr, $col3:expr) => {{
        Mat4::new($col0, $col1, $col2, $col3)
    }};
    ($col0:expr, $col1:expr, $col2:expr, $col3:expr,) => {{
        Mat4::new($col0, $col1, $col2, $col3)
    }};
    ($m00:expr, $m01:expr, $m02:expr, $m03:expr,
     $m10:expr, $m11:expr, $m12:expr, $m13:expr,
     $m20:expr, $m21:expr, $m22:expr, $m23:expr,
     $m30:expr, $m31:expr, $m32:expr, $m33:expr) => {{
        Mat4::new(
            vec4!($m00 as f32, $m01 as f32, $m02 as f32, $m03 as f32),
            vec4!($m10 as f32, $m11 as f32, $m12 as f32, $m13 as f32),
            vec4!($m20 as f32, $m21 as f32, $m22 as f32, $m23 as f32),
            vec4!($m30 as f32, $m31 as f32, $m32 as f32, $m33 as f32),
        )
    }};
    ($m00:expr, $m01:expr, $m02:expr, $m03:expr,
     $m10:expr, $m11:expr, $m12:expr, $m13:expr,
     $m20:expr, $m21:expr, $m22:expr, $m23:expr,
     $m30:expr, $m31:expr, $m32:expr, $m33:expr,) => {{
        Mat4::new(
            vec4!($m00 as f32, $m01 as f32, $m02 as f32, $m03 as f32),
            vec4!($m10 as f32, $m11 as f32, $m12 as f32, $m13 as f32),
            vec4!($m20 as f32, $m21 as f32, $m22 as f32, $m23 as f32),
            vec4!($m30 as f32, $m31 as f32, $m32 as f32, $m33 as f32),
        )
    }};
}
