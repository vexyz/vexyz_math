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
    ///     0.1, 0.2, 0.3, 0.4,
    ///     1.1, 1.2, 1.3, 1.4,
    ///     2.1, 2.2, 2.3, 2.4,
    ///     3.1, 3.2, 3.3, 3.4,
    /// );
    /// let b = mat4!(
    ///     0.1, 1.1, 2.1, 3.1,
    ///     0.2, 1.2, 2.2, 3.2,
    ///     0.3, 1.3, 2.3, 3.3,
    ///     0.4, 1.4, 2.4, 3.4,
    /// );
    /// assert_eq!(a.transpose(), b);
    /// # }
    /// ```
    pub fn transpose(&self) -> Mat4 {
        Mat4::new(
            Vec4::new(self[0][0], self[1][0], self[2][0], self[3][0]),
            Vec4::new(self[0][1], self[1][1], self[2][1], self[3][1]),
            Vec4::new(self[0][2], self[1][2], self[2][2], self[3][2]),
            Vec4::new(self[0][3], self[1][3], self[2][3], self[3][3]),
        )
    }
}

pub trait Mat4Mat4Ops<Rhs> {
    fn lerp(&self, rhs: Rhs, a: f64) -> Mat4;
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
    ///     0.1, 0.2, 0.3, 0.4,
    ///     1.1, 1.2, 1.3, 1.4,
    ///     2.1, 2.2, 2.3, 2.4,
    ///     3.1, 3.2, 3.3, 3.4,
    /// );
    /// let b = mat4!(
    ///     0.5, 0.6, 0.7, 0.8,
    ///     1.5, 1.6, 1.7, 1.8,
    ///     2.5, 2.6, 2.7, 2.8,
    ///     3.5, 3.6, 3.7, 3.8,
    /// );
    /// let c = a*(1.0 - 0.25) + b*0.25;
    /// assert_eq!(a.lerp(b, 0.25), c);
    /// # }
    /// ```
    fn lerp(&self, rhs: &Mat4, a: f64) -> Mat4 {
    	let b = 1.0 - a;
        Mat4::new(
            self[0]*b + rhs[0]*a, self[1]*b + rhs[1]*a, self[2]*b + rhs[2]*a, self[3]*b + rhs[3]*a
        )
    }
}

impl Mat4Mat4Ops<Mat4> for Mat4 {
	/// Shorthand for `lhs.determinant(&rhs)`.
    #[inline(always)] fn lerp(&self, rhs: Mat4, a: f64) -> Mat4 {
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
    ///     vec4!(0.1, 0.2, 0.3, 0.4),
    ///     vec4!(1.1, 1.2, 1.3, 1.4),
    ///     vec4!(2.1, 2.2, 2.3, 2.4),
    ///     vec4!(3.1, 3.2, 3.3, 3.4),
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
    ///     0.1, 0.2, 0.3, 0.4,
    ///     1.1, 1.2, 1.3, 1.4,
    ///     2.1, 2.2, 2.3, 2.4,
    ///     3.1, 3.2, 3.3, 3.4,
    /// );
    /// let b = mat4!(
    ///     0.5, 0.6, 0.7, 0.8,
    ///     1.5, 1.6, 1.7, 1.8,
    ///     2.5, 2.6, 2.7, 2.8,
    ///     3.5, 3.6, 3.7, 3.8,
    /// );
    /// let c = mat4!(
    ///     0.1 + 0.5, 0.2 + 0.6, 0.3 + 0.7, 0.4 + 0.8,
    ///     1.1 + 1.5, 1.2 + 1.6, 1.3 + 1.7, 1.4 + 1.8,
    ///     2.1 + 2.5, 2.2 + 2.6, 2.3 + 2.7, 2.4 + 2.8,
    ///     3.1 + 3.5, 3.2 + 3.6, 3.3 + 3.7, 3.4 + 3.8,
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

impl<'a> Add<f64> for &'a Mat4 {
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
    ///     0.1, 0.2, 0.3, 0.4,
    ///     1.1, 1.2, 1.3, 1.4,
    ///     2.1, 2.2, 2.3, 2.4,
    ///     3.1, 3.2, 3.3, 3.4,
    /// );
    /// let b = 2.0;
    /// let c = mat4!(
    ///     0.1 + 2.0, 0.2 + 2.0, 0.3 + 2.0, 0.4 + 2.0,
    ///     1.1 + 2.0, 1.2 + 2.0, 1.3 + 2.0, 1.4 + 2.0,
    ///     2.1 + 2.0, 2.2 + 2.0, 2.3 + 2.0, 2.4 + 2.0,
    ///     3.1 + 2.0, 3.2 + 2.0, 3.3 + 2.0, 3.4 + 2.0,
    /// );
    /// assert_eq!(a + b, c);
    /// # }
    /// ```
    fn add(self, rhs: f64) -> Mat4 {
        Mat4::new(self[0] + rhs, self[1] + rhs, self[2] + rhs, self[3] + rhs)
    }
}

impl Add<f64> for Mat4 {
    type Output = Mat4;
    
    /// Shorthand for `&lhs + rhs`.
    #[inline(always)] fn add(self, rhs: f64) -> Mat4 {
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
    ///     0.1, 0.2, 0.3, 0.4,
    ///     1.1, 1.2, 1.3, 1.4,
    ///     2.1, 2.2, 2.3, 2.4,
    ///     3.1, 3.2, 3.3, 3.4,
    /// );
    /// let b = mat4!(
    ///     0.5, 0.6, 0.7, 0.8,
    ///     1.5, 1.6, 1.7, 1.8,
    ///     2.5, 2.6, 2.7, 2.8,
    ///     3.5, 3.6, 3.7, 3.8,
    /// );
    /// let c = mat4!(
    ///     0.1 - 0.5, 0.2 - 0.6, 0.3 - 0.7, 0.4 - 0.8,
    ///     1.1 - 1.5, 1.2 - 1.6, 1.3 - 1.7, 1.4 - 1.8,
    ///     2.1 - 2.5, 2.2 - 2.6, 2.3 - 2.7, 2.4 - 2.8,
    ///     3.1 - 3.5, 3.2 - 3.6, 3.3 - 3.7, 3.4 - 3.8,
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

impl<'a> Sub<f64> for &'a Mat4 {
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
    ///     0.1, 0.2, 0.3, 0.4,
    ///     1.1, 1.2, 1.3, 1.4,
    ///     2.1, 2.2, 2.3, 2.4,
    ///     3.1, 3.2, 3.3, 3.4,
    /// );
    /// let b = 2.0;
    /// let c = mat4!(
    ///     0.1 - 2.0, 0.2 - 2.0, 0.3 - 2.0, 0.4 - 2.0,
    ///     1.1 - 2.0, 1.2 - 2.0, 1.3 - 2.0, 1.4 - 2.0,
    ///     2.1 - 2.0, 2.2 - 2.0, 2.3 - 2.0, 2.4 - 2.0,
    ///     3.1 - 2.0, 3.2 - 2.0, 3.3 - 2.0, 3.4 - 2.0,
    /// );
    /// assert_eq!(a - b, c);
    /// # }
    /// ```
    fn sub(self, rhs: f64) -> Mat4 {
        Mat4::new(self[0] - rhs, self[1] - rhs, self[2] - rhs, self[3] - rhs)
    }
}

impl Sub<f64> for Mat4 {
    type Output = Mat4;
    
    /// Shorthand for `&lhs - rhs`.
    #[inline(always)] fn sub(self, rhs: f64) -> Mat4 {
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
    ///     0.1, 0.2, 0.3, 0.4,
    ///     1.1, 1.2, 1.3, 1.4,
    ///     2.1, 2.2, 2.3, 2.4,
    ///     3.1, 3.2, 3.3, 3.4,
    /// );
    /// let b = mat4!(
    ///     0.5, 0.6, 0.7, 0.8,
    ///     1.5, 1.6, 1.7, 1.8,
    ///     2.5, 2.6, 2.7, 2.8,
    ///     3.5, 3.6, 3.7, 3.8,
    /// );
    /// let c = mat4!(
    ///     0.1*0.5 + 1.1*0.6 + 2.1*0.7 + 3.1*0.8,
    ///     0.2*0.5 + 1.2*0.6 + 2.2*0.7 + 3.2*0.8,
    ///     0.3*0.5 + 1.3*0.6 + 2.3*0.7 + 3.3*0.8,
    ///     0.4*0.5 + 1.4*0.6 + 2.4*0.7 + 3.4*0.8,
    ///
    ///     0.1*1.5 + 1.1*1.6 + 2.1*1.7 + 3.1*1.8,
    ///     0.2*1.5 + 1.2*1.6 + 2.2*1.7 + 3.2*1.8,
    ///     0.3*1.5 + 1.3*1.6 + 2.3*1.7 + 3.3*1.8,
    ///     0.4*1.5 + 1.4*1.6 + 2.4*1.7 + 3.4*1.8,
    ///
    ///     0.1*2.5 + 1.1*2.6 + 2.1*2.7 + 3.1*2.8,
    ///     0.2*2.5 + 1.2*2.6 + 2.2*2.7 + 3.2*2.8,
    ///     0.3*2.5 + 1.3*2.6 + 2.3*2.7 + 3.3*2.8,
    ///     0.4*2.5 + 1.4*2.6 + 2.4*2.7 + 3.4*2.8,
    ///
    ///     0.1*3.5 + 1.1*3.6 + 2.1*3.7 + 3.1*3.8,
    ///     0.2*3.5 + 1.2*3.6 + 2.2*3.7 + 3.2*3.8,
    ///     0.3*3.5 + 1.3*3.6 + 2.3*3.7 + 3.3*3.8,
    ///     0.4*3.5 + 1.4*3.6 + 2.4*3.7 + 3.4*3.8,
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
    ///     0.1, 0.2, 0.3, 0.4,
    ///     1.1, 1.2, 1.3, 1.4,
    ///     2.1, 2.2, 2.3, 2.4,
    ///     3.1, 3.2, 3.3, 3.4,
    /// );
    /// let u = vec4!(0.5, 0.6, 0.7, 0.8);
    /// let v = vec4!(
    ///     0.1*0.5 + 1.1*0.6 + 2.1*0.7 + 3.1*0.8,
    ///     0.2*0.5 + 1.2*0.6 + 2.2*0.7 + 3.2*0.8,
    ///     0.3*0.5 + 1.3*0.6 + 2.3*0.7 + 3.3*0.8,
    ///     0.4*0.5 + 1.4*0.6 + 2.4*0.7 + 3.4*0.8,
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

impl<'a> Mul<f64> for &'a Mat4 {
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
    ///     0.1, 0.2, 0.3, 0.4,
    ///     1.1, 1.2, 1.3, 1.4,
    ///     2.1, 2.2, 2.3, 2.4,
    ///     3.1, 3.2, 3.3, 3.4,
    /// );
    /// let b = 2.0;
    /// let c = mat4!(
    ///     0.1 * 2.0, 0.2 * 2.0, 0.3 * 2.0, 0.4 * 2.0,
    ///     1.1 * 2.0, 1.2 * 2.0, 1.3 * 2.0, 1.4 * 2.0,
    ///     2.1 * 2.0, 2.2 * 2.0, 2.3 * 2.0, 2.4 * 2.0,
    ///     3.1 * 2.0, 3.2 * 2.0, 3.3 * 2.0, 3.4 * 2.0,
    /// );
    /// assert_eq!(a * b, c);
    /// # }
    /// ```
    fn mul(self, rhs: f64) -> Mat4 {
        Mat4::new(self[0] * rhs, self[1] * rhs, self[2] * rhs, self[3] * rhs)
    }
}

impl Mul<f64> for Mat4 {
    type Output = Mat4;
    
    /// Shorthand for `&lhs * rhs`.
    #[inline(always)] fn mul(self, rhs: f64) -> Mat4 {
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
    ///     0.1, 0.2, 0.3, 0.4,
    ///     1.1, 1.2, 1.3, 1.4,
    ///     2.1, 2.2, 2.3, 2.4,
    ///     3.1, 3.2, 3.3, 3.4,
    /// );
    /// let b = mat4!(
    ///     0.5, 0.6, 0.7, 0.8,
    ///     1.5, 1.6, 1.7, 1.8,
    ///     2.5, 2.6, 2.7, 2.8,
    ///     3.5, 3.6, 3.7, 3.8,
    /// );
    /// let c = mat4!(
    ///     0.1 / 0.5, 0.2 / 0.6, 0.3 / 0.7, 0.4 / 0.8,
    ///     1.1 / 1.5, 1.2 / 1.6, 1.3 / 1.7, 1.4 / 1.8,
    ///     2.1 / 2.5, 2.2 / 2.6, 2.3 / 2.7, 2.4 / 2.8,
    ///     3.1 / 3.5, 3.2 / 3.6, 3.3 / 3.7, 3.4 / 3.8,
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

impl<'a> Div<f64> for &'a Mat4 {
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
    ///     0.1, 0.2, 0.3, 0.4,
    ///     1.1, 1.2, 1.3, 1.4,
    ///     2.1, 2.2, 2.3, 2.4,
    ///     3.1, 3.2, 3.3, 3.4,
    /// );
    /// let b = 2.0;
    /// let c = mat4!(
    ///     0.1 / 2.0, 0.2 / 2.0, 0.3 / 2.0, 0.4 / 2.0,
    ///     1.1 / 2.0, 1.2 / 2.0, 1.3 / 2.0, 1.4 / 2.0,
    ///     2.1 / 2.0, 2.2 / 2.0, 2.3 / 2.0, 2.4 / 2.0,
    ///     3.1 / 2.0, 3.2 / 2.0, 3.3 / 2.0, 3.4 / 2.0,
    /// );
    /// assert_eq!(a / b, c);
    /// # }
    /// ```
    fn div(self, rhs: f64) -> Mat4 {
        Mat4::new(self[0] / rhs, self[1] / rhs, self[2] / rhs, self[3] / rhs)
    }
}

impl Div<f64> for Mat4 {
    type Output = Mat4;
    
    /// Shorthand for `&lhs / rhs`.
    #[inline(always)] fn div(self, rhs: f64) -> Mat4 {
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
    ///     0.1, 0.2, 0.3, 0.4,
    ///     1.1, 1.2, 1.3, 1.4,
    ///     2.1, 2.2, 2.3, 2.4,
    ///     3.1, 3.2, 3.3, 3.4,
    /// );
    /// let b = mat4!(
    ///     -0.1, -0.2, -0.3, -0.4,
    ///     -1.1, -1.2, -1.3, -1.4,
    ///     -2.1, -2.2, -2.3, -2.4,
    ///     -3.1, -3.2, -3.3, -3.4,
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
/// Create a new column major matrix with `0.5` on diagonal:
///
/// ```
/// #[macro_use] extern crate vexyz_math;
/// use vexyz_math::*;
///
/// # fn main() {
/// let a = mat4!(0.5);
/// let b = Mat4::new(
///     vec4!(0.5, 0.0, 0.0, 0.0),
///     vec4!(0.0, 0.5, 0.0, 0.0),
///     vec4!(0.0, 0.0, 0.5, 0.0),
///     vec4!(0.0, 0.0, 0.0, 0.5),
/// );
/// assert_eq!(a, b);
/// # }
/// ```
///
/// Create a new column major matrix with
///     `column0 = vec4(0.5, 0.6, 0.7, 0.8)`,
///     `column1 = vec4(1.5, 1.6, 1.7, 1.8)`,
///     `column2 = vec4(2.5, 2.6, 2.7, 2.8)`,
///     `column3 = vec4(3.5, 3.6, 3.7, 3.8)`:
///
/// ```
/// #[macro_use] extern crate vexyz_math;
/// use vexyz_math::*;
///
/// # fn main() {
/// let a = mat4!(
///     vec4!(0.5, 0.6, 0.7, 0.8),
///     vec4!(1.5, 1.6, 1.7, 1.8),
///     vec4!(2.5, 2.6, 2.7, 2.8),
///     vec4!(3.5, 3.6, 3.7, 3.8),
/// );
/// let b = Mat4::new(
///     vec4!(0.5, 0.6, 0.7, 0.8),
///     vec4!(1.5, 1.6, 1.7, 1.8),
///     vec4!(2.5, 2.6, 2.7, 2.8),
///     vec4!(3.5, 3.6, 3.7, 3.8),
/// );
/// assert_eq!(a, b);
/// # }
/// ```
///
/// Create a new column major matrix with
///     `column0 = vec4(0.5, 0.6, 0.7, 0.8)`,
///     `column1 = vec4(1.5, 1.6, 1.7, 1.8)`,
///     `column2 = vec4(2.5, 2.6, 2.7, 2.8)`,
///     `column3 = vec4(3.5, 3.6, 3.7, 3.8)`:
///
/// ```
/// #[macro_use] extern crate vexyz_math;
/// use vexyz_math::*;
///
/// # fn main() {
/// let a = mat4!(
///     0.5, 0.6, 0.7, 0.8,
///     1.5, 1.6, 1.7, 1.8,
///     2.5, 2.6, 2.7, 2.8,
///     3.5, 3.6, 3.7, 3.8,
/// );
/// let b = Mat4::new(
///     vec4!(0.5, 0.6, 0.7, 0.8),
///     vec4!(1.5, 1.6, 1.7, 1.8),
///     vec4!(2.5, 2.6, 2.7, 2.8),
///     vec4!(3.5, 3.6, 3.7, 3.8),
/// );
/// assert_eq!(a, b);
/// # }
/// ```
#[macro_export]
macro_rules! mat4 {
    ($s:expr) => {{
        let s = $s as f64;
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
            vec4!($m00 as f64, $m01 as f64, $m02 as f64, $m03 as f64),
            vec4!($m10 as f64, $m11 as f64, $m12 as f64, $m13 as f64),
            vec4!($m20 as f64, $m21 as f64, $m22 as f64, $m23 as f64),
            vec4!($m30 as f64, $m31 as f64, $m32 as f64, $m33 as f64),
        )
    }};
    ($m00:expr, $m01:expr, $m02:expr, $m03:expr,
     $m10:expr, $m11:expr, $m12:expr, $m13:expr,
     $m20:expr, $m21:expr, $m22:expr, $m23:expr,
     $m30:expr, $m31:expr, $m32:expr, $m33:expr,) => {{
        Mat4::new(
            vec4!($m00 as f64, $m01 as f64, $m02 as f64, $m03 as f64),
            vec4!($m10 as f64, $m11 as f64, $m12 as f64, $m13 as f64),
            vec4!($m20 as f64, $m21 as f64, $m22 as f64, $m23 as f64),
            vec4!($m30 as f64, $m31 as f64, $m32 as f64, $m33 as f64),
        )
    }};
}
