
pub trait FloatOps {
    fn approx_equal(&self, rhs: f64, eps: f64) -> bool;
}

impl FloatOps for f64 {
    /// Tests for approximate equality within given absolute error.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {{
    /// let s = 1.0;
    /// assert!(s.approx_equal(s + 1e-9, 1e-8));
    /// assert!(!s.approx_equal(s + 1e-8, 1e-9));
    /// # }}
    /// ```
    fn approx_equal(&self, rhs: f64, eps: f64) -> bool {
        (self - rhs).abs() < eps
    }
}
