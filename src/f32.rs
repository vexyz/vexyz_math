use std;

static DEG_TO_RAD_FACTOR: f32 = (std::f64::consts::PI / 180.0) as f32;
static RAD_TO_DEG_FACTOR: f32 = (180.0 / std::f64::consts::PI) as f32;

pub trait FloatOps {
    fn to_rad(self) -> f32;
    fn to_deg(self) -> f32;
    fn approx_equal(&self, rhs: f32, eps: f32) -> bool;
}

impl FloatOps for f32 {
    
    /// Converts degrees to radians.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use std::f32::consts::*;
    /// use vexyz_math::*;
    ///
    /// # fn main() {{
    /// let rad = 180_f32.to_rad();
    /// assert!(rad.approx_equal(PI, 1e-7));
    /// # }}
    /// ```
    fn to_rad(self) -> f32 {
        self * DEG_TO_RAD_FACTOR
    }
    
    /// Converts radians to degrees.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use std::f32::consts::*;
    /// use vexyz_math::*;
    ///
    /// # fn main() {{
    /// let deg = PI.to_deg();
    /// assert!(deg.approx_equal(180.0, 1e-7));
    /// # }}
    /// ```
    fn to_deg(self) -> f32 {
        self * RAD_TO_DEG_FACTOR
    }
    
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
    /// assert!(s.approx_equal(s + 1e-7, 1e-6));
    /// assert!(!s.approx_equal(s + 1e-6, 1e-7));
    /// # }}
    /// ```
    fn approx_equal(&self, rhs: f32, eps: f32) -> bool {
        (self - rhs).abs() < eps
    }
}
