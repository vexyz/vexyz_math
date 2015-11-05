// Generated code.
use std::fmt::{Display, Formatter, Result};
use std::ops::*;

/// 4-dimensional vector with boolean `x`, `y`, `z`, and `w` components.
///
/// Vectors can also represent colors with the help of
/// `r()`, `g()`, `b()`, and `a()` accessors.
///
/// Most operators and methods on vectors are performed in component-wise fashion. The notable
/// exception is vector-matrix multiplication.
#[derive(Debug, PartialEq, Copy, Clone)]
pub struct Vec4b { data: [bool; 4] }

impl Vec4b {
    /// Constructs a new `Vec4b`.
    pub fn new(x: bool, y: bool, z: bool, w: bool) -> Self {
         Vec4b { data: [x, y, z, w] }
    }
    
    /// Component accessor, returns the 1st component of the vector.
    #[inline(always)] pub fn x(&self) -> bool { self[0] }

    /// Component accessor, returns the 2nd component of the vector.
    #[inline(always)] pub fn y(&self) -> bool { self[1] }

    /// Component accessor, returns the 3rd component of the vector.
    #[inline(always)] pub fn z(&self) -> bool { self[2] }

    /// Component accessor, returns the 4th component of the vector.
    #[inline(always)] pub fn w(&self) -> bool { self[3] }
    
    
    /// Color-style component accessor, returns the 1st component of the vector.
    #[inline(always)] pub fn r(&self) -> bool { self[0] }

    /// Color-style component accessor, returns the 2nd component of the vector.
    #[inline(always)] pub fn g(&self) -> bool { self[1] }

    /// Color-style component accessor, returns the 3rd component of the vector.
    #[inline(always)] pub fn b(&self) -> bool { self[2] }

    /// Color-style component accessor, returns the 4th component of the vector.
    #[inline(always)] pub fn a(&self) -> bool { self[3] }
    
    /// Returns true if all components of the vector are true, false otherwise.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// assert!(bvec4!(true, true, true, true).all());
    /// assert!(!bvec4!(true, false, true, true).all());
    /// # }
    /// ```
    pub fn all(&self) -> bool {
        self.x() && self.y() && self.z() && self.w()
    }
    
    /// Returns true if at least one of the vector components is true, false otherwise.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// assert!(bvec4!(false, true, false, false).any());
    /// assert!(!bvec4!(false, false, false, false).any());
    /// # }
    /// ```
    pub fn any(&self) -> bool {
        self.x() || self.y() || self.z() || self.w()
    }
    
    /// Performs component-wise negation of the vector, returning a new vector.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let u = bvec4!(true, false, true, false);
    /// assert_eq!(u.not(), bvec4!(false, true, false, true));
    /// # }
    /// ```
    pub fn not(&self) -> Vec4b {
        Vec4b::new(!self.x(), !self.y(), !self.z(), !self.w())
    }
}

impl Display for Vec4b {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "Vec4b({}, {}, {}, {})", self[0], self[1], self[2], self[3])
    }
}

impl Index<usize> for Vec4b {
    type Output = bool;
    
    /// Index notation for acessing components of a vector.
    ///
    /// Caveat: due to language constraints, index-based accessors are slower than corresponding
    /// method-based accessors for SIMD implementation.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let u = bvec4!(true, false, true, false);
    /// assert_eq!(u, bvec4!(u[0], u[1], u[2], u[3]));
    /// # }
    /// ```
    ///
    /// # Panics
    ///
    /// Will panic if the index is greater than 3.
    #[inline(always)] fn index<'a>(&'a self, i: usize) -> &'a bool {
        &self.data[i]
    }
}

impl IndexMut<usize> for Vec4b {

    /// Index notation for mutating components of a vector.
    ///
    /// Caveat: due to language constraints, index-based accessors are slower than corresponding
    /// method-based accessors for SIMD implementation.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let mut u = bvec4!(true, false, true, false);
    /// u[0] = true; u[1] = true; u[2] = false; u[3] = true;
    /// assert_eq!(u, bvec4!(true, true, false, true));
    /// # }
    /// ```
    ///
    /// # Panics
    ///
    /// Will panic if the index is greater than 3.
    #[inline(always)] fn index_mut<'a>(&'a mut self, i: usize) -> &'a mut bool {
        &mut self.data[i]
    }
}

impl<'a, 'b> BitAnd<&'b Vec4b> for &'a Vec4b {
    type Output = Vec4b;

    /// Performs logical *and* between each component of the `lhs` vector
    /// and the corresponding component of the `rhs` vector, producing a new vector.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let u = bvec4!(true, false, true, false) & bvec4!(true, true, false, true);
    /// assert_eq!(u, bvec4!(true & true, false & true, true & false, false & true));
    /// # }
    /// ```
    fn bitand(self, rhs: &Vec4b) -> Vec4b {
        Vec4b::new(self.x() & rhs.x(), self.y() & rhs.y(), self.z() & rhs.z(), self.w() & rhs.w())
    }
}

impl<'a> BitAnd<Vec4b> for &'a Vec4b {
    type Output = Vec4b;
    
    /// Shorthand for `lhs & &rhs`.
    #[inline(always)] fn bitand(self, rhs: Vec4b) -> Vec4b {
        self & &rhs
    }
}

impl<'b> BitAnd<&'b Vec4b> for Vec4b {
    type Output = Vec4b;
    
    /// Shorthand for `&lhs & rhs`.
    #[inline(always)] fn bitand(self, rhs: &Vec4b) -> Vec4b {
        &self & rhs
    }
}

impl BitAnd<Vec4b> for Vec4b {
    type Output = Vec4b;
    
    /// Shorthand for `&lhs & &rhs`.
    #[inline(always)] fn bitand(self, rhs: Vec4b) -> Vec4b {
        &self & &rhs
    }
}

impl<'a> BitAnd<bool> for &'a Vec4b {
    type Output = Vec4b;
    
    /// Performs logical *and* between each component of a vector
    /// and a scalar, producing a new vector.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let u = bvec4!(true, false, true, false) & true;
    /// assert_eq!(u, bvec4!(true & true, false & true, true & true, false & true));
    /// # }
    /// ```
    fn bitand(self, rhs: bool) -> Vec4b {
        Vec4b::new(self.x() & rhs, self.y() & rhs, self.z() & rhs, self.w() & rhs)
    }
}

impl BitAnd<bool> for Vec4b {
    type Output = Vec4b;
    
    /// Shorthand for `&lhs & rhs`.
    #[inline(always)] fn bitand(self, rhs: bool) -> Vec4b {
        &self & rhs
    }
}

impl<'a, 'b> BitOr<&'b Vec4b> for &'a Vec4b {
    type Output = Vec4b;

    /// Performs logical *or* between each component of the `lhs` vector
    /// and the corresponding component of the `rhs` vector, producing a new vector.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let u = bvec4!(true, false, true, false) | bvec4!(true, true, false, true);
    /// assert_eq!(u, bvec4!(true | true, false | true, true | false, false | true));
    /// # }
    /// ```
    fn bitor(self, rhs: &Vec4b) -> Vec4b {
        Vec4b::new(self.x() | rhs.x(), self.y() | rhs.y(), self.z() | rhs.z(), self.w() | rhs.w())
    }
}

impl<'a> BitOr<Vec4b> for &'a Vec4b {
    type Output = Vec4b;
    
    /// Shorthand for `lhs | &rhs`.
    #[inline(always)] fn bitor(self, rhs: Vec4b) -> Vec4b {
        self | &rhs
    }
}

impl<'b> BitOr<&'b Vec4b> for Vec4b {
    type Output = Vec4b;
    
    /// Shorthand for `&lhs | rhs`.
    #[inline(always)] fn bitor(self, rhs: &Vec4b) -> Vec4b {
        &self | rhs
    }
}

impl BitOr<Vec4b> for Vec4b {
    type Output = Vec4b;
    
    /// Shorthand for `&lhs | &rhs`.
    #[inline(always)] fn bitor(self, rhs: Vec4b) -> Vec4b {
        &self | &rhs
    }
}

impl<'a> BitOr<bool> for &'a Vec4b {
    type Output = Vec4b;
    
    /// Performs logical *or* between each component of a vector
    /// and a scalar, producing a new vector.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let u = bvec4!(true, false, true, false) | true;
    /// assert_eq!(u, bvec4!(true | true, false | true, true | true, false | true));
    /// # }
    /// ```
    fn bitor(self, rhs: bool) -> Vec4b {
        Vec4b::new(self.x() | rhs, self.y() | rhs, self.z() | rhs, self.w() | rhs)
    }
}

impl BitOr<bool> for Vec4b {
    type Output = Vec4b;
    
    /// Shorthand for `&lhs | rhs`.
    #[inline(always)] fn bitor(self, rhs: bool) -> Vec4b {
        &self | rhs
    }
}

impl<'a, 'b> BitXor<&'b Vec4b> for &'a Vec4b {
    type Output = Vec4b;

    /// Performs logical *xor* between each component of the `lhs` vector
    /// and the corresponding component of the `rhs` vector, producing a new vector.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let u = bvec4!(true, false, true, false) ^ bvec4!(true, true, false, true);
    /// assert_eq!(u, bvec4!(true ^ true, false ^ true, true ^ false, false ^ true));
    /// # }
    /// ```
    fn bitxor(self, rhs: &Vec4b) -> Vec4b {
        Vec4b::new(self.x() ^ rhs.x(), self.y() ^ rhs.y(), self.z() ^ rhs.z(), self.w() ^ rhs.w())
    }
}

impl<'a> BitXor<Vec4b> for &'a Vec4b {
    type Output = Vec4b;
    
    /// Shorthand for `lhs ^ &rhs`.
    #[inline(always)] fn bitxor(self, rhs: Vec4b) -> Vec4b {
        self ^ &rhs
    }
}

impl<'b> BitXor<&'b Vec4b> for Vec4b {
    type Output = Vec4b;
    
    /// Shorthand for `&lhs ^ rhs`.
    #[inline(always)] fn bitxor(self, rhs: &Vec4b) -> Vec4b {
        &self ^ rhs
    }
}

impl BitXor<Vec4b> for Vec4b {
    type Output = Vec4b;
    
    /// Shorthand for `&lhs ^ &rhs`.
    #[inline(always)] fn bitxor(self, rhs: Vec4b) -> Vec4b {
        &self ^ &rhs
    }
}

impl<'a> BitXor<bool> for &'a Vec4b {
    type Output = Vec4b;
    
    /// Performs logical *xor* between each component of a vector
    /// and a scalar, producing a new vector.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let u = bvec4!(true, false, true, false) ^ true;
    /// assert_eq!(u, bvec4!(true ^ true, false ^ true, true ^ true, false ^ true));
    /// # }
    /// ```
    fn bitxor(self, rhs: bool) -> Vec4b {
        Vec4b::new(self.x() ^ rhs, self.y() ^ rhs, self.z() ^ rhs, self.w() ^ rhs)
    }
}

impl BitXor<bool> for Vec4b {
    type Output = Vec4b;
    
    /// Shorthand for `&lhs ^ rhs`.
    #[inline(always)] fn bitxor(self, rhs: bool) -> Vec4b {
        &self ^ rhs
    }
}

impl<'a> Not for &'a Vec4b {
    type Output = Vec4b;
    
    /// Applies logical negation to each component of a vector, producing a new vector.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let u = bvec4!(true, false, true, false);
    /// assert_eq!(!u, bvec4!(!true, !false, !true, !false));
    /// # }
    /// ```
    fn not(self) -> Vec4b {
        Vec4b::new(!self.x(), !self.y(), !self.z(), !self.w())
    }
}

impl Not for Vec4b {
    type Output = Vec4b;
    
    /// Shorthand for `!&arg`.
    #[inline(always)] fn not(self) -> Vec4b {
        !&self
    }
}

/// Builder macro for creating new 4-dimensional vectors with boolean components.
///
/// # Examples
///
/// Create a new vector with all components set to `true`:
///
/// ```
/// #[macro_use] extern crate vexyz_math;
/// use vexyz_math::*;
///
/// # fn main() {
/// let u = bvec4!(true);
/// assert_eq!(u, Vec4b::new(true, true, true, true));
/// # }
/// ```
/// 
/// Create a new vector with `x = true`, `y = true`, `z = false`, and `w = true`:
///
/// ```
/// #[macro_use] extern crate vexyz_math;
/// use vexyz_math::*;
///
/// # fn main() {
/// let u = bvec4!(true, true, false, true);
/// assert_eq!(u, Vec4b::new(true, true, false, true));
/// # }
/// ```
#[macro_export]
macro_rules! bvec4 {
    ($s:expr) => {{
        let s = $s as bool;
        Vec4b::new(s, s, s, s)
    }};
    ($x:expr, $y:expr, $z:expr, $w:expr) => {{
        Vec4b::new($x as bool, $y as bool, $z as bool, $w as bool)
    }};
    ($x:expr, $y:expr, $z:expr, $w:expr,) => {{
        Vec4b::new($x as bool, $y as bool, $z as bool, $w as bool)
    }};
}
