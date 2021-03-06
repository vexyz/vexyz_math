// Generated code.
use std::fmt::{Display, Formatter, Result};
use std::ops::*;

/// 3-dimensional vector with boolean `x`, `y`, and `z` components.
///
/// Vectors can also represent colors with the help of
/// `r()`, `g()`, and `b()` accessors.
///
/// Most operators and methods on vectors are performed in component-wise fashion. The notable
/// exception is vector-matrix multiplication.
#[derive(Debug, PartialEq, Copy, Clone)]
pub struct Vec3b { data: [bool; 3] }

impl Vec3b {
    /// Constructs a new `Vec3b`.
    pub fn new(x: bool, y: bool, z: bool) -> Self {
         Vec3b { data: [x, y, z] }
    }
    
    /// Component accessor, returns the 1st component of the vector.
    #[inline(always)] pub fn x(&self) -> bool { self[0] }

    /// Component accessor, returns the 2nd component of the vector.
    #[inline(always)] pub fn y(&self) -> bool { self[1] }

    /// Component accessor, returns the 3rd component of the vector.
    #[inline(always)] pub fn z(&self) -> bool { self[2] }
    
    
    /// Color-style component accessor, returns the 1st component of the vector.
    #[inline(always)] pub fn r(&self) -> bool { self[0] }

    /// Color-style component accessor, returns the 2nd component of the vector.
    #[inline(always)] pub fn g(&self) -> bool { self[1] }

    /// Color-style component accessor, returns the 3rd component of the vector.
    #[inline(always)] pub fn b(&self) -> bool { self[2] }
    
    /// Returns true if all components of the vector are true, false otherwise.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// assert!(bvec3!(true, true, true).all());
    /// assert!(!bvec3!(true, false, true).all());
    /// # }
    /// ```
    pub fn all(&self) -> bool {
        self.x() && self.y() && self.z()
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
    /// assert!(bvec3!(false, true, false).any());
    /// assert!(!bvec3!(false, false, false).any());
    /// # }
    /// ```
    pub fn any(&self) -> bool {
        self.x() || self.y() || self.z()
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
    /// let u = bvec3!(true, false, true);
    /// assert_eq!(u.not(), bvec3!(false, true, false));
    /// # }
    /// ```
    pub fn not(&self) -> Vec3b {
        Vec3b::new(!self.x(), !self.y(), !self.z())
    }
}

impl Display for Vec3b {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "Vec3b({}, {}, {})", self[0], self[1], self[2])
    }
}

impl Index<usize> for Vec3b {
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
    /// let u = bvec3!(true, false, true);
    /// assert_eq!(u, bvec3!(u[0], u[1], u[2]));
    /// # }
    /// ```
    ///
    /// # Panics
    ///
    /// Will panic if the index is greater than 2.
    #[inline(always)] fn index<'a>(&'a self, i: usize) -> &'a bool {
        &self.data[i]
    }
}

impl IndexMut<usize> for Vec3b {

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
    /// let mut u = bvec3!(true, false, true);
    /// u[0] = true; u[1] = true; u[2] = false;
    /// assert_eq!(u, bvec3!(true, true, false));
    /// # }
    /// ```
    ///
    /// # Panics
    ///
    /// Will panic if the index is greater than 2.
    #[inline(always)] fn index_mut<'a>(&'a mut self, i: usize) -> &'a mut bool {
        &mut self.data[i]
    }
}

impl<'a, 'b> BitAnd<&'b Vec3b> for &'a Vec3b {
    type Output = Vec3b;

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
    /// let u = bvec3!(true, false, true) & bvec3!(true, true, false);
    /// assert_eq!(u, bvec3!(true & true, false & true, true & false));
    /// # }
    /// ```
    fn bitand(self, rhs: &Vec3b) -> Vec3b {
        Vec3b::new(self.x() & rhs.x(), self.y() & rhs.y(), self.z() & rhs.z())
    }
}

impl<'a> BitAnd<Vec3b> for &'a Vec3b {
    type Output = Vec3b;
    
    /// Shorthand for `lhs & &rhs`.
    #[inline(always)] fn bitand(self, rhs: Vec3b) -> Vec3b {
        self & &rhs
    }
}

impl<'b> BitAnd<&'b Vec3b> for Vec3b {
    type Output = Vec3b;
    
    /// Shorthand for `&lhs & rhs`.
    #[inline(always)] fn bitand(self, rhs: &Vec3b) -> Vec3b {
        &self & rhs
    }
}

impl BitAnd<Vec3b> for Vec3b {
    type Output = Vec3b;
    
    /// Shorthand for `&lhs & &rhs`.
    #[inline(always)] fn bitand(self, rhs: Vec3b) -> Vec3b {
        &self & &rhs
    }
}

impl<'a> BitAnd<bool> for &'a Vec3b {
    type Output = Vec3b;
    
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
    /// let u = bvec3!(true, false, true) & true;
    /// assert_eq!(u, bvec3!(true & true, false & true, true & true));
    /// # }
    /// ```
    fn bitand(self, rhs: bool) -> Vec3b {
        Vec3b::new(self.x() & rhs, self.y() & rhs, self.z() & rhs)
    }
}

impl BitAnd<bool> for Vec3b {
    type Output = Vec3b;
    
    /// Shorthand for `&lhs & rhs`.
    #[inline(always)] fn bitand(self, rhs: bool) -> Vec3b {
        &self & rhs
    }
}

impl<'a, 'b> BitOr<&'b Vec3b> for &'a Vec3b {
    type Output = Vec3b;

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
    /// let u = bvec3!(true, false, true) | bvec3!(true, true, false);
    /// assert_eq!(u, bvec3!(true | true, false | true, true | false));
    /// # }
    /// ```
    fn bitor(self, rhs: &Vec3b) -> Vec3b {
        Vec3b::new(self.x() | rhs.x(), self.y() | rhs.y(), self.z() | rhs.z())
    }
}

impl<'a> BitOr<Vec3b> for &'a Vec3b {
    type Output = Vec3b;
    
    /// Shorthand for `lhs | &rhs`.
    #[inline(always)] fn bitor(self, rhs: Vec3b) -> Vec3b {
        self | &rhs
    }
}

impl<'b> BitOr<&'b Vec3b> for Vec3b {
    type Output = Vec3b;
    
    /// Shorthand for `&lhs | rhs`.
    #[inline(always)] fn bitor(self, rhs: &Vec3b) -> Vec3b {
        &self | rhs
    }
}

impl BitOr<Vec3b> for Vec3b {
    type Output = Vec3b;
    
    /// Shorthand for `&lhs | &rhs`.
    #[inline(always)] fn bitor(self, rhs: Vec3b) -> Vec3b {
        &self | &rhs
    }
}

impl<'a> BitOr<bool> for &'a Vec3b {
    type Output = Vec3b;
    
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
    /// let u = bvec3!(true, false, true) | true;
    /// assert_eq!(u, bvec3!(true | true, false | true, true | true));
    /// # }
    /// ```
    fn bitor(self, rhs: bool) -> Vec3b {
        Vec3b::new(self.x() | rhs, self.y() | rhs, self.z() | rhs)
    }
}

impl BitOr<bool> for Vec3b {
    type Output = Vec3b;
    
    /// Shorthand for `&lhs | rhs`.
    #[inline(always)] fn bitor(self, rhs: bool) -> Vec3b {
        &self | rhs
    }
}

impl<'a, 'b> BitXor<&'b Vec3b> for &'a Vec3b {
    type Output = Vec3b;

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
    /// let u = bvec3!(true, false, true) ^ bvec3!(true, true, false);
    /// assert_eq!(u, bvec3!(true ^ true, false ^ true, true ^ false));
    /// # }
    /// ```
    fn bitxor(self, rhs: &Vec3b) -> Vec3b {
        Vec3b::new(self.x() ^ rhs.x(), self.y() ^ rhs.y(), self.z() ^ rhs.z())
    }
}

impl<'a> BitXor<Vec3b> for &'a Vec3b {
    type Output = Vec3b;
    
    /// Shorthand for `lhs ^ &rhs`.
    #[inline(always)] fn bitxor(self, rhs: Vec3b) -> Vec3b {
        self ^ &rhs
    }
}

impl<'b> BitXor<&'b Vec3b> for Vec3b {
    type Output = Vec3b;
    
    /// Shorthand for `&lhs ^ rhs`.
    #[inline(always)] fn bitxor(self, rhs: &Vec3b) -> Vec3b {
        &self ^ rhs
    }
}

impl BitXor<Vec3b> for Vec3b {
    type Output = Vec3b;
    
    /// Shorthand for `&lhs ^ &rhs`.
    #[inline(always)] fn bitxor(self, rhs: Vec3b) -> Vec3b {
        &self ^ &rhs
    }
}

impl<'a> BitXor<bool> for &'a Vec3b {
    type Output = Vec3b;
    
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
    /// let u = bvec3!(true, false, true) ^ true;
    /// assert_eq!(u, bvec3!(true ^ true, false ^ true, true ^ true));
    /// # }
    /// ```
    fn bitxor(self, rhs: bool) -> Vec3b {
        Vec3b::new(self.x() ^ rhs, self.y() ^ rhs, self.z() ^ rhs)
    }
}

impl BitXor<bool> for Vec3b {
    type Output = Vec3b;
    
    /// Shorthand for `&lhs ^ rhs`.
    #[inline(always)] fn bitxor(self, rhs: bool) -> Vec3b {
        &self ^ rhs
    }
}

impl<'a> Not for &'a Vec3b {
    type Output = Vec3b;
    
    /// Applies logical negation to each component of a vector, producing a new vector.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let u = bvec3!(true, false, true);
    /// assert_eq!(!u, bvec3!(!true, !false, !true));
    /// # }
    /// ```
    fn not(self) -> Vec3b {
        Vec3b::new(!self.x(), !self.y(), !self.z())
    }
}

impl Not for Vec3b {
    type Output = Vec3b;
    
    /// Shorthand for `!&arg`.
    #[inline(always)] fn not(self) -> Vec3b {
        !&self
    }
}

/// Builder macro for creating new 3-dimensional vectors with boolean components.
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
/// let u = bvec3!(true);
/// assert_eq!(u, Vec3b::new(true, true, true));
/// # }
/// ```
/// 
/// Create a new vector with `x = true`, `y = true`, and `z = false`:
///
/// ```
/// #[macro_use] extern crate vexyz_math;
/// use vexyz_math::*;
///
/// # fn main() {
/// let u = bvec3!(true, true, false);
/// assert_eq!(u, Vec3b::new(true, true, false));
/// # }
/// ```
#[macro_export]
macro_rules! bvec3 {
    ($s:expr) => {{
        let s = $s as bool;
        Vec3b::new(s, s, s)
    }};
    ($x:expr, $y:expr, $z:expr) => {{
        Vec3b::new($x as bool, $y as bool, $z as bool)
    }};
    ($x:expr, $y:expr, $z:expr,) => {{
        Vec3b::new($x as bool, $y as bool, $z as bool)
    }};
}
