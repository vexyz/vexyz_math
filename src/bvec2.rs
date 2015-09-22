// Generated code.
use std::fmt::{Display, Formatter, Result};
use std::ops::*;

/// 2-dimensional vector with boolean `x`, and `y` components.
///
/// Vectors can also represent colors with the help of
/// `r()`, and `g()` accessors.
///
/// Most operators and methods on vectors are performed in component-wise fashion. The notable
/// exception is vector-matrix multiplication.
#[derive(Debug, PartialEq, Copy, Clone)]
pub struct Vec2b { data: [bool; 2] }

impl Vec2b {
    /// Constructs a new `Vec2b`.
    pub fn new(x: bool, y: bool) -> Self {
         Vec2b { data: [x, y] }
    }
    
    /// Component accessor, returns the 1st component of the vector.
    #[inline(always)] pub fn x(&self) -> bool { self[0] }

    /// Component accessor, returns the 2nd component of the vector.
    #[inline(always)] pub fn y(&self) -> bool { self[1] }
    
	
    /// Color-style component accessor, returns the 1st component of the vector.
    #[inline(always)] pub fn r(&self) -> bool { self[0] }

    /// Color-style component accessor, returns the 2nd component of the vector.
    #[inline(always)] pub fn g(&self) -> bool { self[1] }
	
	/// Returns true if all components of the vector are true, false otherwise.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// assert!(bvec2!(true, true).all());
    /// assert!(!bvec2!(true, false).all());
    /// # }
    /// ```
	pub fn all(&self) -> bool {
		self[0] && self[1]
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
    /// assert!(bvec2!(false, true).any());
    /// assert!(!bvec2!(false, false).any());
    /// # }
    /// ```
	pub fn any(&self) -> bool {
		self[0] || self[1]
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
    /// let u = bvec2!(true, false);
    /// assert_eq!(u.not(), bvec2!(false, true));
    /// # }
    /// ```
	pub fn not(&self) -> Vec2b {
		Vec2b::new(!self[0], !self[1])
	}
}

impl Display for Vec2b {
    fn fmt(&self, f: &mut Formatter) -> Result {
    	write!(f, "Vec2b({}, {})", self[0], self[1])
    }
}

impl Index<usize> for Vec2b {
    type Output = bool;
    
    /// Index notation for acessing components of a vector.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let u = bvec2!(true, false);
    /// assert_eq!(u, bvec2!(u[0], u[1]));
    /// # }
    /// ```
    ///
    /// # Panics
    ///
    /// Will panic if the index is greater than 1.
    #[inline(always)] fn index<'a>(&'a self, i: usize) -> &'a bool {
        &self.data[i]
    }
}

impl<'a, 'b> BitAnd<&'b Vec2b> for &'a Vec2b {
    type Output = Vec2b;

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
    /// let u = bvec2!(true, false) & bvec2!(true, true);
    /// assert_eq!(u, bvec2!(true & true, false & true));
    /// # }
    /// ```
    fn bitand(self, rhs: &Vec2b) -> Vec2b {
        Vec2b::new(self[0] & rhs[0], self[1] & rhs[1])
    }
}

impl<'a> BitAnd<Vec2b> for &'a Vec2b {
    type Output = Vec2b;
    
    /// Shorthand for `lhs & &rhs`.
    #[inline(always)] fn bitand(self, rhs: Vec2b) -> Vec2b {
        self & &rhs
    }
}

impl<'b> BitAnd<&'b Vec2b> for Vec2b {
    type Output = Vec2b;
    
    /// Shorthand for `&lhs & rhs`.
    #[inline(always)] fn bitand(self, rhs: &Vec2b) -> Vec2b {
        &self & rhs
    }
}

impl BitAnd<Vec2b> for Vec2b {
    type Output = Vec2b;
    
    /// Shorthand for `&lhs & &rhs`.
    #[inline(always)] fn bitand(self, rhs: Vec2b) -> Vec2b {
        &self & &rhs
    }
}

impl<'a> BitAnd<bool> for &'a Vec2b {
    type Output = Vec2b;
    
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
    /// let u = bvec2!(true, false) & true;
    /// assert_eq!(u, bvec2!(true & true, false & true));
    /// # }
    /// ```
    fn bitand(self, rhs: bool) -> Vec2b {
        Vec2b::new(self[0] & rhs, self[1] & rhs)
    }
}

impl BitAnd<bool> for Vec2b {
    type Output = Vec2b;
    
    /// Shorthand for `&lhs & rhs`.
    #[inline(always)] fn bitand(self, rhs: bool) -> Vec2b {
        &self & rhs
    }
}

impl<'a, 'b> BitOr<&'b Vec2b> for &'a Vec2b {
    type Output = Vec2b;

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
    /// let u = bvec2!(true, false) | bvec2!(true, true);
    /// assert_eq!(u, bvec2!(true | true, false | true));
    /// # }
    /// ```
    fn bitor(self, rhs: &Vec2b) -> Vec2b {
        Vec2b::new(self[0] | rhs[0], self[1] | rhs[1])
    }
}

impl<'a> BitOr<Vec2b> for &'a Vec2b {
    type Output = Vec2b;
    
    /// Shorthand for `lhs | &rhs`.
    #[inline(always)] fn bitor(self, rhs: Vec2b) -> Vec2b {
        self | &rhs
    }
}

impl<'b> BitOr<&'b Vec2b> for Vec2b {
    type Output = Vec2b;
    
    /// Shorthand for `&lhs | rhs`.
    #[inline(always)] fn bitor(self, rhs: &Vec2b) -> Vec2b {
        &self | rhs
    }
}

impl BitOr<Vec2b> for Vec2b {
    type Output = Vec2b;
    
    /// Shorthand for `&lhs | &rhs`.
    #[inline(always)] fn bitor(self, rhs: Vec2b) -> Vec2b {
        &self | &rhs
    }
}

impl<'a> BitOr<bool> for &'a Vec2b {
    type Output = Vec2b;
    
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
    /// let u = bvec2!(true, false) | true;
    /// assert_eq!(u, bvec2!(true | true, false | true));
    /// # }
    /// ```
    fn bitor(self, rhs: bool) -> Vec2b {
        Vec2b::new(self[0] | rhs, self[1] | rhs)
    }
}

impl BitOr<bool> for Vec2b {
    type Output = Vec2b;
    
    /// Shorthand for `&lhs | rhs`.
    #[inline(always)] fn bitor(self, rhs: bool) -> Vec2b {
        &self | rhs
    }
}

impl<'a, 'b> BitXor<&'b Vec2b> for &'a Vec2b {
    type Output = Vec2b;

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
    /// let u = bvec2!(true, false) ^ bvec2!(true, true);
    /// assert_eq!(u, bvec2!(true ^ true, false ^ true));
    /// # }
    /// ```
    fn bitxor(self, rhs: &Vec2b) -> Vec2b {
        Vec2b::new(self[0] ^ rhs[0], self[1] ^ rhs[1])
    }
}

impl<'a> BitXor<Vec2b> for &'a Vec2b {
    type Output = Vec2b;
    
    /// Shorthand for `lhs ^ &rhs`.
    #[inline(always)] fn bitxor(self, rhs: Vec2b) -> Vec2b {
        self ^ &rhs
    }
}

impl<'b> BitXor<&'b Vec2b> for Vec2b {
    type Output = Vec2b;
    
    /// Shorthand for `&lhs ^ rhs`.
    #[inline(always)] fn bitxor(self, rhs: &Vec2b) -> Vec2b {
        &self ^ rhs
    }
}

impl BitXor<Vec2b> for Vec2b {
    type Output = Vec2b;
    
    /// Shorthand for `&lhs ^ &rhs`.
    #[inline(always)] fn bitxor(self, rhs: Vec2b) -> Vec2b {
        &self ^ &rhs
    }
}

impl<'a> BitXor<bool> for &'a Vec2b {
    type Output = Vec2b;
    
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
    /// let u = bvec2!(true, false) ^ true;
    /// assert_eq!(u, bvec2!(true ^ true, false ^ true));
    /// # }
    /// ```
    fn bitxor(self, rhs: bool) -> Vec2b {
        Vec2b::new(self[0] ^ rhs, self[1] ^ rhs)
    }
}

impl BitXor<bool> for Vec2b {
    type Output = Vec2b;
    
    /// Shorthand for `&lhs ^ rhs`.
    #[inline(always)] fn bitxor(self, rhs: bool) -> Vec2b {
        &self ^ rhs
    }
}

impl<'a> Not for &'a Vec2b {
    type Output = Vec2b;
    
    /// Applies logical negation to each component of a vector, producing a new vector.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use] extern crate vexyz_math;
    /// use vexyz_math::*;
    ///
    /// # fn main() {
    /// let u = bvec2!(true, false);
    /// assert_eq!(!u, bvec2!(!true, !false));
    /// # }
    /// ```
    fn not(self) -> Vec2b {
        Vec2b::new(!self[0], !self[1])
    }
}

impl Not for Vec2b {
    type Output = Vec2b;
    
    /// Shorthand for `!&arg`.
    #[inline(always)] fn not(self) -> Vec2b {
        !&self
    }
}

/// Builder macro for creating new 2-dimensional vectors with boolean components.
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
/// let u = bvec2!(true);
/// assert_eq!(u, Vec2b::new(true, true));
/// # }
/// ```
/// 
/// Create a new vector with `x = true`, and `y = true`:
///
/// ```
/// #[macro_use] extern crate vexyz_math;
/// use vexyz_math::*;
///
/// # fn main() {
/// let u = bvec2!(true, true);
/// assert_eq!(u, Vec2b::new(true, true));
/// # }
/// ```
#[macro_export]
macro_rules! bvec2 {
    ($s:expr) => {{
        let s = $s as bool;
        Vec2b::new(s, s)
    }};
    ($x:expr, $y:expr) => {{
        Vec2b::new($x as bool, $y as bool)
    }};
    ($x:expr, $y:expr,) => {{
        Vec2b::new($x as bool, $y as bool)
    }};
}
