use gen_common::*;
use std::iter::*;
use util::*;

pub static COLS: [&'static str; 4] = ["col0", "col1", "col2", "col3"];

pub struct MatGen {
    pub struct_name: String,
    pub tpe: Type,
    pub col_tpe: String,
    pub col_builder: String,
    pub nr_cols: usize,
    pub nr_rows: usize,
    pub macro_builder_name: String,
}

impl MatGen {
    pub fn sized_imports(&self) -> String {
        format!("use vec{n}::*;", n = self.nr_rows)
    }
    
    pub fn getters(&self) -> Box<Iterator<Item = String>> {
        Box::new((0 .. self.nr_cols).map(|i| mat_getter(i)))
    }
    
    pub fn cols(&self) -> Box<Iterator<Item = String>> {
        Box::new(COLS.iter().take(self.nr_cols).map(|s| s.to_string()))
    }
    
    pub fn lhs(&self, c: usize, r: usize) -> String {
        format!("{}{}.0", c + 1, r + 1)
    }
    
    pub fn rhs(&self, c: usize, r: usize) -> String {
        format!("{}{}.0", c + 1, r + 5)
    }
    
    pub fn lhs_col(&self, c: usize) -> String {
        (0..self.nr_rows).map(|r| self.lhs(c, r)).concat(", ")
    }
    
    pub fn rhs_col(&self, c: usize) -> String {
        (0..self.nr_rows).map(|r| self.rhs(c, r)).concat(", ")
    }
    
    pub fn doc_orthogonal_matrix(&self) -> String {"\
    /// When all the matrix columns are orthogonal to each other, and each column has a unit length,
    /// then the matrix is called 'orthogonal matrix'. Orthogonal matrices represent rotation.
    /// Transpose of an orthogonal matrix is equal to its inverse.".to_string()
    }
}
