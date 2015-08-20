pub trait StringColOps {
    fn concat(mut self, sep: &str) -> String;
    fn mk_string(mut self, pref: &str, sep: &str, suff: &str) -> String;
    fn worded(mut self) -> String;
}

impl<T> StringColOps for T where T: Iterator<Item = String> {
    fn concat(self, sep: &str) -> String {
        let mut buf = String::new();
        for item in self {
            if !buf.is_empty() { buf.push_str(sep); }
            buf.push_str(&item);
        }
        buf
    }
    fn mk_string(self, pref: &str, sep: &str, suff: &str) -> String {
        let mut buf = String::new();
        buf.push_str(pref);
        buf.push_str(&self.concat(sep));
        buf.push_str(suff);
        buf
    }
    fn worded(self) -> String {
        let mut peekable = self.peekable();
        let mut buf = String::new();
        while let Some(item) = peekable.next() {
            if !buf.is_empty() {
                if peekable.peek().is_some() { buf.push_str(", "); }
                else { buf.push_str(", and "); }
            }
            buf.push_str(&item);
        }
        buf
    }
}

pub trait PrefixLines {
    fn prefix_lines(&self, pref: &str) -> String;
}

impl PrefixLines for String {
    fn prefix_lines(&self, pref: &str) -> String {
        self.split('\n').map(|line| format!("{}{}", pref, line)).concat("\n")
    }
}

pub fn nth(index: usize) -> String {
    match index {
        0 => "1st".to_string(),
        1 => "2nd".to_string(),
        2 => "3rd".to_string(),
        3 => "4th".to_string(),
        _ => unreachable!(),
    }
}
