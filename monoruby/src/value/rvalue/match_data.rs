use super::*;

#[derive(Debug, Clone)]
#[repr(C)]
pub struct MatchDataInner {
    regex: Value,
    heystack: String,
    matches: Box<Vec<Option<(usize, usize)>>>,
}

impl GC<RValue> for MatchDataInner {
    fn mark(&self, alloc: &mut Allocator<RValue>) {
        self.regex.mark(alloc);
    }
}

impl MatchDataInner {
    pub fn new(regex: Value, heystack: String, matches: Vec<Option<(usize, usize)>>) -> Self {
        MatchDataInner {
            regex,
            heystack,
            matches: Box::new(matches),
        }
    }

    pub fn from_capture(captures: Captures, heystack: String, regex: Value) -> Self {
        let matches = captures
            .iter()
            .map(|m| {
                if let Some(m) = m {
                    Some((m.start(), m.end()))
                } else {
                    None
                }
            })
            .collect();
        MatchDataInner::new(regex, heystack, matches)
    }

    pub fn regexp(&self) -> Value {
        self.regex
    }

    pub fn string(&self) -> &str {
        &self.heystack
    }

    pub fn pos(&self, pos: usize) -> Option<(usize, usize)> {
        self.matches[pos]
    }

    pub fn at(&self, pos: usize) -> Option<&str> {
        self.matches[pos].map(|(start, end)| &self.heystack[start..end])
    }

    pub fn len(&self) -> usize {
        self.matches.len()
    }

    pub fn captures(&self) -> impl Iterator<Item = Option<&str>> {
        self.matches
            .iter()
            .map(|m| m.map(|(start, end)| &self.heystack[start..end]))
    }

    pub fn to_s(&self) -> String {
        self.at(0).unwrap().to_string()
    }

    pub fn inspect(&self) -> String {
        let mut s = format!("#<MatchData \"{}\"", self.at(0).unwrap());
        for i in 1..self.len() {
            if let Some(capture) = self.at(i) {
                s.push_str(&format!(" {i}:\"{}\"", capture));
            } else {
                s.push_str(&format!(" {i}:nil"));
            }
        }
        s.push_str(">");
        s
    }
}
