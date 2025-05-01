///
/// Wrapper of ID for local variables.
///
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct LvarId(usize);

impl LvarId {
    #[inline(always)]
    pub fn as_usize(&self) -> usize {
        self.0
    }

    #[inline(always)]
    pub fn as_u32(&self) -> u32 {
        self.0 as u32
    }
}

impl From<usize> for LvarId {
    #[inline(always)]
    fn from(id: usize) -> Self {
        LvarId(id)
    }
}

impl From<LvarId> for usize {
    #[inline(always)]
    fn from(id: LvarId) -> usize {
        id.0
    }
}

impl From<u32> for LvarId {
    #[inline(always)]
    fn from(id: u32) -> Self {
        LvarId(id as usize)
    }
}

///
/// The struct which holds various information about local variables in a certain instruction sequence.
///
#[derive(Debug, Clone, PartialEq, Default)]
pub struct LvarCollector {
    pub kw: Vec<LvarId>,
    pub table: LvarTable,
    kwrest: Option<LvarId>,
    block: Option<LvarId>,
    pub forwarding_param: Option<LvarId>,
    pub numbered_param: Option<crate::Loc>,
    pub prohibit_numbered_param: Option<crate::Loc>,
}

impl LvarCollector {
    pub fn from(id: String) -> Self {
        let mut table = LvarTable::new();
        table.push(id);
        Self {
            kw: vec![],
            table,
            kwrest: None,
            block: None,
            forwarding_param: None,
            numbered_param: None,
            prohibit_numbered_param: None,
        }
    }
}

impl LvarCollector {
    /// Create new `LvarCollector`.
    pub fn new() -> Self {
        LvarCollector {
            kw: vec![],
            table: LvarTable::new(),
            kwrest: None,
            block: None,
            forwarding_param: None,
            numbered_param: None,
            prohibit_numbered_param: None,
        }
    }

    /// Check whether `val` exists in `LvarCollector` or not, and return `LvarId` if exists.
    /// If not, add new variable `val` to the `LvarCollector`.
    pub fn insert(&mut self, val: &str) -> LvarId {
        match self.table.get_lvarid(val) {
            Some(id) => id,
            None => {
                self.table.push(val.to_string());
                (self.len() - 1).into()
            }
        }
    }

    /// Add a new variable `val` to the `LvarCollector`.
    /// Return None if `val` already exists.
    pub fn insert_new(&mut self, val: String) -> Option<LvarId> {
        match self.table.get_lvarid(&val) {
            Some(_) => None,
            None => {
                self.table.push(val);
                Some(LvarId::from(self.len() - 1))
            }
        }
    }

    /// Add a new block parameter `val` to the `LvarCollector`.
    /// Return None if `val` already exists.
    pub fn insert_block_param(&mut self, val: String) -> Option<LvarId> {
        let lvar = self.insert_new(val)?;
        self.block = Some(lvar);
        Some(lvar)
    }

    /// Add a new keyword parameter `val` to the `LvarCollector`.
    /// Return None if `val` already exists.
    pub fn insert_kwrest_param(&mut self, val: String) -> Option<LvarId> {
        let lvar = self.insert_new(val)?;
        self.kwrest = Some(lvar);
        Some(lvar)
    }

    /// Add a delegate parameter `val` to the `LvarCollector`.
    /// Return None if `val` already exists.
    pub fn insert_delegate_param(&mut self) -> Option<LvarId> {
        let lvar = self.insert_new("...".to_string())?;
        self.forwarding_param = Some(lvar);
        Some(lvar)
    }

    fn get_name_id(&self, id: LvarId) -> Option<String> {
        self.table.get(id.into())
    }

    /// Get name string of `id`.
    pub fn get_name(&self, id: LvarId) -> String {
        match self.get_name_id(id) {
            Some(id) => format!("{:?}", id),
            None => "<unnamed>".to_string(),
        }
    }

    #[inline(always)]
    pub fn kwrest_param(&self) -> Option<LvarId> {
        self.kwrest
    }

    #[inline(always)]
    pub fn block_param(&self) -> Option<LvarId> {
        self.block
    }

    #[inline(always)]
    pub fn len(&self) -> usize {
        self.table.0.len()
    }

    #[inline(always)]
    pub fn is_empty(&self) -> bool {
        self.table.0.is_empty()
    }

    #[inline(always)]
    pub fn table(&self) -> &Vec<String> {
        &self.table.0
    }

    #[inline(always)]
    pub fn block(&self) -> &Option<LvarId> {
        &self.block
    }
}

#[derive(Debug, Clone, PartialEq, Default)]
pub struct LvarTable(pub Vec<String>);

impl LvarTable {
    pub fn new() -> Self {
        Self(vec![])
    }

    pub fn get_lvarid(&self, name: &str) -> Option<LvarId> {
        self.0.iter().position(|i| i == name).map(LvarId::from)
    }

    fn push(&mut self, name: String) {
        self.0.push(name)
    }

    fn get(&self, i: usize) -> Option<String> {
        self.0.get(i).cloned()
    }
}
