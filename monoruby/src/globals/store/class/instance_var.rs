use super::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(transparent)]
pub struct IvarId(u32);

impl IvarId {
    pub(crate) fn new(id: u32) -> Self {
        Self(id)
    }

    pub(crate) fn into_usize(self) -> usize {
        self.0 as usize
    }

    pub(crate) fn get(&self) -> u32 {
        self.0
    }

    pub(crate) fn is_inline(&self) -> bool {
        self.into_usize() < OBJECT_INLINE_IVAR
    }
}

impl Store {
    ///
    /// Get the value of a instance variable with *name* which belongs to *val*.
    ///
    pub(crate) fn get_ivar(&self, val: Value, name: IdentId) -> Option<Value> {
        let rval = val.try_rvalue()?;
        rval.get_ivar(self, name)
    }

    pub(crate) fn get_ivars(&self, mut val: Value) -> Vec<(IdentId, Value)> {
        let class_id = val.class();
        let rval = match val.try_rvalue_mut() {
            Some(rval) => rval,
            None => return vec![],
        };
        self.classes[class_id]
            .ivar_names
            .iter()
            .filter_map(|(name, id)| rval.get_ivar_by_ivarid(*id).map(|v| (*name, v)))
            .collect()
    }

    ///
    /// Set *val* to the instance variable with *name* which belongs to *base*.
    ///
    pub(crate) fn set_ivar(&mut self, mut base: Value, name: IdentId, val: Value) -> Result<()> {
        let class_id = base.class();
        let rval = match base.try_rvalue_mut() {
            Some(rval) => rval,
            None => {
                return Err(MonorubyErr::cant_modify_frozen(self, base));
            }
        };
        let id = self.get_ivar_id(class_id, name);
        rval.set_ivar_by_ivarid(id, val);
        Ok(())
    }

    pub(crate) fn get_ivar_id(&mut self, class_id: ClassId, ivar_name: IdentId) -> IvarId {
        let table = &mut self.classes[class_id].ivar_names;
        match table.get(&ivar_name) {
            Some(id) => *id,
            None => {
                let id = IvarId(table.len() as u32);
                table.insert(ivar_name, id);
                id
            }
        }
    }
}

#[test]
fn test_ivar() {
    let mut globals = Globals::new(0, false, true);
    let obj = Value::object(OBJECT_CLASS);
    assert_eq!(None, globals.store.get_ivar(obj, IdentId::INITIALIZE));
    assert!(globals
        .store
        .set_ivar(obj, IdentId::INITIALIZE, Value::fixnum(42))
        .is_ok());
    assert_eq!(
        Some(Value::fixnum(42)),
        globals.store.get_ivar(obj, IdentId::INITIALIZE)
    );
}
