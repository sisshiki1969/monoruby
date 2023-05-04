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
}

impl Globals {
    fn get_ivar_id(&mut self, class_id: ClassId, ivar_name: IdentId) -> IvarId {
        let table = &mut self.class[class_id].ivar_names;
        match table.get(&ivar_name) {
            Some(id) => *id,
            None => {
                let id = IvarId(table.len() as u32);
                table.insert(ivar_name, id);
                id
            }
        }
    }

    ///
    /// Get the value of a instance variable with *name* which belongs to *val*.
    ///
    pub(crate) fn get_ivar(&self, mut val: Value, name: IdentId) -> Option<Value> {
        let class_id = val.class();
        let rval = val.try_rvalue_mut()?;
        let id = self.class[class_id].ivar_names.get(&name)?;
        rval.get_var(*id)
    }

    pub(crate) fn get_ivars(&self, mut val: Value) -> Vec<(IdentId, Value)> {
        let class_id = val.class();
        let rval = match val.try_rvalue_mut() {
            Some(rval) => rval,
            None => return vec![],
        };
        self.class[class_id]
            .ivar_names
            .iter()
            .filter_map(|(name, id)| rval.get_var(*id).map(|v| (*name, v)))
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
        rval.set_var(id, val);
        Ok(())
    }
}

pub(crate) extern "C" fn get_instance_var_with_cache(
    mut base: Value,
    name: IdentId,
    globals: &mut Globals,
    cache_class: &mut ClassId,
    cache_ivarid: &mut IvarId,
) -> Value {
    let class_id = base.class();
    let rval = match base.try_rvalue_mut() {
        Some(rval) => rval,
        None => return Value::nil(),
    };
    if class_id == *cache_class {
        return rval.get_var(*cache_ivarid).unwrap_or_default();
    }
    let ivar_id = match globals.class[class_id].ivar_names.get(&name) {
        Some(id) => *id,
        None => return Value::nil(),
    };
    *cache_class = class_id;
    *cache_ivarid = ivar_id;
    rval.get_var(ivar_id).unwrap_or_default()
}

#[repr(C)]
pub(crate) struct InstanceVarCache {
    class_id: ClassId,
    ivar_id: IvarId,
}

pub(crate) extern "C" fn set_instance_var_with_cache(
    globals: &mut Globals,
    mut base: Value,
    name: IdentId,
    val: Value,
    cache: &mut InstanceVarCache,
) -> Option<Value> {
    let class_id = base.class();
    let rval = match base.try_rvalue_mut() {
        Some(rval) => rval,
        None => {
            vm.err_cant_modify_frozen(base);
            return None;
        }
    };
    if class_id == cache.class_id {
        rval.set_var(cache.ivar_id, val);
        return Some(Value::nil());
    }
    let ivar_id = globals.get_ivar_id(class_id, name);
    cache.class_id = class_id;
    cache.ivar_id = ivar_id;
    rval.set_var(ivar_id, val);
    Some(Value::nil())
}

#[test]
fn test_ivar() {
    let mut globals = Globals::new(0, false);
    let obj = Value::new_object(OBJECT_CLASS);
    assert_eq!(None, globals.get_ivar(obj, IdentId::INITIALIZE));
    assert!(globals
        .set_ivar(obj, IdentId::INITIALIZE, Value::fixnum(42))
        .is_ok());
    assert_eq!(
        Some(Value::fixnum(42)),
        globals.get_ivar(obj, IdentId::INITIALIZE)
    );
}
