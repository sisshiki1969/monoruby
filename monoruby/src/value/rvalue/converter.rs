use super::*;
use crate::builtins::encoding::StreamConvertResult;

#[monoruby_object]
pub struct Converter(Value);

impl Converter {
    pub(crate) fn new(val: Value) -> Self {
        assert_eq!(val.ty(), Some(ObjTy::CONVERTER));
        Self(val)
    }
}

///
/// The payload of an `Encoding::Converter` (`ObjTy::CONVERTER`).
///
/// CRuby keeps `rb_econv_t` in the object's TypedData. monoruby kept the
/// same state in `/`-prefixed instance variables, which meant the
/// encodings were stored as *names* and re-parsed on every call, and the
/// byte buffers (pending input, held output, read-again bytes) were
/// wrapped as Ruby Strings only to be unwrapped again a moment later.
/// These are those fields as the Rust values the transcoder actually
/// works with.
///
/// Nothing here is a `Value`, so `mark` traces nothing and the object
/// has no outgoing references — the same shape as `TimeInner` or
/// `RegexpInner`.
#[derive(Debug, Clone)]
pub struct ConverterInner {
    /// The pair `Converter.new` resolved, as `#source_encoding` /
    /// `#destination_encoding` report it. Every error message names
    /// these, not whatever a BOM later settled (`src_bom`).
    src: Encoding,
    dst: Encoding,
    /// `replace:` / `#replacement=`, as the destination spells it
    /// (#1583) — the bytes and the encoding they are tagged with.
    /// `None` leaves `#replacement` to answer the destination's
    /// default.
    replacement: Option<(Vec<u8>, Encoding)>,
    /// `INVALID_REPLACE` / `UNDEF_REPLACE` plus the decorator bits,
    /// as `Converter.new` collected them.
    flags: ConverterFlag,
    /// `#finish`, or a `#primitive_convert` that reached the end of
    /// the input, has run: `#convert` raises and every later
    /// `#primitive_convert` answers `:finished` having read nothing.
    finished: bool,
    /// Source bytes read but not yet converted — a chunk that ended
    /// inside a character, or what a capped destination had no room
    /// for. The head of whatever the next call converts.
    pending: Vec<u8>,
    /// Output bytes a `dst_bytesize` cap held back mid-character, for
    /// the next call to write before it converts anything new (#1532).
    pending_out: Vec<u8>,
    /// Read-again bytes left by an `:invalid_byte_sequence` outcome,
    /// which `#putback` hands to the caller and any other call reads
    /// back first.
    readagain: Vec<u8>,
    /// The last conversion step's outcome, for `#primitive_errinfo`
    /// and `#last_error`. `None` until the first one has run.
    outcome: Option<ConverterOutcome>,
    /// The byte order a dummy `UTF-16` / `UTF-32` source's BOM named.
    /// It arrives in the first chunk only, so the chunks after it have
    /// to be told (#1576).
    src_bom: Option<Encoding>,
    /// The destination's own BOM has been written. A dummy writes one,
    /// once, ahead of the first character it emits (#1576).
    dst_bom_written: bool,
    /// The ISO-2022-JP designation in effect. The escape stays in
    /// effect across `#convert` calls and the closing one is written
    /// from `#finish` (#1609).
    iso_state: Option<u8>,
}

#[derive(Debug, Clone, Copy, Default)]
pub(crate) struct ConverterFlag(pub i64);

impl ConverterFlag {
    pub(crate) fn invalid_replace(&self) -> bool {
        self.0 & CONVERTER_FLAG_INVALID_REPLACE != 0
    }

    pub(crate) fn set_invalid_replace(&mut self) {
        self.0 |= CONVERTER_FLAG_INVALID_REPLACE
    }

    pub(crate) fn undef_replace(&self) -> bool {
        self.0 & CONVERTER_FLAG_UNDEF_REPLACE != 0
    }

    pub(crate) fn set_undef_replace(&mut self) {
        self.0 |= CONVERTER_FLAG_UNDEF_REPLACE
    }

    pub(crate) fn set_raw_flag(&mut self, flag: i64) {
        self.0 |= flag
    }
}

/// Conversion flags configured at construction (`invalid: :replace`
/// / `undef: :replace` kwargs, or the `INVALID_REPLACE` /
/// `UNDEF_REPLACE` Integer-flag bits), kept in
/// `ConverterInner::flags` alongside the decorator bits.
const CONVERTER_FLAG_INVALID_REPLACE: i64 = 0b01;
const CONVERTER_FLAG_UNDEF_REPLACE: i64 = 0b10;

/// What a conversion step stopped on, as `#primitive_errinfo` reports
/// it.
#[derive(Debug, Clone)]
pub struct ConverterOutcome {
    /// The Symbol `#primitive_convert` returned.
    pub result: StreamConvertResult,
    /// `None` when the outcome was not an error, which is what makes
    /// `#last_error` answer `nil`.
    pub error: Option<ConverterError>,
}

/// The error half of an outcome: everything `#last_error`'s exception
/// object and the error-bytes half of `#primitive_errinfo` are built
/// from.
#[derive(Debug, Clone)]
pub struct ConverterError {
    pub message: String,
    /// The hop the error happened on, as the two encoding names
    /// `#primitive_errinfo` reports.
    pub stage_src: String,
    pub stage_dst: String,
    pub error_bytes: Vec<u8>,
    pub readagain_bytes: Vec<u8>,
}

impl ConverterInner {
    /// A converter over a resolved pair, before any input has reached
    /// it.
    pub(crate) fn new(src: Encoding, dst: Encoding) -> Self {
        Self {
            src,
            dst,
            replacement: None,
            flags: ConverterFlag(0),
            finished: false,
            pending: vec![],
            pending_out: vec![],
            readagain: vec![],
            outcome: None,
            src_bom: None,
            dst_bom_written: false,
            iso_state: None,
        }
    }

    pub(crate) fn src(&self) -> Encoding {
        self.src
    }

    pub(crate) fn dst(&self) -> Encoding {
        self.dst
    }

    pub(crate) fn replacement(&self) -> Option<(&[u8], Encoding)> {
        self.replacement.as_ref().map(|(b, e)| (b.as_slice(), *e))
    }

    pub(crate) fn set_replacement(&mut self, bytes: Vec<u8>, enc: Encoding) {
        self.replacement = Some((bytes, enc));
    }

    pub(crate) fn flags(&self) -> ConverterFlag {
        self.flags
    }

    pub(crate) fn invalid_replace(&self) -> bool {
        self.flags.invalid_replace()
    }

    pub(crate) fn undef_replace(&self) -> bool {
        self.flags.undef_replace()
    }

    pub(crate) fn set_flags(&mut self, flags: ConverterFlag) {
        self.flags = flags;
    }

    pub(crate) fn finished(&self) -> bool {
        self.finished
    }

    pub(crate) fn set_finished(&mut self) {
        self.finished = true;
    }

    pub(crate) fn pending(&self) -> &[u8] {
        &self.pending
    }

    pub(crate) fn set_pending(&mut self, bytes: Vec<u8>) {
        self.pending = bytes;
    }

    pub(crate) fn pending_out(&self) -> &[u8] {
        &self.pending_out
    }

    pub(crate) fn set_pending_out(&mut self, bytes: Vec<u8>) {
        self.pending_out = bytes;
    }

    pub(crate) fn readagain(&self) -> &[u8] {
        &self.readagain
    }

    pub(crate) fn set_readagain(&mut self, bytes: Vec<u8>) {
        self.readagain = bytes;
    }

    /// The read-again bytes, taken out of the converter: they are the
    /// head of whatever the next call converts, unless `#putback`
    /// handed them back to the caller first.
    pub(crate) fn take_readagain(&mut self) -> Vec<u8> {
        std::mem::take(&mut self.readagain)
    }

    pub(crate) fn outcome(&self) -> Option<&ConverterOutcome> {
        self.outcome.as_ref()
    }

    pub(crate) fn set_outcome(&mut self, outcome: ConverterOutcome) {
        self.outcome = Some(outcome);
    }

    pub(crate) fn src_bom(&self) -> Option<Encoding> {
        self.src_bom
    }

    pub(crate) fn set_src_bom(&mut self, enc: Encoding) {
        self.src_bom = Some(enc);
    }

    pub(crate) fn dst_bom_written(&self) -> bool {
        self.dst_bom_written
    }

    pub(crate) fn set_dst_bom_written(&mut self) {
        self.dst_bom_written = true;
    }

    pub(crate) fn iso_state(&self) -> Option<u8> {
        self.iso_state
    }

    pub(crate) fn set_iso_state(&mut self, state: Option<u8>) {
        self.iso_state = state;
    }
}
