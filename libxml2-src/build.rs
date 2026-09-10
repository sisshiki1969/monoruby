//! Build the vendored libxml2 (2.13.8 + nokogiri's patches, see
//! `vendor/libxml2/NOKOGIRI-PATCHES`) with `cc`, without autotools or
//! cmake: `config.h` is the checked-in unix version in `config/`, and
//! `libxml/xmlversion.h` is generated here from the upstream template with
//! the feature set nokogiri builds (`--with-c14n --with-debug
//! --with-threads`, no legacy / zlib / lzma / http / ftp / python).

use std::env;
use std::fs;
use std::path::PathBuf;

const SOURCES: &[&str] = &[
    // core
    "buf",
    "chvalid",
    "dict",
    "entities",
    "encoding",
    "error",
    "globals",
    "hash",
    "list",
    "parser",
    "parserInternals",
    "SAX2",
    "threads",
    "tree",
    "uri",
    "valid",
    "xmlIO",
    "xmlmemory",
    "xmlstring",
    // optional modules, matching the WITH_* set below
    "c14n",
    "catalog",
    "debugXML",
    "HTMLparser",
    "HTMLtree",
    "xmlsave",
    "pattern",
    "xmlreader",
    "xmlregexp",
    "xmlunicode",
    "relaxng",
    "xmlschemas",
    "xmlschemastypes",
    "xpath",
    "schematron",
    "xmlwriter",
    "xinclude",
    "xlink",
    "xpointer",
];

/// `@WITH_X@` -> 0/1 for `xmlversion.h.in`.
const FEATURES: &[(&str, u8)] = &[
    ("THREADS", 1),
    ("THREAD_ALLOC", 0),
    ("TREE", 1),
    ("OUTPUT", 1),
    ("PUSH", 1),
    ("READER", 1),
    ("PATTERN", 1),
    ("WRITER", 1),
    ("SAX1", 1),
    ("FTP", 0),
    ("HTTP", 0),
    ("VALID", 1),
    ("HTML", 1),
    ("LEGACY", 0),
    ("C14N", 1),
    ("CATALOG", 1),
    ("XPATH", 1),
    ("XPTR", 1),
    ("XPTR_LOCS", 0),
    ("XINCLUDE", 1),
    ("ICONV", 1),
    ("ICU", 0),
    ("ISO8859X", 1),
    ("DEBUG", 1),
    ("REGEXPS", 1),
    ("SCHEMAS", 1),
    ("SCHEMATRON", 1),
    ("MODULES", 0),
    ("ZLIB", 0),
    ("LZMA", 0),
];

fn main() {
    let manifest = PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap());
    let vendor = manifest.join("vendor/libxml2");
    let out = PathBuf::from(env::var("OUT_DIR").unwrap());
    let gen_include = out.join("include");
    fs::create_dir_all(gen_include.join("libxml")).unwrap();

    // xmlversion.h from the template.
    let template = fs::read_to_string(vendor.join("include/libxml/xmlversion.h.in")).unwrap();
    let mut version_h = template
        .replace("@VERSION@", "2.13.8")
        .replace("@LIBXML_VERSION_NUMBER@", "21308")
        .replace("@LIBXML_VERSION_EXTRA@", "-nokogiri")
        .replace("@MODULE_EXTENSION@", ".so");
    for (name, on) in FEATURES {
        version_h = version_h.replace(&format!("@WITH_{name}@"), &on.to_string());
    }
    assert!(!version_h.contains('@'), "unsubstituted placeholder in xmlversion.h.in");
    fs::write(gen_include.join("libxml/xmlversion.h"), version_h).unwrap();
    fs::copy(manifest.join("config/config.h"), gen_include.join("config.h")).unwrap();

    let mut build = cc::Build::new();
    build
        .include(&gen_include)
        .include(vendor.join("include"))
        .include(&vendor)
        .opt_level(2)
        .warnings(false)
        .flag_if_supported("-Wno-unused-parameter")
        .flag_if_supported("-fvisibility=hidden")
        .define("_GNU_SOURCE", None);
    for src in SOURCES {
        build.file(vendor.join(format!("{src}.c")));
    }
    build.compile("xml2");

    let target_os = env::var("CARGO_CFG_TARGET_OS").unwrap_or_default();
    if target_os == "macos" {
        // iconv is a separate library on Darwin (libc has it on glibc).
        println!("cargo:rustc-link-lib=iconv");
    }
    if target_os == "linux" {
        println!("cargo:rustc-link-lib=m");
    }
    println!("cargo:rerun-if-changed=build.rs");
    println!("cargo:rerun-if-changed=config/config.h");
    println!("cargo:rerun-if-changed=vendor/libxml2");
}
