//! Build the vendored libxml2 (2.13.8 + nokogiri's patches, see
//! `vendor/libxml2/NOKOGIRI-PATCHES`), nokogiri's gumbo-parser (the HTML5
//! parser, `vendor/gumbo-parser`) and libxslt / libexslt (1.1.43,
//! `vendor/libxslt`) with `cc`, without autotools or cmake: the `config.h`s
//! are the checked-in unix versions in `config/`, and `libxml/xmlversion.h`
//! / `libxslt/xsltconfig.h` / `libexslt/exsltconfig.h` are generated here
//! from the upstream templates with the feature set nokogiri builds
//! (`--with-c14n --with-debug --with-threads`, no legacy / zlib / lzma /
//! http / ftp / python; libxslt without crypto and plugins).

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
    // monoruby's helpers (parser-context accessors, the variadic SAX
    // message callbacks), built into the same archive.
    build.file(manifest.join("glue/monoruby_glue.c"));
    build.compile("xml2");

    // gumbo-parser (nokogiri's fork of libgumbo, the HTML5 parser),
    // built as nokogiri does (`-std=c99`, -O2), with the glue that walks
    // its tree into a libxml2 document.
    let gumbo = manifest.join("vendor/gumbo-parser/src");
    let mut build = cc::Build::new();
    build
        .include(&gumbo)
        .include(&gen_include)
        .include(vendor.join("include"))
        .std("c99")
        .opt_level(2)
        .warnings(false)
        .flag_if_supported("-Wno-unused-parameter")
        .flag_if_supported("-fvisibility=hidden");
    for entry in fs::read_dir(&gumbo).unwrap() {
        let path = entry.unwrap().path();
        if path.extension().is_some_and(|e| e == "c") {
            build.file(path);
        }
    }
    build.file(manifest.join("glue/monoruby_gumbo.c"));
    build.compile("gumbo");

    // libxslt + libexslt (1.1.43, unmodified: nokogiri's only libxslt patch
    // touches config.guess / config.sub). Their `xsltconfig.h` /
    // `exsltconfig.h` come from the upstream templates, their `config.h`
    // is `config/xslt-config.h`; all three live in a separate include root
    // so the two libraries' `config.h`s never meet.
    let xslt = manifest.join("vendor/libxslt");
    let xslt_include = out.join("xslt-include");
    fs::create_dir_all(xslt_include.join("libxslt")).unwrap();
    fs::create_dir_all(xslt_include.join("libexslt")).unwrap();
    let template = fs::read_to_string(xslt.join("libxslt/xsltconfig.h.in")).unwrap();
    let mut xsltconfig_h = template
        .replace("@VERSION@", "1.1.43")
        .replace("@LIBXSLT_VERSION_NUMBER@", "10143")
        .replace("@LIBXSLT_VERSION_EXTRA@", "")
        .replace("@LIBXSLT_DEFAULT_PLUGINS_PATH@", "");
    for (name, on) in [
        ("XSLT_DEBUG", 1),
        ("TRIO", 0),
        ("DEBUGGER", 1),
        ("PROFILER", 1),
        ("MODULES", 0),
    ] {
        xsltconfig_h = xsltconfig_h.replace(&format!("@WITH_{name}@"), &on.to_string());
    }
    assert!(
        !xsltconfig_h.contains("@WITH_"),
        "unsubstituted placeholder in xsltconfig.h.in"
    );
    fs::write(xslt_include.join("libxslt/xsltconfig.h"), xsltconfig_h).unwrap();
    let template = fs::read_to_string(xslt.join("libexslt/exsltconfig.h.in")).unwrap();
    let exsltconfig_h = template
        .replace("@LIBEXSLT_VERSION_NUMBER@", "824")
        .replace("@LIBEXSLT_VERSION_EXTRA@", "")
        .replace("@LIBEXSLT_VERSION@", "0.8.24")
        .replace("@WITH_CRYPTO@", "0");
    assert!(
        !exsltconfig_h.contains("@WITH_"),
        "unsubstituted placeholder in exsltconfig.h.in"
    );
    fs::write(xslt_include.join("libexslt/exsltconfig.h"), exsltconfig_h).unwrap();
    fs::copy(
        manifest.join("config/xslt-config.h"),
        xslt_include.join("config.h"),
    )
    .unwrap();

    let mut build = cc::Build::new();
    build
        .include(&xslt_include)
        .include(&xslt)
        .include(&gen_include)
        .include(vendor.join("include"))
        .opt_level(2)
        .warnings(false)
        .flag_if_supported("-Wno-unused-parameter")
        .flag_if_supported("-fvisibility=hidden")
        .define("_GNU_SOURCE", None);
    for lib in ["libxslt", "libexslt"] {
        for entry in fs::read_dir(xslt.join(lib)).unwrap() {
            let path = entry.unwrap().path();
            if path.extension().is_some_and(|e| e == "c") {
                build.file(path);
            }
        }
    }
    // The generic-error capture and the `_private` accessors.
    build.file(manifest.join("glue/monoruby_xslt.c"));
    build.compile("xslt");

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
    println!("cargo:rerun-if-changed=glue/monoruby_glue.c");
    println!("cargo:rerun-if-changed=glue/monoruby_gumbo.c");
    println!("cargo:rerun-if-changed=config/xslt-config.h");
    println!("cargo:rerun-if-changed=glue/monoruby_xslt.c");
    println!("cargo:rerun-if-changed=vendor/gumbo-parser");
    println!("cargo:rerun-if-changed=vendor/libxml2");
    println!("cargo:rerun-if-changed=vendor/libxslt");
}
