//! The integration tests, as one test binary.
//!
//! Every `tests/*.rs` file used to be its own crate, and so its own
//! executable linking the whole interpreter: ~190 of them at ~125 MB each
//! under the coverage instrumentation, ~24 GB of the CI runner's disk and
//! a link step apiece. Cargo's automatic test discovery is switched off
//! (`autotests = false` in `Cargo.toml`) and each file is a module of this
//! crate instead, so the interpreter is linked once. The files stay where
//! they were, so `tests/<name>.rs` keeps meaning what it meant; run one of
//! them with `cargo test --test integration <name>::`.
//!
//! A new `tests/*.rs` file must be added to the list below.
//! `every_test_file_is_a_module` fails if one is missing.

mod add_coverage;
mod arith;
mod array_frozen_index_assign;
mod array_initialize;
mod array_loop_seed;
mod array_shared_slice;
mod autoload;
mod backtrace;
mod bang_operator;
mod begin_end;
mod bigdecimal;
mod bigint_const_ops;
mod block_forward_proxy;
mod block_param_assign;
mod block_root_recv_miss_heal;
mod block_surrender_narrowing;
mod block_to_ary;
mod bmethod;
mod bool_op_jit;
mod break_proc;
mod builtin_operator_binding;
mod bundler_host_checkout;
mod capi;
mod capture_callee_no_abort;
mod capture_ret;
mod case;
mod class_chain;
mod class_new_inline;
mod cold_call_site;
mod command_line;
mod comparable;
mod const_across_block_call;
mod converter_consumes_bad_run;
mod converter_edges;
mod converter_error_hop;
mod converter_iso2022jp_cap;
mod converter_not_found;
mod converter_partial_input;
mod copy_propagation;
mod date;
mod date_parse;
mod deferred_def;
mod deferred_unwind;
mod dup_uncopyable;
mod encode_big5_family;
mod encode_cjk_sweep;
mod encode_cp5022x;
mod encode_first_failure;
mod encode_kddi_ibm037;
mod encode_xml_invalid;
mod encoding_locale;
mod ensure_end_deferral_gate;
mod enumerable;
mod eq_search;
mod equal_identity;
mod errinfo_inline_load;
mod escape_analysis;
mod exception_api;
mod exit_before_regions_value;
mod exit_handler_error_roots;
mod exit_order;
mod far_branch_relaxation;
mod fcntl_lock;
mod fiber_scheduler_io;
mod fiddle_int64;
mod fiddle_virtual_library;
mod flip_flop;
mod float_across_block_call;
mod float_call_convention;
mod float_speculation;
mod forwarded_block_yield;
mod forwarding_depth;
mod forwarding_recompile;
mod frameless_call;
mod fused_branch_nested;
mod gem_boot_compat;
mod gem_prelude;
mod given_block_escape;
mod gp_float_reuse_repro;
mod gsub_binary_replacement;
mod gsub_wide_replacement;
mod hash_compare_by_identity;
mod hash_each_pair_yield;
mod hash_probe_jit;
mod hash_seed;
mod hash_string_keys;
mod install_root;
mod integer_64bit;
mod invoked_name;
mod io_console;
mod io_read_conversion;
mod ivar_ty;
mod jit_method_deps;
mod jit_method_missing;
mod jit_send;
mod json;
mod kernel_warn;
mod kwargs_forward;
mod kwargs_source_order;
mod language_fixes;
mod literal;
mod loop_analysis_outer_widen;
mod loop_entry_types;
mod loop_head_const_placement;
mod loop_hoist;
mod loop_jit_dead_loop_end;
mod loop_jit_exit_sp;
mod loop_pin;
mod main_script;
mod method_call;
mod method_lists;
mod method_lookup_errors;
mod method_object_call;
mod mkmf;
mod mul_assign;
mod name_error_api;
mod native_ext;
mod nested_float_loop;
mod no_method_error_message;
mod nonlocal_exit_ensure;
mod nonlocal_exit_inline_ensure;
mod nonlocal_exit_intermediate_ensure;
mod nonlocal_exit_rescue;
mod nonlocal_return;
mod numbered_param;
mod openssl_cipher;
mod openssl_digest;
mod opt_case_huge_unit;
mod outer_float_write_through;
mod pack_single_directive;
mod pic_shared_target;
mod predefined_gvar;
mod prism_bridge;
mod proc_compose;
mod psych;
mod public_send_visibility;
mod ractor;
mod rails_app_gaps;
mod rational;
mod rbconfig;
mod receiver_eval_order;
mod recompile_budget;
mod redefine;
mod regexp_ascii_engine;
mod regexp_binary_subject;
mod regexp_once;
mod relocatable_install;
mod replayed_ensure_raise;
mod require;
mod require_option;
mod rescue;
mod residual_arm_rollback;
mod retry_errinfo;
mod return_state;
mod ruby2_keywords;
mod runtime_malloc_trim;
mod scrub_block_and_replacement;
mod self_ivar_base;
mod set;
mod setivar_wb;
mod signal_handling;
mod signal_trap;
mod singleton_class_facts;
mod socket;
mod sort_fast_path;
mod spawn_fds;
mod spec_conformance;
mod spliced_exit_chain;
mod spliced_exit_claim;
mod spliced_exit_delivery;
mod sqlite3;
mod std_fds;
mod std_stream_seek;
mod stdout_buffering;
mod stdout_epipe;
mod string_bytes;
mod string_concat;
mod string_literal_inline;
mod string_search_fast_path;
mod stringio_encoding;
mod strscan;
mod super_called_name;
mod symbol_proc_cache;
mod symbol_proc_no_receiver;
mod system_exit_status;
mod time_timezone;
mod tr_wide_replacement;
mod type_error_names;
mod variables;
mod warning_gating;
mod watchdog;
mod weakmap;
mod yaml_scalars;
mod r#yield;
mod zlib_deflate;
mod zlib_gzip;

/// Every `tests/*.rs` file is declared above: with automatic test
/// discovery off, a file that is not would silently never be built.
#[test]
fn every_test_file_is_a_module() {
    let source = include_str!("integration.rs");
    let dir = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("tests");
    let mut missing = vec![];
    for entry in std::fs::read_dir(&dir).unwrap() {
        let path = entry.unwrap().path();
        if path.extension().is_none_or(|e| e != "rs") {
            continue;
        }
        let stem = path.file_stem().unwrap().to_str().unwrap();
        if stem == "integration" {
            continue;
        }
        let declared = ["", "r#"]
            .iter()
            .any(|raw| source.contains(&format!("mod {raw}{stem};")));
        if !declared {
            missing.push(stem.to_string());
        }
    }
    missing.sort();
    assert!(
        missing.is_empty(),
        "not declared in tests/integration.rs: {missing:?}"
    );
}
