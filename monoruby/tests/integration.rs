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
mod alloc_limits_full_message_check_id;
mod arith;
mod arith_seq_enumerator;
mod arith_seq_float;
mod array_block_methods_ruby;
mod array_frozen_index_assign;
mod array_initialize;
mod array_loop_seed;
mod array_own_storage;
mod array_shared_slice;
mod attr_scope_visibility;
mod autoload;
mod backtrace;
mod backtrace_label_anonymous;
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
mod builtin_inline_hot;
mod builtin_operator_binding;
mod bundler_host_checkout;
mod byte_paths_line_limits_regexp_escapes;
mod capi;
mod capture_callee_no_abort;
mod capture_in_compiled_frame;
mod capture_ret;
mod capture_through_helper;
mod case;
mod case_mapping;
mod cesu8;
mod check_funcall_builtins;
mod class_chain;
mod class_dispatch;
mod class_new_inline;
mod cold_call_site;
mod cold_page_class_guard;
mod command_line;
mod comparable;
mod compare_operand_order;
mod complex_compare_case_eq;
mod complex_undefs;
mod const_across_block_call;
mod converter_consumes_bad_run;
mod converter_edges;
mod converter_error_hop;
mod converter_iso2022jp_cap;
mod converter_not_found;
mod converter_partial_input;
mod copy_propagation;
mod count_num2long;
mod count_num2long_more;
mod date;
mod date_parse;
mod deferred_def;
mod deferred_unwind;
mod define_method_arity;
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
mod env_grep_gvar_format_file_stat;
mod eq_funcall;
mod eq_search;
mod equal_identity;
mod errinfo_inline_load;
mod error_highlight;
mod escape_analysis;
mod etc;
mod exception_api;
mod exception_mesg_and_encodings;
mod exit_before_regions_value;
mod exit_handler_error_roots;
mod exit_landing_join;
mod exit_order;
mod extend_special_singletons;
mod far_branch_relaxation;
mod fcntl_lock;
mod fiber_scheduler_io;
mod fiddle_int64;
mod fiddle_virtual_library;
mod fixpoint_chain;
mod flip_flop;
mod float_across_block_call;
mod float_call_convention;
mod float_home_exits;
mod float_literal_carry;
mod float_mod_zero_dup_keys_chain_enum;
mod float_speculation;
mod forwarded_block_yield;
mod forwarding_depth;
mod forwarding_megamorphic;
mod forwarding_recompile;
mod fp_inline_const_operand;
mod frameless_call;
mod frozen_init_errno_id2ref_gc_info;
mod fused_branch_nested;
mod gem_boot_compat;
mod gem_prelude;
mod given_block_escape;
mod gp_float_reuse_repro;
mod gsub_binary_replacement;
mod gsub_wide_replacement;
mod hash_builder_string_keys;
mod hash_compare_by_identity;
mod hash_each_pair_yield;
mod hash_key_protocol;
mod hash_literal_gc;
mod hash_probe_jit;
mod hash_seed;
mod hash_string_keys;
mod hash_walk_mutation;
mod heap_limit_div_between_sort_order;
mod hidden_ivars;
mod hidden_state;
mod initgroups_argf_lineno_hash_recursion_kwsplat;
mod inspect_dispatch;
mod install_root;
mod integer_64bit;
mod internal_gvars_hidden;
mod interpolation_to_s;
mod invoked_name;
mod io_console;
mod io_read_conversion;
mod ivar_ty;
mod jit_method_deps;
mod jit_method_missing;
mod jit_send;
mod json;
mod kernel_warn;
mod key_search_protocol;
mod kwargs_forward;
mod kwargs_source_order;
mod language_fixes;
mod late_direct_call;
mod literal;
mod load_autoload_path_encodings;
mod loop_analysis_outer_widen;
mod loop_dead_float;
mod loop_entry_types;
mod loop_head_const_placement;
mod loop_hoist;
mod loop_jit_dead_loop_end;
mod loop_jit_exit_sp;
mod loop_pin;
mod main_script;
mod marshal_exception_fields;
mod method_call;
mod method_inspect_send_frames;
mod method_lists;
mod method_lookup_errors;
mod method_missing_conversion;
mod method_object_call;
mod mkmf;
mod module_instance_method;
mod monitor;
mod mul_assign;
mod name_argument_checks;
mod name_argument_leftovers;
mod name_error_api;
mod name_error_messages;
mod name_errors_singleton_fstring;
mod nameerr_lazy_class_uncaught_throw;
mod native_ext;
mod negative_count_messages;
mod nested_float_loop;
mod nil_eqq_string_new_set_alias;
mod nil_nilp_set_copy_unknown_names;
mod no_method_error_message;
mod nonlocal_exit_ensure;
mod nonlocal_exit_inline_ensure;
mod nonlocal_exit_intermediate_ensure;
mod nonlocal_exit_rescue;
mod nonlocal_return;
mod num2long_args;
mod num_coerce_cmpint_float_ties_set_marshal;
mod numbered_param;
mod numeric_step_keywords;
mod openssl_cipher;
mod openssl_digest;
mod opt_case_huge_unit;
mod outer_float_write_through;
mod pack_single_directive;
mod pic_shared_target;
mod predefined_gvar;
mod prism_bridge;
mod prism_parse_warnings;
mod private_call_fcall;
mod proc_compose;
mod protected_callee_float_home;
mod psych;
mod public_send_visibility;
mod r#yield;
mod ractor;
mod rails_app_gaps;
mod range_eqq_cover;
mod rational;
mod rbconfig;
mod receiver_eval_order;
mod recompile_budget;
mod redefine;
mod refinement_funcall;
mod refinement_inline;
mod regexp_ascii_engine;
mod regexp_binary_subject;
mod regexp_once;
mod relocatable_install;
mod replayed_ensure_raise;
mod replica_encoding_regexp;
mod report_stderr_regexp_messages;
mod require;
mod require_option;
mod rescue;
mod residual_arm_rollback;
mod retry_errinfo;
mod return_state;
mod rs_setter_module_bind_time_range_complex_parts_ary_growth;
mod ruby2_keywords;
mod runtime_malloc_trim;
mod scrub_block_and_replacement;
mod self_ivar_base;
mod set;
mod set_subclass_compat_exception_messages;
mod setivar_wb;
mod signal_handling;
mod signal_trap;
mod singleton_class_facts;
mod singleton_class_fstring;
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
mod step_non_numeric;
mod string_bytes;
mod string_concat;
mod string_eval_cref;
mod string_literal_inline;
mod string_search_fast_path;
mod stringio_encoding;
mod strscan;
mod super_called_name;
mod symbol_literal_block;
mod symbol_name_methods;
mod symbol_proc_cache;
mod symbol_proc_no_receiver;
mod syntax_report_did_you_mean_source_names;
mod syntax_suggest;
mod system_exit_status;
mod tally_single_probe;
mod time_interval;
mod time_timezone;
mod to_c_enum_size_integer_complex_pow_stdout_lazy;
mod to_hash_conversion;
mod tr_wide_replacement;
mod type_error_names;
mod uncaught_report_bytes_tty_locale;
mod unnamed_class_layout_instance_exec;
mod usascii_8bit_symbol;
mod var_site_cache;
mod variables;
mod void_warn_autoload_env_uninit_io;
mod warning_gating;
mod watchdog;
mod weakmap;
mod wide_lines_nil_conversions;
mod wide_string_scan;
mod wide_string_slice_tr;
mod yaml_scalars;
mod yield_block_kinds;
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
