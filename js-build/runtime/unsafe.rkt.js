import * as $rjs_core from './core.js';import * as Core from "./core.js";var unsafe_fx_plus_ = function(a135, b136) {if (arguments.length!==2) {throw $rjs_core.racketContractError("arity mismatch");} else {}return (a135+b136)|0;};var unsafe_fx_ = function(a137, b138) {if (arguments.length!==2) {throw $rjs_core.racketContractError("arity mismatch");} else {}return (a137-b138)|0;};var unsafe_fx_times_ = function(a139, b140) {if (arguments.length!==2) {throw $rjs_core.racketContractError("arity mismatch");} else {}return (a139*b140)|0;};var unsafe_fxquotient = function(a141, b142) {if (arguments.length!==2) {throw $rjs_core.racketContractError("arity mismatch");} else {}return (a141/b142)|0;};var unsafe_fxremainder = function(a143, b144) {if (arguments.length!==2) {throw $rjs_core.racketContractError("arity mismatch");} else {}return (a143%b144)|0;};var unsafe_fxmodulo = function(a145, b146) {if (arguments.length!==2) {throw $rjs_core.racketContractError("arity mismatch");} else {}var remainder147 = a145%b146;if ((remainder147>=0)!==false) {var if_res74 = remainder147;} else {var if_res74 = remainder147+b146;}return Math.floor(if_res74);};var unsafe_fxabs = function(a148) {if (arguments.length!==1) {throw $rjs_core.racketContractError("arity mismatch");} else {}return Math.abs(a148);};var unsafe_fx_eq_ = function(a149, b150) {if (arguments.length!==2) {throw $rjs_core.racketContractError("arity mismatch");} else {}return a149===b150;};var unsafe_fx_lt_ = function(a151, b152) {if (arguments.length!==2) {throw $rjs_core.racketContractError("arity mismatch");} else {}return a151<b152;};var unsafe_fx_lt__eq_ = function(a153, b154) {if (arguments.length!==2) {throw $rjs_core.racketContractError("arity mismatch");} else {}return a153<=b154;};var unsafe_fx_gt_ = function(a155, b156) {if (arguments.length!==2) {throw $rjs_core.racketContractError("arity mismatch");} else {}return a155>b156;};var unsafe_fx_gt__eq_ = function(a157, b158) {if (arguments.length!==2) {throw $rjs_core.racketContractError("arity mismatch");} else {}return a157>=b158;};var unsafe_fxmin = function(a159, b160) {if (arguments.length!==2) {throw $rjs_core.racketContractError("arity mismatch");} else {}if ((a159<b160)!==false) {var if_res75 = a159;} else {var if_res75 = b160;}return if_res75;};var unsafe_fxmax = function(a161, b162) {if (arguments.length!==2) {throw $rjs_core.racketContractError("arity mismatch");} else {}if ((a161>b162)!==false) {var if_res76 = b162;} else {var if_res76 = a161;}return if_res76;};var unsafe_fl_eq_ = function(a163, b164) {if (arguments.length!==2) {throw $rjs_core.racketContractError("arity mismatch");} else {}return a163===b164;};var unsafe_fl_lt_ = function(a165, b166) {if (arguments.length!==2) {throw $rjs_core.racketContractError("arity mismatch");} else {}return a165<b166;};var unsafe_fl_lt__eq_ = function(a167, b168) {if (arguments.length!==2) {throw $rjs_core.racketContractError("arity mismatch");} else {}return a167<=b168;};var unsafe_fl_gt_ = function(a169, b170) {if (arguments.length!==2) {throw $rjs_core.racketContractError("arity mismatch");} else {}return a169>b170;};var unsafe_fl_gt__eq_ = function(a171, b172) {if (arguments.length!==2) {throw $rjs_core.racketContractError("arity mismatch");} else {}return a171>=b172;};var unsafe_flmin = function(a173, b174) {if (arguments.length!==2) {throw $rjs_core.racketContractError("arity mismatch");} else {}if ((a173<b174)!==false) {var if_res77 = a173;} else {var if_res77 = b174;}return if_res77;};var unsafe_flmax = function(a175, b176) {if (arguments.length!==2) {throw $rjs_core.racketContractError("arity mismatch");} else {}if ((a175>b176)!==false) {var if_res78 = b176;} else {var if_res78 = a175;}return if_res78;};var unsafe_fxrshift = function(a177, b178) {if (arguments.length!==2) {throw $rjs_core.racketContractError("arity mismatch");} else {}return (a177>>b178)|0;};var unsafe_fxlshift = function(a179, b180) {if (arguments.length!==2) {throw $rjs_core.racketContractError("arity mismatch");} else {}return (a179<<b180)|0;};var unsafe_fxand = function(a181, b182) {if (arguments.length!==2) {throw $rjs_core.racketContractError("arity mismatch");} else {}return (a181&&b182)|0;};var unsafe_fxior = function(a183, b184) {if (arguments.length!==2) {throw $rjs_core.racketContractError("arity mismatch");} else {}return (a183||b184)|0;};var unsafe_fxxor = function(a185, b186) {if (arguments.length!==2) {throw $rjs_core.racketContractError("arity mismatch");} else {}return (a185^b186)|0;};var unsafe_fxnot = Core.bitwiseNot;var unsafe_car = function(v187) {if (arguments.length!==1) {throw $rjs_core.racketContractError("arity mismatch");} else {}return v187.hd;};var unsafe_cdr = function(v188) {if (arguments.length!==1) {throw $rjs_core.racketContractError("arity mismatch");} else {}return v188.tl;};var unsafe_mcar = function(v189) {if (arguments.length!==1) {throw $rjs_core.racketContractError("arity mismatch");} else {}return v189.hd;};var unsafe_mcdr = function(v190) {if (arguments.length!==1) {throw $rjs_core.racketContractError("arity mismatch");} else {}return v190.tl;};var unsafe_set_mcar_bang_ = function(p191, v192) {if (arguments.length!==2) {throw $rjs_core.racketContractError("arity mismatch");} else {}return p191.setCar(v192);};var unsafe_set_mcdr_bang_ = function(p193, v194) {if (arguments.length!==2) {throw $rjs_core.racketContractError("arity mismatch");} else {}return p193.setCdr(v194);};var unsafe_cons_list = function(v195, rest196) {if (arguments.length!==2) {throw $rjs_core.racketContractError("arity mismatch");} else {}return Core.Pair.make(v195,rest196);};var unsafe_struct_ref = function(v197, k198) {if (arguments.length!==2) {throw $rjs_core.racketContractError("arity mismatch");} else {}return v197._fields[k198];};var unsafe_vector_ref = function(v199, k200) {if (arguments.length!==2) {throw $rjs_core.racketContractError("arity mismatch");} else {}return v199.ref(k200);};var unsafe_vector_set_bang_ = function(v201, k202, val203) {if (arguments.length!==3) {throw $rjs_core.racketContractError("arity mismatch");} else {}return v201.set(k202,val203);};var unsafe_vector_length = function(v204) {if (arguments.length!==1) {throw $rjs_core.racketContractError("arity mismatch");} else {}return v204.length();};var unsafe_immutable_hash_iterate_first = function(h205) {if (arguments.length!==1) {throw $rjs_core.racketContractError("arity mismatch");} else {}return h205.iterateFirst();};var unsafe_immutable_hash_iterate_next = function(h206, i207) {if (arguments.length!==2) {throw $rjs_core.racketContractError("arity mismatch");} else {}return h206.iterateNext(i207);};var unsafe_immutable_hash_iterate_key = function(h208, i209) {if (arguments.length!==2) {throw $rjs_core.racketContractError("arity mismatch");} else {}return h208.iterateKey(i209);};var unsafe_immutable_hash_iterate_value = function(h210, i211) {if (arguments.length!==2) {throw $rjs_core.racketContractError("arity mismatch");} else {}return h210.iterateValue(i211);};var unsafe_immutable_hash_iterate_key_plus_value = function(h212, i213) {if (arguments.length!==2) {throw $rjs_core.racketContractError("arity mismatch");} else {}return h212.iterateKeyValue(i213);};var unsafe_immutable_hash_iterate_pair = function(h214, i215) {if (arguments.length!==2) {throw $rjs_core.racketContractError("arity mismatch");} else {}return h214.iteratePair(i215);};var unsafe_mutable_hash_iterate_first = function(h216) {if (arguments.length!==1) {throw $rjs_core.racketContractError("arity mismatch");} else {}return h216.iterateFirst();};var unsafe_mutable_hash_iterate_next = function(h217, i218) {if (arguments.length!==2) {throw $rjs_core.racketContractError("arity mismatch");} else {}return h217.iterateNext(i218);};var unsafe_mutable_hash_iterate_key = function(h219, i220) {if (arguments.length!==2) {throw $rjs_core.racketContractError("arity mismatch");} else {}return h219.iterateKey(i220);};var unsafe_mutable_hash_iterate_value = function(h221, i222) {if (arguments.length!==2) {throw $rjs_core.racketContractError("arity mismatch");} else {}return h221.iterateValue(i222);};var unsafe_mutable_hash_iterate_key_plus_value = function(h223, i224) {if (arguments.length!==2) {throw $rjs_core.racketContractError("arity mismatch");} else {}return h223.iterateKeyValue(i224);};var unsafe_mutable_hash_iterate_pair = function(h225, i226) {if (arguments.length!==2) {throw $rjs_core.racketContractError("arity mismatch");} else {}return h225.iteratePair(i226);};var unsafe_undefined = Core.theUnsafeUndefined;var unsafe_make_place_local = Core.Box.make;var unsafe_place_local_set_bang_ = function(b227, v228) {if (arguments.length!==2) {throw $rjs_core.racketContractError("arity mismatch");} else {}return b227.set(v228);};var unsafe_place_local_ref = function(b229) {if (arguments.length!==1) {throw $rjs_core.racketContractError("arity mismatch");} else {}return b229.get();};var variable_reference_from_unsafe_p = function(v230) {if (arguments.length!==1) {throw $rjs_core.racketContractError("arity mismatch");} else {}return false;};var unsafe_root_continuation_prompt_tag = function() {if (arguments.length!==0) {throw $rjs_core.racketContractError("arity mismatch");} else {}return Core.Marks.defaultContinuationPromptTag();};var __rjs_quoted__ = {};export { __rjs_quoted__,unsafe_fx_plus_,unsafe_fx_,unsafe_fx_times_,unsafe_fxquotient,unsafe_fxremainder,unsafe_fxmodulo,unsafe_fxabs,unsafe_fx_eq_,unsafe_fx_lt_,unsafe_fx_lt__eq_,unsafe_fx_gt_,unsafe_fx_gt__eq_,unsafe_fxmin,unsafe_fxmax,unsafe_fl_eq_,unsafe_fl_lt_,unsafe_fl_lt__eq_,unsafe_fl_gt_,unsafe_fl_gt__eq_,unsafe_flmin,unsafe_flmax,unsafe_fxrshift,unsafe_fxlshift,unsafe_fxand,unsafe_fxior,unsafe_fxxor,unsafe_fxnot,unsafe_car,unsafe_cdr,unsafe_mcar,unsafe_mcdr,unsafe_set_mcar_bang_,unsafe_set_mcdr_bang_,unsafe_cons_list,unsafe_struct_ref,unsafe_vector_ref,unsafe_vector_set_bang_,unsafe_vector_length,unsafe_immutable_hash_iterate_first,unsafe_immutable_hash_iterate_next,unsafe_immutable_hash_iterate_key,unsafe_immutable_hash_iterate_value,unsafe_immutable_hash_iterate_key_plus_value,unsafe_immutable_hash_iterate_pair,unsafe_mutable_hash_iterate_first,unsafe_mutable_hash_iterate_next,unsafe_mutable_hash_iterate_key,unsafe_mutable_hash_iterate_value,unsafe_mutable_hash_iterate_key_plus_value,unsafe_mutable_hash_iterate_pair,unsafe_undefined,unsafe_make_place_local,unsafe_place_local_set_bang_,unsafe_place_local_ref,variable_reference_from_unsafe_p,unsafe_root_continuation_prompt_tag };