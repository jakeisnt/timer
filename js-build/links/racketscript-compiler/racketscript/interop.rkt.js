import * as $rjs_core from '../../../runtime/core.js';import * as M0 from "./private/interop.rkt.js";import * as M1 from "../../../collects/racket/private/reverse.rkt.js";import * as M2 from "../../../runtime/kernel.rkt.js";var __eq__gt_$ = function(lam_expr899) {if (arguments.length!==1) {throw $rjs_core.racketContractError("arity mismatch");} else {}return $rjs_core.Marks.wrapWithContext(lam_expr899);};var js_string = function(e900) {if (arguments.length!==1) {throw $rjs_core.racketContractError("arity mismatch");} else {}return e900.toString();};var js_string__gt_string = function(e901) {if (arguments.length!==1) {throw $rjs_core.racketContractError("arity mismatch");} else {}return $rjs_core.UString.makeImmutable(e901);};var js_array__gt_list = function(e902) {if (arguments.length!==1) {throw $rjs_core.racketContractError("arity mismatch");} else {}return $rjs_core.Pair.listFromArray(e902);};var assoc__gt_object = function(pairs903) {if (arguments.length!==1) {throw $rjs_core.racketContractError("arity mismatch");} else {}var result904 = {};var loop905 = function(pairs906) {if (arguments.length!==1) {throw $rjs_core.racketContractError("arity mismatch");} else {}if (M2.null_p(pairs906)!==false) {var if_res441 = result904;} else {var p907 = M2.car(pairs906);var k909 = M2.car(p907);var or_part910 = typeof(k909)==="string";if (or_part910!==false) {var if_res438 = or_part910;} else {var if_res438 = M2.string_p(k909);}if (if_res438!==false) {var if_res440 = k909;} else {if (M2.symbol_p(k909)!==false) {var if_res439 = M2.symbol__gt_string(k909);} else {var if_res439 = M2.error($rjs_core.PrimitiveSymbol.make("assoc->object"),$rjs_core.UString.make("invalid key value"));}var if_res440 = if_res439;}var key908 = if_res440;result904[key908] = M2.car(M2.cdr(p907));var if_res441 = loop905(M2.cdr(pairs906));}return if_res441;};return loop905(pairs903);};var js_array_p = function(v911) {if (arguments.length!==1) {throw $rjs_core.racketContractError("arity mismatch");} else {}return Array.isArray(v911);};var in_js_array = function(arr912) {if (arguments.length!==1) {throw $rjs_core.racketContractError("arity mismatch");} else {}check_array(arr912);var arr913 = arr912;if (js_array_p(arr913)!==false) {var if_res442 = M2.rvoid();} else {var if_res442 = in_js_array(arr913);}if_res442;var for_loop914 = function(fold_var915, i916) {if (arguments.length!==2) {throw $rjs_core.racketContractError("arity mismatch");} else {}if (M2.__lt_(i916,arr913.length)!==false) {var v917 = arr913[i916];if (true!==false) {var fold_var919 = fold_var915;var fold_var920 = M2.cons(v917,fold_var919);var fold_var918 = M2.values(fold_var920);if (true!==false) {var if_res443 = M2.not(false);} else {var if_res443 = false;}if (if_res443!==false) {var if_res444 = for_loop914(fold_var918,i916+1);} else {var if_res444 = fold_var918;}var if_res445 = if_res444;} else {var if_res445 = fold_var915;}var if_res446 = if_res445;} else {var if_res446 = fold_var915;}return if_res446;};return M1.alt_reverse(for_loop914(M2.rnull,0));};var check_array = function(v921) {if (arguments.length!==1) {throw $rjs_core.racketContractError("arity mismatch");} else {}if (js_array_p(v921)!==false) {var if_res447 = M2.rvoid();} else {var if_res447 = M2.raise_argument_error($rjs_core.PrimitiveSymbol.make("in-js-array"),$rjs_core.UString.make("js-array?"),v921);}return if_res447;};var in_js_obect = function(obj922) {if (arguments.length!==1) {throw $rjs_core.racketContractError("arity mismatch");} else {}check_object(obj922);var obj923 = obj922;var keys924 = Object.keys(obj922);if (js_object_p(obj923)!==false) {var if_res448 = M2.rvoid();} else {var if_res448 = in_js_array(obj923);}if_res448;var for_loop925 = function(fold_var926, i927) {if (arguments.length!==2) {throw $rjs_core.racketContractError("arity mismatch");} else {}if (M2.__lt_(i927,keys924.length)!==false) {var k928 = keys924[i927];var v929 = obj923[keys924[i927]];if (true!==false) {var fold_var931 = fold_var926;var fold_var932 = M2.cons(M2.values(k928,v929),fold_var931);var fold_var930 = M2.values(fold_var932);if (true!==false) {var if_res449 = M2.not(false);} else {var if_res449 = false;}if (if_res449!==false) {var if_res450 = for_loop925(fold_var930,i927+1);} else {var if_res450 = fold_var930;}var if_res451 = if_res450;} else {var if_res451 = fold_var926;}var if_res452 = if_res451;} else {var if_res452 = fold_var926;}return if_res452;};return M1.alt_reverse(for_loop925(M2.rnull,0));};var js_object_p = function(v933) {if (arguments.length!==1) {throw $rjs_core.racketContractError("arity mismatch");} else {}return ((typeof(v933)==="object")&&(v933!==null))&&M2.not($rjs_core.Primitive.check(v933));};var check_object = function(v934) {if (arguments.length!==1) {throw $rjs_core.racketContractError("arity mismatch");} else {}if (js_object_p(v934)!==false) {var if_res453 = M2.rvoid();} else {var if_res453 = M2.raise_argument_error($rjs_core.PrimitiveSymbol.make("in-js-object"),$rjs_core.UString.make("js-object?"),v934);}return if_res453;};var __rjs_quoted__ = {};__rjs_quoted__.js_object_p = js_object_p;__rjs_quoted__.js_array_p = js_array_p;__rjs_quoted__.in_js_array = in_js_array;__rjs_quoted__.js_string = js_string;export { __rjs_quoted__,js_object_p,js_array_p,assoc__gt_object,js_array__gt_list,js_string__gt_string,js_string,__eq__gt_$ };