import * as $rjs_core from './core.js';import * as M0 from "../links/racketscript-compiler/racketscript/private/interop.rkt.js";import * as M1 from "./lib.rkt.js";var __syntax_p = function(v909) {if (arguments.length!==1) {throw $rjs_core.racketContractError("arity mismatch");} else {}return M1.Core.Correlated.syntaxP(v909);};var __datum__gt_syntax = function(v910) {if (arguments.length!==1) {throw $rjs_core.racketContractError("arity mismatch");} else {}return M1.Core.Correlated.datumToSyntax(v910);};var __syntax_e = function(v911) {if (arguments.length!==1) {throw $rjs_core.racketContractError("arity mismatch");} else {}return v911.get();};var __syntax__gt_datum = function(v912) {if (arguments.length!==1) {throw $rjs_core.racketContractError("arity mismatch");} else {}return v912.get();};var __syntax_source = function(v913) {if (arguments.length!==1) {throw $rjs_core.racketContractError("arity mismatch");} else {}return false;};var __syntax_line = function(v914) {if (arguments.length!==1) {throw $rjs_core.racketContractError("arity mismatch");} else {}return false;};var __syntax_column = function(v915) {if (arguments.length!==1) {throw $rjs_core.racketContractError("arity mismatch");} else {}return false;};var __syntax_position = function(v916) {if (arguments.length!==1) {throw $rjs_core.racketContractError("arity mismatch");} else {}return false;};var __syntax_span = function(v917) {if (arguments.length!==1) {throw $rjs_core.racketContractError("arity mismatch");} else {}return false;};var __syntax_property918 = function(s2919, k3920, val1921) {if (arguments.length!==3) {throw $rjs_core.racketContractError("arity mismatch");} else {}var s922 = s2919;var k923 = k3920;if (false!==false) {var if_res429 = false;} else {var if_res429 = val1921;}var val924 = if_res429;if (val924!==false) {var if_res430 = s922;} else {var if_res430 = false;}return if_res430;};var cl431 = function(s925, k926) {if (arguments.length!==2) {throw $rjs_core.racketContractError("arity mismatch");} else {}return __syntax_property918(s925,k926,false);};var cl432 = function(s927, k928, val1929) {if (arguments.length!==3) {throw $rjs_core.racketContractError("arity mismatch");} else {}return __syntax_property918(s927,k928,val1929);};var __syntax_property = $rjs_core.attachProcedureArity(function() {var fixed_lam433 = {'2':cl431,'3':cl432}[arguments.length];if (fixed_lam433!==undefined) {return fixed_lam433.apply(null,arguments);} else {return ___kernel.error($rjs_core.UString.make("case-lambda: invalid case"));}},[2,3]);var __syntax_property_symbol_keys = function(v930) {if (arguments.length!==1) {throw $rjs_core.racketContractError("arity mismatch");} else {}return M1.Core.Pair.EMPTY;};var __rjs_quoted__ = {};export { __rjs_quoted__,__syntax_property_symbol_keys as syntax_property_symbol_keys,__syntax_property as syntax_property,__syntax_span as syntax_span,__syntax_position as syntax_position,__syntax_column as syntax_column,__syntax_line as syntax_line,__syntax_source as syntax_source,__syntax_e as syntax_e,__syntax__gt_datum as syntax__gt_datum,__datum__gt_syntax as datum__gt_syntax,__syntax_p as syntax_p };