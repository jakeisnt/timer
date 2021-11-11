import * as $rjs_core from './core.js';import * as M1 from "./lib.rkt.js";var fl_times_ = M1.Core.attachProcedureArity(M1.Core.Number.mul,0);var fl_by_ = M1.Core.attachProcedureArity(M1.Core.Number.div,1);var fl_plus_ = M1.Core.attachProcedureArity(M1.Core.Number.add,0);var fl_ = M1.Core.attachProcedureArity(M1.Core.Number.sub,1);var fl_lt_ = M1.Core.attachProcedureArity(M1.Core.Number.lt,1);var fl_gt_ = M1.Core.attachProcedureArity(M1.Core.Number.gt,1);var fl_lt__eq_ = M1.Core.attachProcedureArity(M1.Core.Number.lte,1);var fl_gt__eq_ = M1.Core.attachProcedureArity(M1.Core.Number.gte,1);var fl_eq_ = M1.Core.attachProcedureArity(M1.Core.Number.equals,1);var flabs = Math.abs;var flmin = Math.min;var flmax = Math.max;var flround = Math.round;var flfloor = Math.floor;var flceiling = Math.ceil;var fltruncate = Math.trunc;var flsin = Math.sin;var flcos = Math.cos;var fltan = Math.tan;var flasin = Math.asin;var flacos = Math.acos;var flatan = Math.atan;var fllog = Math.log;var flexp = Math.exp;var flsqrt = Math.sqrt;var flexpt = Math.pow;var fx_plus_ = function(a834, b835) {if (arguments.length!==2) {throw $rjs_core.racketContractError("arity mismatch");} else {}return (a834+b835)|0;};var fx_ = function(a836, b837) {if (arguments.length!==2) {throw $rjs_core.racketContractError("arity mismatch");} else {}return (a836-b837)|0;};var fx_times_ = function(a838, b839) {if (arguments.length!==2) {throw $rjs_core.racketContractError("arity mismatch");} else {}return (a838*b839)|0;};var fxquotient = function(a840, b841) {if (arguments.length!==2) {throw $rjs_core.racketContractError("arity mismatch");} else {}return (a840/b841)|0;};var fxremainder = function(a842, b843) {if (arguments.length!==2) {throw $rjs_core.racketContractError("arity mismatch");} else {}return (a842%b843)|0;};var fxmodulo = function(a844, b845) {if (arguments.length!==2) {throw $rjs_core.racketContractError("arity mismatch");} else {}var remainder846 = a844%b845;if ((remainder846>=0)!==false) {var if_res415 = remainder846;} else {var if_res415 = remainder846+b845;}return Math.floor(if_res415);};var fxabs = function(a847) {if (arguments.length!==1) {throw $rjs_core.racketContractError("arity mismatch");} else {}return Math.abs(a847);};var fx_eq_ = function(a848, b849) {if (arguments.length!==2) {throw $rjs_core.racketContractError("arity mismatch");} else {}return a848===b849;};var fx_lt_ = function(a850, b851) {if (arguments.length!==2) {throw $rjs_core.racketContractError("arity mismatch");} else {}return a850<b851;};var fx_lt__eq_ = function(a852, b853) {if (arguments.length!==2) {throw $rjs_core.racketContractError("arity mismatch");} else {}return a852<=b853;};var fx_gt_ = function(a854, b855) {if (arguments.length!==2) {throw $rjs_core.racketContractError("arity mismatch");} else {}return a854>b855;};var fx_gt__eq_ = function(a856, b857) {if (arguments.length!==2) {throw $rjs_core.racketContractError("arity mismatch");} else {}return a856>=b857;};var fxmin = function(a858, b859) {if (arguments.length!==2) {throw $rjs_core.racketContractError("arity mismatch");} else {}if ((a858<b859)!==false) {var if_res416 = a858;} else {var if_res416 = b859;}return if_res416;};var fxmax = function(a860, b861) {if (arguments.length!==2) {throw $rjs_core.racketContractError("arity mismatch");} else {}if ((a860>b861)!==false) {var if_res417 = b861;} else {var if_res417 = a860;}return if_res417;};var fxrshift = function(a862, b863) {if (arguments.length!==2) {throw $rjs_core.racketContractError("arity mismatch");} else {}return (a862>>b863)|0;};var fxlshift = function(a864, b865) {if (arguments.length!==2) {throw $rjs_core.racketContractError("arity mismatch");} else {}return (a864<<b865)|0;};var fxand = function(a866, b867) {if (arguments.length!==2) {throw $rjs_core.racketContractError("arity mismatch");} else {}return (a866&&b867)|0;};var fxior = function(a868, b869) {if (arguments.length!==2) {throw $rjs_core.racketContractError("arity mismatch");} else {}return (a868||b869)|0;};var fxxor = function(a870, b871) {if (arguments.length!==2) {throw $rjs_core.racketContractError("arity mismatch");} else {}return (a870^b871)|0;};var fxnot = M1.Core.bitwiseNot;var __rjs_quoted__ = {};export { __rjs_quoted__,fl_times_,fl_by_,fl_plus_,fl_,fl_lt_,fl_gt_,fl_lt__eq_,fl_gt__eq_,fl_eq_,flabs,flmin,flmax,flround,flfloor,flceiling,fltruncate,flsin,flcos,fltan,flasin,flacos,flatan,fllog,flexp,flsqrt,flexpt,fx_plus_,fx_,fx_times_,fxquotient,fxremainder,fxmodulo,fxabs,fx_eq_,fx_lt_,fx_lt__eq_,fx_gt_,fx_gt__eq_,fxmin,fxmax,fxrshift,fxlshift,fxand,fxior,fxxor,fxnot };