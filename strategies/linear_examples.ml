open LinearLambda.LlamHelpers
open LinearLambda.LlamInterpret
open Datatypes

let extropt = function None -> assert false | Some x -> x

let () = Printer.dispStrategy @@ stratOfTerm @@
    lambdaize "λf:A→A,x:A . f x"

let () = Printer.dispDebugStrategy @@ stratOfTerm @@
    lambdaize "(λf:A→A,x:A.f x)(λx:A.x)"
