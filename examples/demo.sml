(* demo.sml - lint a deliberately messy sample module and print the report.
   Deterministic: identical output on every run and under both compilers. *)

val sample =
  "open Helpers\n\
  \\n\
  \datatype shape = circle of int | Square of int | Triangle of int\n\
  \\n\
  \fun area s =\n\
  \  case s of\n\
  \      circle r => r * r * 3\n\
  \    | Square w => w * w\n\
  \\n\
  \fun describe size =\n\
  \  let\n\
  \    val size = size * 2\n\
  \    val label = \"shape\"\n\
  \  in\n\
  \    area (Square (size))\n\
  \  end\n"

val () = print "--- source under lint ---\n"
val () = print sample
val () = print "\n--- lint report ---\n"
val () = print (Lint.report sample)
