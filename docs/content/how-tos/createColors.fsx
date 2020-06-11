(**
---
category: how-to
title: Creating Colors
menu_order: 1

---
*)

(***hide***)
#r "../../../build/FSharpColors.dll"

(**
Creating Colors
---------------
*)

/// The Hello World of functional languages!
open FSharpColors

Color.fromArgb 255 2 143 204

Color.fromRgb 2 143 204

Color.fromWebColor "#028FCC"

Color.fromHex "0x028FCC"

(**
Hiding code
-----------

If you want to include some code in the source code,
but omit it from the output, you can use the `hide`
command.
*)

(*** hide ***)
/// This is a hidden answer
let hidden = 42

(**
The value will be defined in the F# code and so you
can use it from other (visible) code and get correct
tool tips:
*)

let answer = hidden

(**
Moving code around
------------------

Sometimes, it is useful to first explain some code that
has to be located at the end of the snippet (perhaps
because it uses some definitions discussed in the middle).
This can be done using `include` and `define` commands.

The following snippet gets correct tool tips, even though
it uses `laterFunction`:
*)

(*** include:later-bit ***)

(**
Then we can explain how `laterFunction` is defined:
*)

let laterFunction() =
  "Not very difficult, is it?"

(**
This example covers pretty much all features that are
currently implemented in `literate.fsx`, but feel free
to [fork the project on GitHub][fs] and add more
features or report bugs!

  [fs]: https://github.com/fsprojects/FSharp.Formatting

*)

(*** define:later-bit ***)
let sample =
  laterFunction()
  |> printfn "Got: %s"