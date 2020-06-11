(**
---
category: how-to
title: Creating Colors
menu_order: 1

---
*)

(***hide***)
#r "../../../build/FSharpColors.dll"
open FSharpColors

(**
Creating colors from input
--------------------------
FSharpColors supports parsing both raw color values and string representations (often found on the web) of them

### (a)rgb
*)

Color.fromARGB 100 100 100 100
Color.fromARGBString "argb(100,100,100,100)"

Color.fromRGB 100 100 100
Color.fromRGBString "rgb(100,100,100)"

(**
### cmyk
*)

Color.fromCMYK 0. 0. 0. 0.60
Color.fromCMYKString "cmyk(0%,0%,0%,60%)"

(**
### Hex
*)

//for hex strings, parsing both with and without the prefix is supported
Color.fromHexString "FFFFFF"
Color.fromHexString "0xFFFFFF"

(**
### Webcolors
*)

Color.fromWebColorString "#FFFFFF"
Color.fromStandardWebColor StandardWebColor.AntiqueWhite
