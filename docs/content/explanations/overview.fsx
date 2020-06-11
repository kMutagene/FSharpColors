(**
---
title: FSharpColors Overview
category: explanation
menu_order: 1
---
*)

(***hide***)
#r "../../../src/FSharpColors/bin/Release/netstandard2.0/FSharpColors.dll"

(**
# FSharpColors

FSharpColors is a lightweight utility library to create and manipulate colors completely written in F#.
*)

open FSharpColors

Color.fromRGB 13 3 7

(**
Here is the current feature set:

 - Converting bewteen
   - (a) r g b values
   - "(a)rgb(...)" strings
   - c m y k values 
   - "cmyk(...)" strings
   - W3C conform color keywords (such as "cyan" for rgb(0,255,255))
   - hex strings (also with 0x prefix)
   - webcolors (hex strings without 0x and w # prefix)

 - DU abstraction of all W3C conform color keywords
*)