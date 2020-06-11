#load "Color.fs"

open FSharpColors
open System.Text.RegularExpressions

let testColor1 = {A=1uy; R=3uy; G=3uy; B=7uy}

testColor1
|> Color.toCMYKString
|> Color.fromCMYKString
|> Color.toCMYK
|> fun (c,y,m,k) -> Color.fromCMYK c y m k
|> Color.toARGBString
|> Color.fromARGBString
|> Color.toARGB
|> fun (a,r,g,b) -> Color.fromARGB a r g b
|> Color.toRGBString
|> Color.fromRGBString
|> Color.toRGB
|> fun (r,g,b) -> Color.fromRGB r g b
|> Color.toHexString false
|> Color.fromHexString
|> Color.toWebColorString
|> Color.fromWebColorString

Color.fromColorKeyword "cyan"
|> Color.toCMYKString
60.4123 % 2.