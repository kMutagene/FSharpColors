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


type ColorScale = {ColorPositions : (Color * float) []}


let private normalize (array : float []) =
    let vMax = Array.max array
    let vMin = Array.min array
    let vDif = vMax - vMin
    Array.map (fun v -> (v - vMin) / vDif) array


let createColorScale colors positions =
    if Array.exists (fun p -> p > 100. || p < 0.) positions then failwith "ERROR: Positions must be between 0 and 100."
    if Array.head positions <> 0. || Array.last positions <> 100. then
        let alignedPositions = normalize positions |> Array.map ((*) 100.)
        {ColorPositions = Array.zip colors alignedPositions}
    else {ColorPositions = Array.zip colors positions}


let createColorScaleEquallySpaced (colors : Color []) =
    let noOfColors = colors.Length
    if noOfColors < 2 then 
        failwith "ERROR: At least two colors must be given."
    else
        let positions = Array.init noOfColors (fun i -> float i / (float noOfColors - 1.) * 100.)
        {ColorPositions = Array.zip colors positions}


let getColorAtPos colorScale pos =
    if pos > 100. || pos < 0. then failwith "ERROR: Position must be between 0 and 100."
    let posMatchedColor = Array.tryFind (fun (c,p) -> p = pos) colorScale.ColorPositions // is `pos` actually on the position of one of the given colors?
    if posMatchedColor.IsSome then fst posMatchedColor.Value
    else
        let colorArray = // get the first color with a position < pos and the first color with a position > pos
            let set1 = Array.filter (fun (c,p) -> p < pos) colorScale.ColorPositions
            let set2 = Array.filter (fun (c,p) -> p > pos) colorScale.ColorPositions
            [|Array.head set1; Array.head set2|]
        let c1 = Array.head colorArray
        let c2 = Array.last colorArray
        let a1 = float (fst c1).A
        let r1 = float (fst c1).R
        let g1 = float (fst c1).G
        let b1 = float (fst c1).B
        let a2 = float (fst c2).A
        let r2 = float (fst c2).R
        let g2 = float (fst c2).G
        let b2 = float (fst c2).B
        let p1 = snd c1
        let p2 = snd c2
        let calcNewV v1 v2 p v1p v2p = System.Math.Round((v2 + (v1 - v2) * (p / (v2p - v1p)) : float), 0) |> int
        let aFin = calcNewV a1 a2 pos p1 p2
        let rFin = calcNewV r1 r2 pos p1 p2
        let gFin = calcNewV g1 g2 pos p1 p2
        let bFin = calcNewV b1 b2 pos p1 p2
        Color.fromARGB aFin rFin gFin bFin


type internal ColorGradient(colors, ?positions) = // write overload with ?(colors * positions)

    let emptyColorScale = {ColorPositions = [||]}

    member this.ColorFields = colors

    member this.ColorScale =
        if positions.IsSome then createColorScale colors positions.Value
        else createColorScaleEquallySpaced colors

    member this.GetColorAt(pos) = getColorAtPos this.ColorScale pos