namespace FSharpColors

//http://www.niwa.nu/2013/05/math-behind-colorspace-conversions-rgb-hsl/
/// Represents an ARGB (alpha, red, green, blue) color

module internal Hex =
    
    open System

    [<CompiledName("ToHexDigit")>]
    let toHexDigit n =
        if n < 10 then char (n + 0x30) else char (n + 0x37)
    
    [<CompiledName("FromHexDigit")>]
    let fromHexDigit c =
        if c >= '0' && c <= '9' then int c - int '0'
        elif c >= 'A' && c <= 'F' then (int c - int 'A') + 10
        elif c >= 'a' && c <= 'f' then (int c - int 'a') + 10
        else raise <| new ArgumentException()
        
    [<CompiledName("Encode")>]
    let encode (prefix:string) (color:byte array)  =
        let hex = Array.zeroCreate (color.Length * 2)
        let mutable n = 0
        for i = 0 to color.Length - 1 do
            hex.[n] <- toHexDigit ((int color.[i] &&& 0xF0) >>> 4)
            n <- n + 1
            hex.[n] <- toHexDigit (int color.[i] &&& 0xF)
            n <- n + 1
        String.Concat(prefix, new String(hex))
        
    [<CompiledName("Decode")>]
    let decode (s:string) =
        match s with
        | null -> nullArg "s"
        | _ when s.Length = 0 -> Array.empty
        | _ ->
            let mutable len = s.Length
            let mutable i = 0
            if len >= 2 && s.[0] = '0' && (s.[1] = 'x' || s.[1] = 'X') then do
                len <- len - 2
                i <- i + 2
            if len % 2 <> 0 then invalidArg "s" "Invalid hex format"
            else
                let buf = Array.zeroCreate (len / 2)
                let mutable n = 0
                while i < s.Length do
                    buf.[n] <- byte (((fromHexDigit s.[i]) <<< 4) ||| (fromHexDigit s.[i + 1]))
                    i <- i + 2
                    n <- n + 1
                buf


/// Color component ARGB
type ColorComponent =
    | A of byte
    | R of byte
    | G of byte
    | B of byte 
    
    static member getComponentValue = function
        | A v -> v
        | R v -> v
        | G v -> v
        | B v -> v


//https://www.w3.org/TR/2011/REC-SVG11-20110816/types.html#ColorKeywords

///W3C Recognized color keyword names
type StandardWebColor =
    | AliceBlue               
    | AntiqueWhite            
    | Aqua                    
    | Aquamarine              
    | Azure                   
    | Beige                   
    | Bisque                  
    | Black                   
    | BlanchedAlmond          
    | Blue                    
    | Blueviolet              
    | Brown                   
    | BurlyWood               
    | CadetBlue               
    | Chartreuse              
    | Chocolate               
    | Coral                   
    | CornflowerBlue          
    | CornSilk                
    | Crimson                 
    | Cyan                    
    | DarkBlue                
    | DarkCyan                
    | DarkGoldenRod           
    | DarkGray                
    | DarkGreen               
    | DarkGrey                
    | DarkKhaki               
    | DarkMagenta             
    | Darkolivegreen          
    | DarkOrange              
    | DarkOrchid              
    | DarkRed                 
    | DarkSalmon              
    | DarkSeaGreen            
    | DarkSlateBlue           
    | DarkSlateGray           
    | DarkSlateGrey           
    | DarkTurquoise           
    | DarkViolet              
    | DeepPink                
    | DeepSkyBlue             
    | DimGray                 
    | DimGrey                 
    | DodgerBlue              
    | FireBrick               
    | FloralWhite             
    | ForestGreen             
    | Fuchsia                 
    | Gainsboro               
    | GhostWhite              
    | Gold                    
    | GoldenRod               
    | Gray                    
    | Grey                    
    | Green                   
    | GreenYellow             
    | Honeydew                
    | Hotpink                 
    | IndianRed               
    | Indigo                  
    | Ivory                   
    | Khaki                   
    | Lavender                
    | LavenderBlush           
    | LawnGreen               
    | LemonChiffon            
    | LightBlue               
    | LightCoral              
    | LightCyan               
    | LightGoldenRodYellow    
    | LightGray               
    | LightGreen              
    | LightGrey               
    | LightPink               
    | LightAalmon             
    | LightAeaGreen           
    | LightAkyBlue            
    | LightAlateGray          
    | LightslateGrey          
    | LightSteelBlue          
    | LightYellow             
    | Lime                    
    | Limegreen               
    | Linen                   
    | Magenta                 
    | Maroon                  
    | MediumAquamarine        
    | MediumBlue              
    | MediumOrchid            
    | MediumPurple            
    | MediumSeaGreen          
    | MediumSlateBlue         
    | MediumSpringGreen       
    | MediumTurquoise         
    | MediumVioletRed         
    | MidnightBlue            
    | MintCream               
    | MistyRose               
    | Moccasin                
    | NavajoWhite             
    | Navy                    
    | OldLace                 
    | Olive                   
    | OliveDrab               
    | Orange                  
    | OrangeRed               
    | Orchid                  
    | PaleGoldenRod           
    | PaleGreen               
    | PaleTurquoise           
    | PaleVioletRed           
    | PapayaWhip              
    | PeachPuff               
    | Peru                    
    | Pink                    
    | Plum                    
    | PowderBlue              
    | Purple                  
    | Red                     
    | RosyBrown               
    | RoyalBlue               
    | SaddleBrown             
    | Salmon                  
    | SandyBrown              
    | SeaGreen                
    | SeaShell                
    | Sienna                  
    | Silver                  
    | Skyblue                 
    | SlateBlue               
    | SlateGray               
    | SlateGrey               
    | Snow                    
    | SpringGreen             
    | SteelBlue               
    | Tan                     
    | Teal                    
    | Thistle                 
    | Tomato                  
    | Turquoise               
    | Violet                  
    | Wheat                   
    | White                   
    | WhiteSmoke              
    | Yellow                  
    | YellowGreen             
    

    static member toRGB = function
        | AliceBlue               -> (240, 248, 255)
        | AntiqueWhite            -> (250, 235, 215)
        | Aqua                    -> ( 0, 255, 255)
        | Aquamarine              -> (127, 255, 212)
        | Azure                   -> (240, 255, 255)
        | Beige                   -> (245, 245, 220)
        | Bisque                  -> (255, 228, 196)
        | Black                   -> ( 0, 0, 0)
        | BlanchedAlmond          -> (255, 235, 205)
        | Blue                    -> ( 0, 0, 255)
        | Blueviolet              -> (138, 43, 226)
        | Brown                   -> (165, 42, 42)
        | BurlyWood               -> (222, 184, 135)
        | CadetBlue               -> ( 95, 158, 160)
        | Chartreuse              -> (127, 255, 0)
        | Chocolate               -> (210, 105, 30)
        | Coral                   -> (255, 127, 80)
        | CornflowerBlue          -> (100, 149, 237)
        | CornSilk                -> (255, 248, 220)
        | Crimson                 -> (220, 20, 60)
        | Cyan                    -> ( 0, 255, 255)
        | DarkBlue                -> ( 0, 0, 139)
        | DarkCyan                -> ( 0, 139, 139)
        | DarkGoldenRod           -> (184, 134, 11)
        | DarkGray                -> (169, 169, 169)
        | DarkGreen               -> ( 0, 100, 0)
        | DarkGrey                -> (169, 169, 169)
        | DarkKhaki               -> (189, 183, 107)
        | DarkMagenta             -> (139, 0, 139)
        | Darkolivegreen          -> ( 85, 107, 47)
        | DarkOrange              -> (255, 140, 0)
        | DarkOrchid              -> (153, 50, 204)
        | DarkRed                 -> (139, 0, 0)
        | DarkSalmon              -> (233, 150, 122)
        | DarkSeaGreen            -> (143, 188, 143)
        | DarkSlateBlue           -> ( 72, 61, 139)
        | DarkSlateGray           -> ( 47, 79, 79)
        | DarkSlateGrey           -> ( 47, 79, 79)
        | DarkTurquoise           -> ( 0, 206, 209)
        | DarkViolet              -> (148, 0, 211)
        | DeepPink                -> (255, 20, 147)
        | DeepSkyBlue             -> ( 0, 191, 255)
        | DimGray                 -> (105, 105, 105)
        | DimGrey                 -> (105, 105, 105)
        | DodgerBlue              -> ( 30, 144, 255)
        | FireBrick               -> (178, 34, 34)
        | FloralWhite             -> (255, 250, 240)
        | ForestGreen             -> ( 34, 139, 34)
        | Fuchsia                 -> (255, 0, 255)
        | Gainsboro               -> (220, 220, 220)
        | GhostWhite              -> (248, 248, 255)
        | Gold                    -> (255, 215, 0)
        | GoldenRod               -> (218, 165, 32)
        | Gray                    -> (128, 128, 128)
        | Grey                    -> (128, 128, 128)
        | Green                   -> ( 0, 128, 0)
        | GreenYellow             -> (173, 255, 47)
        | Honeydew                -> (240, 255, 240)
        | Hotpink                 -> (255, 105, 180)
        | IndianRed               -> (205, 92, 92)
        | Indigo                  -> ( 75, 0, 130)
        | Ivory                   -> (255, 255, 240)
        | Khaki                   -> (240, 230, 140)
        | Lavender                -> (230, 230, 250)
        | LavenderBlush           -> (255, 240, 245)
        | LawnGreen               -> (124, 252, 0)
        | LemonChiffon            -> (255, 250, 205)
        | LightBlue               -> (173, 216, 230)
        | LightCoral              -> (240, 128, 128)
        | LightCyan               -> (224, 255, 255)
        | LightGoldenRodYellow    -> (250, 250, 210)
        | LightGray               -> (211, 211, 211)
        | LightGreen              -> (144, 238, 144)
        | LightGrey               -> (211, 211, 211)
        | LightPink               -> (255, 182, 193)
        | LightAalmon             -> (255, 160, 122)
        | LightAeaGreen           -> ( 32, 178, 170)
        | LightAkyBlue            -> (135, 206, 250)
        | LightAlateGray          -> (119, 136, 153)
        | LightslateGrey          -> (119, 136, 153)
        | LightSteelBlue          -> (176, 196, 222)
        | LightYellow             -> (255, 255, 224)
        | Lime                    -> ( 0, 255, 0)
        | Limegreen               -> ( 50, 205, 50)
        | Linen                   -> (250, 240, 230)
        | Magenta                 -> (255, 0, 255)
        | Maroon                  -> (128, 0, 0)
        | MediumAquamarine        -> (102, 205, 170)
        | MediumBlue              -> ( 0, 0, 205)
        | MediumOrchid            -> (186, 85, 211)
        | MediumPurple            -> (147, 112, 219)
        | MediumSeaGreen          -> ( 60, 179, 113)
        | MediumSlateBlue         -> (123, 104, 238)
        | MediumSpringGreen       -> ( 0, 250, 154)
        | MediumTurquoise         -> ( 72, 209, 204)
        | MediumVioletRed         -> (199, 21, 133)
        | MidnightBlue            -> ( 25, 25, 112)
        | MintCream               -> (245, 255, 250)
        | MistyRose               -> (255, 228, 225)
        | Moccasin                -> (255, 228, 181)
        | NavajoWhite             -> (255, 222, 173)
        | Navy                    -> ( 0, 0, 128)
        | OldLace                 -> (253, 245, 230)
        | Olive                   -> (128, 128, 0)
        | OliveDrab               -> (107, 142, 35)
        | Orange                  -> (255, 165, 0)
        | OrangeRed               -> (255, 69, 0)
        | Orchid                  -> (218, 112, 214)
        | PaleGoldenRod           -> (238, 232, 170)
        | PaleGreen               -> (152, 251, 152)
        | PaleTurquoise           -> (175, 238, 238)
        | PaleVioletRed           -> (219, 112, 147)
        | PapayaWhip              -> (255, 239, 213)
        | PeachPuff               -> (255, 218, 185)
        | Peru                    -> (205, 133, 63)
        | Pink                    -> (255, 192, 203)
        | Plum                    -> (221, 160, 221)
        | PowderBlue              -> (176, 224, 230)
        | Purple                  -> (128, 0, 128)
        | Red                     -> (255, 0, 0)
        | RosyBrown               -> (188, 143, 143)
        | RoyalBlue               -> ( 65, 105, 225)
        | SaddleBrown             -> (139, 69, 19)
        | Salmon                  -> (250, 128, 114)
        | SandyBrown              -> (244, 164, 96)
        | SeaGreen                -> ( 46, 139, 87)
        | SeaShell                -> (255, 245, 238)
        | Sienna                  -> (160, 82, 45)
        | Silver                  -> (192, 192, 192)
        | Skyblue                 -> (135, 206, 235)
        | SlateBlue               -> (106, 90, 205)
        | SlateGray               -> (112, 128, 144)
        | SlateGrey               -> (112, 128, 144)
        | Snow                    -> (255, 250, 250)
        | SpringGreen             -> ( 0, 255, 127)
        | SteelBlue               -> ( 70, 130, 180)
        | Tan                     -> (210, 180, 140)
        | Teal                    -> ( 0, 128, 128)
        | Thistle                 -> (216, 191, 216)
        | Tomato                  -> (255, 99, 71)
        | Turquoise               -> ( 64, 224, 208)
        | Violet                  -> (238, 130, 238)
        | Wheat                   -> (245, 222, 179)
        | White                   -> (255, 255, 255)
        | WhiteSmoke              -> (245, 245, 245)
        | Yellow                  -> (255, 255, 0)
        | YellowGreen             -> (154, 205, 50)

    static member ofKeyWord = function
        | "aliceblue"               -> AliceBlue               
        | "antiquewhite"            -> AntiqueWhite            
        | "aqua"                    -> Aqua                    
        | "aquamarine"              -> Aquamarine              
        | "azure"                   -> Azure                   
        | "beige"                   -> Beige                   
        | "bisque"                  -> Bisque                  
        | "black"                   -> Black                   
        | "blanchedalmond"          -> BlanchedAlmond          
        | "blue"                    -> Blue                    
        | "blueviolet"              -> Blueviolet              
        | "brown"                   -> Brown                   
        | "burlywood"               -> BurlyWood               
        | "cadetblue"               -> CadetBlue               
        | "chartreuse"              -> Chartreuse              
        | "chocolate"               -> Chocolate               
        | "coral"                   -> Coral                   
        | "cornflowerblue"          -> CornflowerBlue          
        | "cornsilk"                -> CornSilk                
        | "crimson"                 -> Crimson                 
        | "cyan"                    -> Cyan                    
        | "darkblue"                -> DarkBlue                
        | "darkcyan"                -> DarkCyan                
        | "darkgoldenrod"           -> DarkGoldenRod           
        | "darkgray"                -> DarkGray                
        | "darkgreen"               -> DarkGreen               
        | "darkgrey"                -> DarkGrey                
        | "darkkhaki"               -> DarkKhaki               
        | "darkmagenta"             -> DarkMagenta             
        | "darkolivegreen"          -> Darkolivegreen          
        | "darkorange"              -> DarkOrange              
        | "darkorchid"              -> DarkOrchid              
        | "darkred"                 -> DarkRed                 
        | "darksalmon"              -> DarkSalmon              
        | "darkseagreen"            -> DarkSeaGreen            
        | "darkslateblue"           -> DarkSlateBlue           
        | "darkslategray"          ->  DarkSlateGray           
        | "darkslategrey"           -> DarkSlateGrey           
        | "darkturquoise"           -> DarkTurquoise           
        | "darkviolet"              -> DarkViolet              
        | "deeppink"                -> DeepPink                
        | "deepskyblue"             -> DeepSkyBlue             
        | "dimgray"                 -> DimGray                 
        | "dimgrey"                 -> DimGrey                 
        | "dodgerblue"              -> DodgerBlue              
        | "firebrick"               -> FireBrick               
        | "floralwhite"             -> FloralWhite             
        | "forestgreen"             -> ForestGreen             
        | "fuchsia"                 -> Fuchsia                 
        | "gainsboro"               -> Gainsboro               
        | "ghostwhite"              -> GhostWhite              
        | "gold"                    -> Gold                    
        | "goldenrod"               -> GoldenRod               
        | "gray"                    -> Gray                    
        | "grey"                    -> Grey                    
        | "green"                   -> Green                   
        | "greenyellow"             -> GreenYellow             
        | "honeydew"                -> Honeydew                
        | "hotpink"                 -> Hotpink                 
        | "indianred"               -> IndianRed               
        | "indigo"                  -> Indigo                  
        | "ivory"                   -> Ivory                   
        | "khaki"                   -> Khaki                   
        | "lavender"                -> Lavender                
        | "lavenderblush"           -> LavenderBlush           
        | "lawngreen"               -> LawnGreen               
        | "lemonchiffon"            -> LemonChiffon            
        | "lightblue"               -> LightBlue               
        | "lightcoral"              -> LightCoral              
        | "lightcyan"               -> LightCyan               
        | "lightgoldenrodyellow"    -> LightGoldenRodYellow    
        | "lightgray"               -> LightGray               
        | "lightgreen"              -> LightGreen              
        | "lightgrey"               -> LightGrey               
        | "lightpink"               -> LightPink               
        | "lightsalmon"             -> LightAalmon             
        | "lightseagreen"           -> LightAeaGreen           
        | "lightskyblue"            -> LightAkyBlue            
        | "lightslategray"          -> LightAlateGray          
        | "lightslategrey"          -> LightslateGrey          
        | "lightsteelblue"          -> LightSteelBlue          
        | "lightyellow"             -> LightYellow             
        | "lime"                    -> Lime                    
        | "limegreen"               -> Limegreen               
        | "linen"                   -> Linen                   
        | "magenta"                 -> Magenta                 
        | "maroon"                  -> Maroon                  
        | "mediumaquamarine"        -> MediumAquamarine        
        | "mediumblue"              -> MediumBlue              
        | "mediumorchid"            -> MediumOrchid            
        | "mediumpurple"            -> MediumPurple            
        | "mediumseagreen"          -> MediumSeaGreen          
        | "mediumslateblue"         -> MediumSlateBlue         
        | "mediumspringgreen"       -> MediumSpringGreen       
        | "mediumturquoise"         -> MediumTurquoise         
        | "mediumvioletred"         -> MediumVioletRed         
        | "midnightblue"            -> MidnightBlue            
        | "mintcream"               -> MintCream               
        | "mistyrose"               -> MistyRose               
        | "moccasin"                -> Moccasin                
        | "navajowhite"             -> NavajoWhite             
        | "navy"                    -> Navy                    
        | "oldlace"                 -> OldLace                 
        | "olive"                   -> Olive                   
        | "olivedrab"               -> OliveDrab               
        | "orange"                  -> Orange                  
        | "orangered"               -> OrangeRed               
        | "orchid"                  -> Orchid                  
        | "palegoldenrod"           -> PaleGoldenRod           
        | "palegreen"               -> PaleGreen               
        | "paleturquoise"           -> PaleTurquoise           
        | "palevioletred"           -> PaleVioletRed           
        | "papayawhip"              -> PapayaWhip              
        | "peachpuff"               -> PeachPuff               
        | "peru"                    -> Peru                    
        | "pink"                    -> Pink                    
        | "plum"                    -> Plum                    
        | "powderblue"              -> PowderBlue              
        | "purple"                  -> Purple                  
        | "red"                     -> Red                     
        | "rosybrown"               -> RosyBrown               
        | "royalblue"               -> RoyalBlue               
        | "saddlebrown"             -> SaddleBrown             
        | "salmon"                  -> Salmon                  
        | "sandybrown"              -> SandyBrown              
        | "seagreen"                -> SeaGreen                
        | "seashell"                -> SeaShell                
        | "sienna"                  -> Sienna                  
        | "silver"                  -> Silver                  
        | "skyblue"                 -> Skyblue                 
        | "slateblue"               -> SlateBlue               
        | "slategray"               -> SlateGray               
        | "slategrey"               -> SlateGrey               
        | "snow"                    -> Snow                    
        | "springgreen"             -> SpringGreen             
        | "steelblue"               -> SteelBlue               
        | "tan"                     -> Tan                     
        | "teal"                    -> Teal                    
        | "thistle"                 -> Thistle                 
        | "tomato"                  -> Tomato                  
        | "turquoise"               -> Turquoise               
        | "violet"                  -> Violet                  
        | "wheat"                   -> Wheat                   
        | "white"                   -> White                   
        | "whitesmoke"              -> WhiteSmoke              
        | "yellow"                  -> Yellow                  
        | "yellowgreen"             -> YellowGreen             
        | _                         -> failwith "not a valid color keyword"
    
/// Color structure
type Color = {
    /// The alpha component value of this Color structure.
    A : byte
    /// The red component value of this Color structure.
    R : byte
    /// The green component value of this Color structure.
    G : byte
    /// The blue component value of this Color structure.
    B : byte
    }
with
    /// Returns the highest color component of the given color
    static member getMaxRGB (c:Color) =
        let r,g,b = R c.R,G c.G,B c.B
        max r g |> max b

    /// Returns the lowest color component of the given color
    static member getMinRGB c =
        let r,g,b = R c.R,G c.G,B c.B
        min r g |> min b
        
    /// Creates a Color structure from the four ARGB component (alpha, red, green, and blue) values.
    static member fromArgb a r g b =
        let f v =
            if v < 0 || v > 255 then 
                failwithf "Value for component needs to be between 0 and 255."
            else
                byte v
        {A= f a; R = f r; G = f g; B = f b}

    /// Creates a Color structure from the specified color values (red, green, and blue).
    /// The alpha value is implicitly 255 (fully opaque). 
    static member fromRgb r g b =
        Color.fromArgb 255 r g b

    /// Creates a Color structure from the given W3C conform standard web color type.
    static member fromStandardWebColor (swc:StandardWebColor) =
        swc
        |> StandardWebColor.toRGB
        |> fun (r,g,b) -> Color.fromRgb r g b

    /// Creates a Color structure from the given W3C conform standard web color keyword .
    static member fromColorKeyword (keyWord:string) =
        keyWord
        |> StandardWebColor.ofKeyWord
        |> StandardWebColor.toRGB
        |> fun (r,g,b) -> Color.fromRgb r g b

    //    /// Gets the hue-saturation-brightness (HSB) brightness value for this Color structure.
    //    let getBrightness = ()

    /// Gets the hue-saturation-brightness (HSB) hue value, in degrees, for this Color structure.
    static member getHue c =
        let min = Color.getMinRGB c |> ColorComponent.getComponentValue
        match Color.getMaxRGB c with
        | R r -> float (c.G - c.B) / float (r - min)
        | G g -> 2.0 + float (c.B - c. R) / float (g - min)
        | B b -> 4.0 + float (c.R - c.G) / float (b - min)
        | _   -> failwithf "" // can't be


    /// Gets the hue-saturation-brightness (HSB) saturation value for this Color structure.
    static member getSaturation col =
        let minimum = Color.getMinRGB col
        let maximum = Color.getMaxRGB col
        float (ColorComponent.getComponentValue minimum + ColorComponent.getComponentValue maximum) / 2.
        |> round
           
    /// Gets the 32-bit ARGB value of this Color structure.
    static member toArgb c =
        (int c.A, int c.R, int c.G, int c.B)
    
    /// Gets the hex representataion (FFFFFF) of a color (with valid prefix "0xFFFFFF")
    static member toHex prefix (c:Color) =
        let prefix' = if prefix then "0x" else ""
        Hex.encode prefix' [|c.R;c.G;c.B|]

    /// Gets color from hex representataion (FFFFFF) or (0xFFFFFF)
    static member fromHex (s:string) =
        match (Hex.decode s) with
        | [|r;g;b|]  -> Color.fromRgb (int r) (int g) (int b)
        | _          -> failwithf "Invalid hex color format"

    /// Gets the web color representataion (#FFFFFF)
    static member toWebColor c =        
        Hex.encode "#" [|c.R;c.G;c.B|]                

    /// Gets color from web color (#FFFFFF)
    static member fromWebColor (s:string) =
        let s' = s.TrimStart([|'#'|])
        match (Hex.decode s') with
        | [|r;g;b|]  -> Color.fromRgb (int r) (int g) (int b)
        | _          -> failwithf "Invalid hex color format"


    /// Converts this Color structure to a human-readable string.
    static member toString c =
        let a,r,g,b = Color.toArgb c
        sprintf "{Alpha: %i Red: %i Green: %i Blue: %i}" a r g b




// http://graphicdesign.stackexchange.com/questions/3682/where-can-i-find-a-large-palette-set-of-contrasting-colors-for-coloring-many-d
module Table =    

    let black       = Color.fromRgb   0   0   0                
    let blackLite   = Color.fromRgb  89  89  89 // 35% lighter
    let white       = Color.fromRgb 255 255 255

    /// Color palette from Microsoft office 2016
    module Office = 
        
        // blue
        let blue        = Color.fromRgb  65 113 156        
        let lightBlue   = Color.fromRgb 189 215 238
        let darkBlue    = Color.fromRgb  68 114 196
                        
        // red           
        let red         = Color.fromRgb 241  90  96  
        let lightRed    = Color.fromRgb 252 212 214

        // orange           
        let orange      = Color.fromRgb 237 125  49
        let lightOrange = Color.fromRgb 248 203 173
                                                                  
        // yellow        
        let yellow      = Color.fromRgb 255 217 102
        let lightYellow = Color.fromRgb 255 230 153
        let darkYellow  = Color.fromRgb 255 192   0
                         
        // green         
        let green       = Color.fromRgb 122 195 106
        let lightGreen  = Color.fromRgb 197 224 180
        let darkGreen   = Color.fromRgb 112 173  71

        // grey         
        let grey        = Color.fromRgb 165 165 165
        let lightGrey   = Color.fromRgb 217 217 217

    // From publication: Escaping RGBland: Selecting Colors for Statistical Graphics
    // http://epub.wu.ac.at/1692/1/document.pdf
    module StatisticalGraphics24 =
        let a = 1
    // 
    //{2,63,165},{125,135,185},{190,193,212},{214,188,192},{187,119,132},{142,6,59},{74,111,227},{133,149,225},{181,187,227},{230,175,185},{224,123,145},{211,63,106},{17,198,56},{141,213,147},{198,222,199},{234,211,198},{240,185,141},{239,151,8},{15,207,192},{156,222,214},{213,234,231},{243,225,235},{246,196,225},{247,156,212}

