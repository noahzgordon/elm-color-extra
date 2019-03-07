module Tests exposing (accessibility, blending, c1, c2, convert, gradient, interpolation, lab1, manipulate, p1, p1Result, p2, p2Result)

import Color exposing (Color, hsl, hsla, toRgba)
import Color.Accessibility exposing (..)
import Color.Blending as Ble exposing (..)
import Color.Convert exposing (..)
import Color.Gradient as Gra exposing (..)
import Color.Interpolate as Int exposing (..)
import Color.Manipulate as Man exposing (..)
import Expect exposing (Expectation, FloatingPointTolerance(..))
import Test exposing (..)


rgb : Float -> Float -> Float -> Color
rgb r g b =
    Color.rgb (r / 255) (g / 255) (b / 255)


rgba : Float -> Float -> Float -> Float -> Color
rgba r g b a =
    Color.rgba (r / 255) (g / 255) (b / 255) a


accessibility : Test
accessibility =
    describe "Accessibility"
        [ test "Contrast ratio of black and white should be 21:1" <|
            \() ->
                Expect.within (Absolute 0.001)
                    (contrastRatio Color.black Color.white)
                    21.0
        , test "Contrast ratio of equal colors should be 1:1" <|
            \() ->
                Expect.within (Absolute 0.001)
                    (contrastRatio Color.blue Color.blue)
                    1.0
        , test "Contrast ratio color order does not matter" <|
            \() ->
                Expect.within (Absolute 0.001)
                    (contrastRatio Color.green Color.blue)
                    (contrastRatio Color.blue Color.green)
        , test "Luminance of black is the minimum possible" <|
            \() ->
                Expect.within (Absolute 0.001)
                    (luminance Color.black)
                    0.0
        , test "Luminance of white is the maximum possible" <|
            \() ->
                Expect.within (Absolute 0.001)
                    (luminance Color.white)
                    1.0
        , test "Maximum contrast" <|
            \() ->
                Expect.equal
                    (maximumContrast Color.yellow
                        Color.white
                        [ Color.darkBlue
                        , Color.green
                        ]
                    )
                    Color.darkBlue
        ]


convert : Test
convert =
    describe "Convert"
        [ test "Color to rgb String" <|
            \() -> Expect.equal (colorToCssRgb (rgb 255 125 0)) "rgb(255, 125, 0)"
        , test "Color to rgba String" <|
            \() -> Expect.equal (colorToCssRgba (rgba 255 125 0 0.3)) "rgba(255, 125, 0, 0.3)"
        , test "Color to hsl String" <|
            \() -> Expect.equal (colorToCssHsl (hsl 0.4 0.2 0.2)) "hsl(144, 20%, 20%)"
        , test "Color to hsla String" <|
            \() -> Expect.equal (colorToCssHsla (hsla 0.4 0.2 0.2 1)) "hsla(144, 20%, 20%, 1)"
        , test "Color to hex String" <|
            \() -> Expect.equal (colorToHex (rgb 255 0 255)) "#ff00ff"
        , test "Color to hex String ignores alpha" <|
            \() -> Expect.equal (colorToHex (rgba 255 0 255 0)) "#ff00ff"
        , test "Color to hex String with alpha keeps #RRGGBB format when alpha = 1" <|
            \() -> Expect.equal (colorToHexWithAlpha (rgb 255 0 255)) "#ff00ff"
        , test "Color to hex String with alpha keeps #RRGGBB format when alpha = 1 (explicitly)" <|
            \() -> Expect.equal (colorToHexWithAlpha (rgba 255 0 255 1)) "#ff00ff"
        , test "Color to hex String with alpha uses #RRGGBBAA format when alpha /= 1" <|
            \() -> Expect.equal (colorToHexWithAlpha (rgba 255 0 255 0.5)) "#ff00ff80"
        , test "Hex string to hex color (#RRGGBB)" <|
            \() -> Expect.equal (hexToColor "#ff00ff") (Ok <| rgb 255 0 255)
        , test "Hex string to hex color (RRGGBB)" <|
            \() -> Expect.equal (hexToColor "ff00ff") (Ok <| rgb 255 0 255)
        , test "Hex string to hex color (#RGB)" <|
            \() -> Expect.equal (hexToColor "#f0f") (Ok <| rgb 255 0 255)
        , test "Hex string to hex color (RGB)" <|
            \() -> Expect.equal (hexToColor "0a0") (Ok <| rgb 0 170 0)
        , test "Hex string to hex color (#RRGGBBAA)" <|
            \() -> Expect.equal (hexToColor "#ff00ff80") (Ok <| rgba 255 0 255 0.5)
        , test "Hex string to hex color (RRGGBBAA)" <|
            \() -> Expect.equal (hexToColor "ff00ff80") (Ok <| rgba 255 0 255 0.5)
        , test "Hex string to hex color (#RGBA)" <|
            \() -> Expect.equal (hexToColor "#f0f0") (Ok <| rgba 255 0 255 0)
        , test "Hex string to hex color (RGBA)" <|
            \() -> Expect.equal (hexToColor "f0f0") (Ok <| rgba 255 0 255 0)
        , test "Hex string to hex color (fails)" <|
            \() -> Expect.equal (hexToColor "12345") (Err "Parsing hex regex failed")
        , test "Rgb to lab" <|
            \() -> Expect.equal lab1 (colorToLab (rgb 255 255 0))
        , test "Lab to rgb" <|
            expectColorSimilarity (rgb 255 255 0) (labToColor lab1)
        ]


lab1 : { l : Float, a : Float, b : Float }
lab1 =
    { l = 97.13824698129729, a = -21.555908334832285, b = 94.48248544644461 }


manipulate : Test
manipulate =
    describe "Manipulate"
        [ test "Darken" <|
            expectColorSimilarity (Man.darken 0.5 (hsl 1 1 0.75)) (hsl 1 1 0.25)
        , test "Darken should be limit to 0" <|
            expectColorSimilarity (Man.darken 10 (hsl 1 1 0.75)) (hsl 1 1 0)
        , test "Lighten" <|
            expectColorSimilarity (Man.lighten 0.5 (hsl 1 1 0.2)) (hsl 1 1 0.7)
        , test "Lighten should be limit to 1" <|
            expectColorSimilarity (Man.lighten 10 (hsl 1 1 0)) (hsl 1 1 1)
        , test "Saturate" <|
            expectColorSimilarity (saturate 0.5 (hsl 1 0 1)) (hsl 1 0.5 1)
        , test "Saturate should be limit to 1" <|
            expectColorSimilarity (saturate 10 (hsl 1 1 1)) (hsl 1 1 1)
        , test "Desaturate" <|
            expectColorSimilarity (desaturate 0.5 (hsl 1 1 1)) (hsl 1 0.5 1)
        , test "Desaturate should be limit to 0" <|
            expectColorSimilarity (desaturate 10 (hsl 1 1 1)) (hsl 1 0 1)
        , test "Grayscale" <|
            expectColorSimilarity (Man.grayscale (hsl 1 1 1)) (hsl 1 0 1)
        , test "Fade in" <|
            expectColorSimilarity (fadeIn 0.2 (hsla 1 1 1 0.5)) (hsla 1 1 1 0.7)
        , test "Fade in should be limit to 1" <|
            expectColorSimilarity (fadeIn 10 (hsla 1 1 1 0.5)) (hsla 1 1 1 1)
        , test "Fade out" <|
            expectColorSimilarity (fadeOut 0.2 (hsla 1 1 1 0.5)) (hsla 1 1 1 0.3)
        , test "Fade out should be limit to 0" <|
            expectColorSimilarity (fadeOut 10 (hsla 1 1 1 0.5)) (hsla 1 1 1 0)
        , test "Rotate hue" <|
            expectColorSimilarity (rotateHue 90 (hsla 0 1 1 0)) (hsla (degrees 90) 1 1 0)
        , test "Rotate hue with negative value" <|
            expectColorSimilarity (rotateHue -90 (hsla 0 1 1 0)) (hsla (degrees 270) 1 1 0)
        , test "Rotate hue for more then 360°" <|
            expectColorSimilarity (rotateHue 270 (hsla (degrees 180) 1 1 0))
                (hsla (degrees 90) 1 1 0)
        , test "Scale saturation with positive value" <|
            expectColorSimilarity (hsl (1 / 3) 0.51 0.9)
                (scaleHsl { saturationScale = 0.3, lightnessScale = 0, alphaScale = 0 } (hsl (1 / 3) 0.3 0.9))
        , test "Scale saturation with negative value" <|
            expectColorSimilarity (hsl (1 / 3) 0.21 0.9)
                (scaleHsl { saturationScale = -0.3, lightnessScale = 0, alphaScale = 0 } (hsl (1 / 3) 0.3 0.9))
        , test "Scale lightness with positive value" <|
            expectColorSimilarity (hsl (1 / 3) 0.3 0.915)
                (scaleHsl { saturationScale = 0, lightnessScale = 0.15, alphaScale = 0 } (hsl (1 / 3) 0.3 0.9))
        , test "Scale lightness with negative value" <|
            expectColorSimilarity (hsl (1 / 3) 0.3 0.765)
                (scaleHsl { saturationScale = 0, lightnessScale = -0.15, alphaScale = 0 } (hsl (1 / 3) 0.3 0.9))
        , test "Scale alpha with positive value" <|
            expectColorSimilarity (hsla (1 / 3) 0.3 0.9 0.14)
                (scaleHsl { saturationScale = 0, lightnessScale = 0, alphaScale = 0.14 } (hsla (1 / 3) 0.3 0.9 0))
        , test "Scale alpha with negative value" <|
            expectColorSimilarity (hsla (1 / 3) 0.3 0.9 0.86)
                (scaleHsl { saturationScale = 0, lightnessScale = 0, alphaScale = -0.14 } (hsl (1 / 3) 0.3 0.9))
        , test "Scale red channel with positive value" <|
            expectColorSimilarity (rgb 186.4 20 30) (scaleRgb { redScale = 0.3, greenScale = 0, blueScale = 0, alphaScale = 0 } (rgb 157 20 30))
        , test "Scale red channel with negative value" <| expectColorSimilarity (rgb 109.9 20 30) (scaleRgb { redScale = -0.3, greenScale = 0, blueScale = 0, alphaScale = 0 } (rgb 157 20 30))
        , test "Scale green channel with positive value" <| expectColorSimilarity (rgb 157 55.25 30) (scaleRgb { redScale = 0, greenScale = 0.15, blueScale = 0, alphaScale = 0 } (rgb 157 20 30))
        , test "Scale green channel with negative value" <| expectColorSimilarity (rgb 157 17 30) (scaleRgb { redScale = 0, greenScale = -0.15, blueScale = 0, alphaScale = 0 } (rgb 157 20 30))
        , test "Scale blue channel with positive value" <| expectColorSimilarity (rgb 157 20 61.5) (scaleRgb { redScale = 0, greenScale = 0, blueScale = 0.14, alphaScale = 0 } (rgb 157 20 30))
        , test "Scale blue channel with negative value" <| expectColorSimilarity (rgb 157 20 25.8) (scaleRgb { redScale = 0, greenScale = 0, blueScale = -0.14, alphaScale = 0 } (rgb 157 20 30))
        , test "Scale alpha channel with positive value" <| expectColorSimilarity (rgba 157 20 30 0.6) (scaleRgb { redScale = 0, greenScale = 0, blueScale = 0, alphaScale = 0.2 } (rgba 157 20 30 0.5))
        , test "Scale alpha channel with negative value" <| expectColorSimilarity (rgba 157 20 30 0.4) (scaleRgb { redScale = 0, greenScale = 0, blueScale = 0, alphaScale = -0.2 } (rgba 157 20 30 0.5))
        , test "Mix 1" <| expectColorSimilarity (rgb 127.5 0 127.5) (mix (rgb 255 0 0) (rgb 0 0 255))
        , test "Mix 2" <| expectColorSimilarity (rgb 127.5 127.5 127.5) (mix (rgb 255 255 0) (rgb 0 0 255))
        , test "Mix 3" <| expectColorSimilarity (rgb 127.5 144.5 85) (mix (rgb 255 119 0) (rgb 0 170 170))
        , test "Mix 4" <| expectColorSimilarity (rgb 63.75 0 191.25) (weightedMix (rgb 255 0 0) (rgb 0 0 255) 0.25)
        , test "Mix 5" <| expectColorSimilarity (rgba 63.75 0 191.25 0.75) (mix (rgba 255 0 0 0.5) (rgb 0 0 255))
        , test "Mix 6" <| expectColorSimilarity (rgb 255 0 0) (weightedMix (rgb 255 0 0) (rgb 0 0 255) 1)
        , test "Mix 7" <| expectColorSimilarity (rgb 0 0 255) (weightedMix (rgb 255 0 0) (rgb 0 0 255) 0)
        , test "Mix 8" <| expectColorSimilarity (rgba 255 0 0 0.5) (mix (rgb 255 0 0) (rgba 0 0 255 0))
        , test "Mix 9" <| expectColorSimilarity (rgba 0 0 255 0.5) (mix (rgba 255 0 0 0) (rgb 0 0 255))
        , test "Mix 10" <| expectColorSimilarity (rgb 255 0 0) (weightedMix (rgb 255 0 0) (rgba 0 0 255 0) 1)
        , test "Mix 11" <| expectColorSimilarity (rgb 0 0 255) (weightedMix (rgba 255 0 0 0) (rgb 0 0 255) 0)
        , test "Mix 12" <| expectColorSimilarity (rgba 0 0 255 0) (weightedMix (rgb 255 0 0) (rgba 0 0 255 0) 0)
        , test "Mix 13" <| expectColorSimilarity (rgba 255 0 0 0) (weightedMix (rgba 255 0 0 0) (rgb 0 0 255) 1)
        ]


expectColorSimilarity : Color -> Color -> (() -> Expectation)
expectColorSimilarity color1 color2 =
    let
        color1Rgb =
            toRgba color1

        color2Rgb =
            toRgba color2

        compare a b fn =
            \_ -> Expect.within (Absolute 0.01) (fn a) (fn b)
    in
    Expect.all (List.map (compare color1Rgb color2Rgb) [ .red, .green, .blue, .alpha ])


c1 : Color
c1 =
    rgb 255 102 0


c2 : Color
c2 =
    rgb 0 255 0


blending : Test
blending =
    describe "Blending"
        [ test "Multiply" <| expectColorSimilarity (multiply c1 c2) (rgb 0 102 0)
        , test "Screen" <| expectColorSimilarity (screen c1 c2) (rgb 255 255 0)
        , test "Overlay" <| expectColorSimilarity (overlay c1 c2) (rgb 255 204 0)
        , test "Softlight" <| expectColorSimilarity (softlight c1 c2) (rgb 255 161.27 0)
        , test "Hardlight" <| expectColorSimilarity (hardlight c1 c2) c2
        , test "Difference" <| expectColorSimilarity (difference c1 c2) (rgb 255 153 0)
        , test "Exclusion" <| expectColorSimilarity (exclusion c1 c2) (rgb 255 153 0)
        , test "Darken" <| expectColorSimilarity (Ble.darken c1 c2) (rgb 0 102 0)
        , test "Lighten" <| expectColorSimilarity (Ble.lighten c1 c2) (rgb 255 255 0)
        ]


interpolation : Test
interpolation =
    describe "Interpolate"
        [ test "Mix" <| \() -> Expect.equal (interpolate RGB (rgba 0 0 0 0) (rgba 255 255 255 1) 0.5) (rgba 127.5 127.5 127.5 0.5)
        ]


p1 : Palette
p1 =
    [ rgb 200 0 200
    , rgb 0 100 100
    , rgb 100 0 0
    ]


p1Result : Palette
p1Result =
    [ rgb 200 0 200
    , rgb 100 50 150
    , rgb 0 100 100
    , rgb 50 50 50
    , rgb 100 0 0
    ]


p2 : Gradient
p2 =
    [ ( 0, rgb 200 0 200 )
    , ( 0.25, rgb 0 100 100 )
    , ( 1, rgb 150 175 160 )
    ]


p2Result : Palette
p2Result =
    [ rgb 200 0 200
    , rgb 0 100 100
    , rgb 50 125 120
    , rgb 100 150 140
    , rgb 150 175 160
    ]


gradient : Test
gradient =
    describe "Gradient"
        [ test "Gradient from list" <| \() -> Expect.equal (Gra.linearGradient RGB p1 5) p1Result
        , test "Gradient from stops" <| \() -> Expect.equal (Gra.linearGradientFromStops RGB p2 5) p2Result
        ]
