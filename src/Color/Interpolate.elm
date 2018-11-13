module Color.Interpolate exposing (Space(..), interpolate)

{-|


# Interpolate

Interpolate between two colors

@docs Space, interpolate

-}

import Color exposing (Color, hsla, rgba, toHsla, toRgba)
import Color.Convert exposing (colorToLab, labToColor)


{-| The color space that is used for the interpolation
-}
type Space
    = RGB
    | HSL
    | LAB


degree180 : Float
degree180 =
    degrees 180


degree360 : Float
degree360 =
    degrees 360


{-| Linear interpolation of two colors by a factor between `0` and `1`.
-}
interpolate : Space -> Color -> Color -> Float -> Color
interpolate space cl1 cl2 t =
    let
        i =
            linear t
    in
    case space of
        RGB ->
            let
                cl1_ =
                    toRgba cl1

                cl2_ =
                    toRgba cl2
            in
            rgba (i cl1_.red cl2_.red)
                (i cl1_.green cl2_.green)
                (i cl1_.blue cl2_.blue)
                (i cl1_.alpha cl2_.alpha)

        HSL ->
            let
                cl1_ =
                    toHsla cl1

                cl2_ =
                    toHsla cl2

                h1 =
                    cl1_.hue

                h2 =
                    cl2_.hue

                dH =
                    if h2 > h1 && h2 - h1 > degree180 then
                        h2 - h1 + degree360

                    else if h2 < h1 && h1 - h2 > degree180 then
                        h2 + degree360 - h1

                    else
                        h2 - h1
            in
            hsla (h1 + t * dH)
                (i cl1_.saturation cl2_.saturation)
                (i cl1_.lightness cl2_.lightness)
                (i cl1_.alpha cl2_.alpha)

        LAB ->
            let
                lab1 =
                    colorToLab cl1

                lab2 =
                    colorToLab cl2
            in
            labToColor
                { l = i lab1.l lab2.l
                , a = i lab1.a lab2.a
                , b = i lab1.b lab2.b
                }


linear : Float -> Float -> Float -> Float
linear t i1 i2 =
    i1 + (i2 - i1) * t
