module Internal.Draw
    exposing
        ( positionAttributes
        , PathType(..)
        , toPath
        , classAttribute
        , classBase
        , classAttributeOriented
        , toLinePath
        , toTranslate
        , toRotate
        , toStyle
        , toPixels
        , toPixelsInt
        , addDisplacement
        , toClipPathId
        )

import Svg exposing (Svg, Attribute)
import Svg.Attributes
import Internal.Scale exposing (..)
import Plot.Attributes exposing (Plot, Orientation(..), InterpolationOption(..))
import Plot.Types as Types exposing (..)


{- Common drawing functions. -}


positionAttributes : ( Float, Float ) -> ( Float, Float ) -> List (Attribute a)
positionAttributes ( x1, y1 ) ( x2, y2 ) =
    [ Svg.Attributes.x1 (toString x1)
    , Svg.Attributes.y1 (toString y1)
    , Svg.Attributes.x2 (toString x2)
    , Svg.Attributes.y2 (toString y2)
    ]


classAttribute : String -> List String -> Attribute a
classAttribute base classes =
    (classBase base)
        :: classes
        |> String.join " "
        |> Svg.Attributes.class


classAttributeOriented : String -> Orientation -> List String -> Attribute a
classAttributeOriented base orientation classes =
    case orientation of
        X ->
            (classBase base ++ "--x") :: classes |> classAttribute base

        Y ->
            (classBase base ++ "--y") :: classes |> classAttribute base


classBase : String -> String
classBase base =
    "elm-plot__" ++ base



-- Path Stuff


type PathType
    = L Point
    | M Point
    | S Point Point Point
    | Z


toPath : Plot -> List PathType -> String
toPath plot pathParts =
    List.foldl (\part result -> result ++ toPathTypeString plot part) "" pathParts


toPathTypeString : Plot -> PathType -> String
toPathTypeString plot pathType =
    case pathType of
        M point ->
            toPathTypeStringSinglePoint plot "M" point

        L point ->
            toPathTypeStringSinglePoint plot "L" point

        S p1 p2 p3 ->
            toPathTypeStringS plot p1 p2 p3

        Z ->
            "Z"


toPathTypeStringSinglePoint : Plot -> String -> Point -> String
toPathTypeStringSinglePoint plot typeString point =
    typeString ++ " " ++ pointToString plot point


toPathTypeStringS : Plot -> Point -> Point -> Point -> String
toPathTypeStringS plot p1 p2 p3 =
    let
        ( point1, point2 ) =
            toBezierPoints p1 p2 p3
    in
        "S" ++ " " ++ pointToString plot point1 ++ "," ++ pointToString plot point2


magnitude : Float
magnitude =
    0.5


toBezierPoints : Point -> Point -> Point -> ( Point, Point )
toBezierPoints ( x0, y0 ) ( x, y ) ( x1, y1 ) =
    ( ( x - ((x1 - x0) / 2 * magnitude), y - ((y1 - y0) / 2 * magnitude) )
    , ( x, y )
    )


pointToString : Plot -> Point -> String
pointToString plot point =
    let
        ( x, y ) =
            toSvgCoords plot.scales point
    in
        (toString x) ++ "," ++ (toString y)


toLinePath : InterpolationOption -> List Point -> List PathType
toLinePath interpolation =
    case interpolation of
        NoInterpolation ->
            List.map L

        Bezier ->
            toSPathTypes [] >> List.reverse


toSPathTypes : List PathType -> List Point -> List PathType
toSPathTypes result points =
    case points of
        [ p1, p2 ] ->
            S p1 p2 p2 :: result

        [ p1, p2, p3 ] ->
            toSPathTypes (S p1 p2 p3 :: result) [ p2, p3 ]

        p1 :: p2 :: p3 :: rest ->
            toSPathTypes (S p1 p2 p3 :: result) (p2 :: p3 :: rest)

        _ ->
            result



-- Utils


toClipPathId : Plot -> String
toClipPathId plot =
    plot.id ++ "__scale-clip-path"


toTranslate : ( Float, Float ) -> String
toTranslate ( x, y ) =
    "translate(" ++ (toString x) ++ "," ++ (toString y) ++ ")"


toRotate : Float -> Float -> Float -> String
toRotate d x y =
    "rotate(" ++ (toString d) ++ " " ++ (toString x) ++ " " ++ (toString y) ++ ")"


toStyle : List ( String, String ) -> String
toStyle styles =
    List.foldr (\( p, v ) r -> r ++ p ++ ":" ++ v ++ "; ") "" styles


toPixels : Float -> String
toPixels pixels =
    toString pixels ++ "px"


toPixelsInt : Int -> String
toPixelsInt =
    toPixels << toFloat


addDisplacement : Point -> Point -> Point
addDisplacement ( x, y ) ( dx, dy ) =
    ( x + dx, y + dy )
