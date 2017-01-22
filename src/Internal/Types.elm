module Internal.Types exposing (..)

import Plot.Types exposing (..)


type alias Edges =
    { lower : Float
    , upper : Float
    }


type alias Oriented a =
    { x : a
    , y : a
    }


type alias Scale =
    { values : List Float
    , ticks : List Float
    , bounds : Edges
    , padding : Edges
    , offset : Edges
    , lengthTotal : Float
    , length : Float
    , toSvgCoords : Point -> Point
    , fromSvgCoords : Point -> Point
    }
