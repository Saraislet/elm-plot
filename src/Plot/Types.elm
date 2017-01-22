module Plot.Types exposing (..)

{-| Types

# Definitions
@docs Point,  Value, Style
-}


{-| -}
type alias Value =
    Float


{-| Convenience type to represent coordinates.
-}
type alias Point =
    ( Value, Value )


{-| Convenience type to represent style.
-}
type alias Style =
    List ( String, String )
