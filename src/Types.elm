module Types (..) where

import Math.Vector3 exposing (Vec3)

type alias Person =
    { position : Vec3
    , velocity : Vec3
    , rotation : Float
    , lookVert : Float
    }