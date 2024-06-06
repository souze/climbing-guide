module Bridge exposing (..)

import ClimbTypes


type ToBackend
    = SmashedLikeButton
    | UpdateCrag Int ClimbTypes.Crag
    | RemoveCrag Int
    | AddCrag
