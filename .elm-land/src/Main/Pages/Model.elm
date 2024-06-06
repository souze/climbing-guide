module Main.Pages.Model exposing (Model(..))

import Pages.Home_
import Pages.Crags
import Pages.Crags.Id_
import Pages.NotFound_
import View exposing (View)


type Model
    = Home_ Pages.Home_.Model
    | Crags Pages.Crags.Model
    | Crags_Id_ { id : String } Pages.Crags.Id_.Model
    | NotFound_ Pages.NotFound_.Model
    | Redirecting_
    | Loading_
