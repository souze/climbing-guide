module Main.Pages.Msg exposing (Msg(..))

import Pages.Home_
import Pages.Crags
import Pages.Crags.Id_
import Pages.NotFound_


type Msg
    = Home_ Pages.Home_.Msg
    | Crags Pages.Crags.Msg
    | Crags_Id_ Pages.Crags.Id_.Msg
    | NotFound_ Pages.NotFound_.Msg
