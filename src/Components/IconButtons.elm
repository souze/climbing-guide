module Components.IconButtons exposing (..)

import Components.EditButton
import Element exposing (Element)
import Element.Events
import Element.Input
import Html
import Material.Icons as MaterialIcons
import Material.Icons.Types exposing (Coloring(..))
import Pages.NotFound_ exposing (Msg)
import Widget
import Widget.Icon as Icon
import Widget.Material as Material


commitButton : msg -> Element msg
commitButton msg =
    Widget.iconButton
        (Material.iconButton Material.defaultPalette)
        { icon = MaterialIcons.check_circle |> Icon.elmMaterialIcons Color
        , onPress = Just msg
        , text = "Commit"
        }


cancelButton : msg -> Element msg
cancelButton msg =
    Widget.iconButton
        (Material.iconButton Material.defaultPalette)
        { icon = MaterialIcons.close |> Icon.elmMaterialIcons Color
        , onPress = Just msg
        , text = "Cancel"
        }


addButton : String -> msg -> Element msg
addButton text msg =
    iconButton MaterialIcons.library_add msg text


iconButton : (Int -> Coloring -> Html.Html msg) -> msg -> String -> Element msg
iconButton icon msg text =
    Widget.iconButton
        (Material.iconButton Material.defaultPalette)
        { icon = icon |> Icon.elmMaterialIcons Color
        , onPress = Just msg
        , text = text
        }
