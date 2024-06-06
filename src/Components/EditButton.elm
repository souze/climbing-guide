module Components.EditButton exposing (..)

import Element exposing (Element)
import Material.Icons as MaterialIcons
import Material.Icons.Types exposing (Coloring(..))
import Widget
import Widget.Icon as Icon
import Widget.Material as Material


view :
    { onClick : msg
    }
    -> Element msg
view props =
    Widget.iconButton
        (Material.iconButton Material.defaultPalette)
        { icon = MaterialIcons.edit_note |> Icon.elmMaterialIcons Color
        , onPress = Just props.onClick
        , text = "Edit"
        }
