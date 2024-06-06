module Components.TextButtons exposing (..)

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


textButton : String -> msg -> Element msg
textButton text msg =
    Widget.textButton
        (Material.textButton Material.defaultPalette)
        { text = text
        , onPress = Just msg
        }
