module Components.TitleTextEdit exposing (Mode(..), multilineEditor, viewInlineTight, viewTextField, viewTitle)

import Components.EditButton
import Components.IconButtons
import Element exposing (Element)
import Element.Events
import Element.Input
import Material.Icons as MaterialIcons
import Material.Icons.Types exposing (Coloring(..))
import Pages.NotFound_ exposing (Msg)
import Widget
import Widget.Icon as Icon
import Widget.Material as Material


viewEdit :
    { text : String
    , onChange : String -> msg
    , onSave : msg
    , onCancel : msg
    }
    -> Element msg
viewEdit props =
    Widget.textInput
        (Material.textInput Material.defaultPalette)
        { chips =
            [ { icon = always Element.none
              , text = "✅"
              , onPress = Just props.onSave
              }
            , { icon = always Element.none
              , text = "❌"
              , onPress = Just props.onCancel
              }
            ]
        , text = props.text
        , placeholder = Nothing
        , label = "Editing name"
        , onChange = props.onChange
        }


type Mode
    = Normal
    | Hover
    | Edit


viewTitle :
    { text : String
    , onChange : String -> msg
    , onSave : msg
    , onCancel : msg
    , onEnter : msg
    , onLeave : msg
    , onClickEdit : msg
    , mode : Mode
    , style : List (Element.Attribute msg)
    , withTrash : Maybe msg
    }
    -> Element msg
viewTitle { text, onChange, onSave, onCancel, onEnter, onLeave, onClickEdit, mode, style, withTrash } =
    case mode of
        Edit ->
            viewEdit
                { onChange = onChange
                , onSave = onSave
                , onCancel = onCancel
                , text = text
                }

        Hover ->
            Element.el
                (Element.Events.onMouseLeave onLeave
                    :: style
                )
                (Element.paragraph
                    []
                    [ Element.text text
                    , Components.EditButton.view { onClick = onClickEdit }
                    , case withTrash of
                        Just msg ->
                            Components.IconButtons.iconButton MaterialIcons.remove_circle msg "Radera leden"

                        Nothing ->
                            Element.none
                    ]
                )

        Normal ->
            Element.el
                (Element.Events.onMouseEnter onEnter
                    :: style
                )
                (Element.paragraph
                    []
                    [ Element.text text
                    , Element.text ""
                    ]
                )


viewTextField :
    { text : String
    , onChange : String -> msg
    , onSave : msg
    , onCancel : msg
    , onEnter : msg
    , onLeave : msg
    , onClickEdit : msg
    , mode : Mode
    , style : List (Element.Attribute msg)
    , editFieldWidth : Int
    , withTrash : Maybe msg
    }
    -> Element msg
viewTextField ({ text, onEnter, onLeave, onClickEdit, mode, style, withTrash } as props) =
    case mode of
        Edit ->
            multilineEditor props

        Hover ->
            Element.wrappedRow
                (Element.Events.onMouseLeave onLeave
                    :: style
                )
                [ Element.paragraph []
                    [ Element.text text
                    , Components.EditButton.view { onClick = onClickEdit }
                    , case withTrash of
                        Just msg ->
                            Components.IconButtons.iconButton MaterialIcons.remove_circle msg "Radera leden"

                        Nothing ->
                            Element.none
                    ]
                ]

        Normal ->
            Element.wrappedRow
                (Element.Events.onMouseEnter onEnter
                    :: style
                )
                [ Element.paragraph []
                    [ Element.text text
                    , Element.text ""
                    ]
                ]


multilineEditor : { a | text : String, onChange : String -> msg, onSave : msg, onCancel : msg, editFieldWidth : Int } -> Element msg
multilineEditor { text, onChange, onSave, onCancel, editFieldWidth } =
    Element.column [ Element.width Element.fill ]
        [ Element.Input.multiline [ Element.width (Element.px editFieldWidth) ]
            { onChange = onChange
            , text = text
            , placeholder = Nothing
            , label = Element.Input.labelLeft [] Element.none
            , spellcheck = True
            }
        , Element.row []
            [ Components.IconButtons.commitButton onSave
            , Components.IconButtons.cancelButton onCancel
            ]
        ]


viewInlineTight :
    { text : String
    , onChange : String -> msg
    , onSave : msg
    , onCancel : msg
    , onEnter : msg
    , onLeave : msg
    , onClickEdit : msg
    , mode : Mode
    , style : List (Element.Attribute msg)
    }
    -> Element msg
viewInlineTight { text, onChange, onSave, onCancel, onEnter, onLeave, onClickEdit, mode, style } =
    case mode of
        Edit ->
            viewEdit
                { onChange = onChange
                , onSave = onSave
                , onCancel = onCancel
                , text = text
                }

        Hover ->
            Element.el
                (Element.Events.onMouseLeave onLeave
                    :: style
                )
                (Element.paragraph
                    [ Element.inFront
                        (Element.el
                            [ Element.moveUp 5
                            , Element.moveLeft 15
                            ]
                            (Components.EditButton.view { onClick = onClickEdit })
                        )
                    ]
                    [ Element.text text
                    ]
                )

        Normal ->
            Element.el
                (Element.Events.onMouseEnter onEnter
                    :: style
                )
                (Element.paragraph
                    []
                    [ Element.text text
                    , Element.text ""
                    ]
                )
