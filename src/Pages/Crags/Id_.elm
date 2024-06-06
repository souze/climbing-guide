module Pages.Crags.Id_ exposing (Model, Msg, page)

import Bridge
import ClimbTypes exposing (Crag, Route, Sector)
import Components.EditButton
import Components.IconButtons exposing (iconButton)
import Components.TextButtons
import Components.TitleTextEdit
import Effect exposing (Effect)
import Element exposing (Element, el)
import Element.Background
import Element.Border
import Element.Events
import Element.Font as Font
import Element.Input
import Html
import Lamdera exposing (sendToBackend)
import List.Extra
import Material.Icons as MaterialIcons
import Material.Icons.Types exposing (Coloring(..))
import Page exposing (Page)
import Route as PageRoute
import Shared
import View exposing (View)
import Widget
import Widget.Icon as Icon
import Widget.Material as Material


page : Shared.Model -> PageRoute.Route { id : String } -> Page Model Msg
page shared route =
    Page.new
        { init = init
        , update = update (getMaybeCragIntTuple route.params.id shared.crags)
        , subscriptions = subscriptions
        , view = view shared.crags (route.params.id |> String.toInt |> Maybe.withDefault 0)
        }


getMaybeCragIntTuple : String -> Maybe (List Crag) -> Maybe ( Int, Crag )
getMaybeCragIntTuple id crags =
    id
        |> String.toInt
        |> Maybe.andThen
            (\nid ->
                getCrag crags nid
                    |> Maybe.map (\c -> ( nid, c ))
            )



-- INIT


type alias Model =
    { canEdit : Bool
    , editState : EditState
    }


type EditState
    = NotAllowed
    | NoEdit
    | HoverEdit FieldId
    | Editing FieldId String
    | Rearranging Int (List Route)


type FieldId
    = TitleField
    | CragDescField
    | CragDirectionsField
    | CragAccessField
    | SectorTitleField Int
    | SectorDescField Int
    | SectorImagesField Int
    | RouteNameField RouteId
    | RouteGradeField RouteId
    | RouteTypeField RouteId
    | RouteFaField RouteId
    | RouteDescField RouteId


type alias RouteId =
    { sectorId : Int, routeId : Int }


init : () -> ( Model, Effect Msg )
init () =
    ( { canEdit = True, editState = NotAllowed }
    , Effect.none
    )



-- UPDATE


type Msg
    = NoOp
    | StartEditing
    | StopEditing
    | MouseEnter FieldId
    | MouseLeave FieldId
    | EditField FieldId String
    | FieldTextChange FieldId String
    | EditSave
    | EditCancel
    | RearrangeRoutes Int (List Route)
    | AddRoute Int
    | AddSector
    | GotoNoEdit
    | SaveRouteArrangement
    | RemoveThing FieldId


emptySector : Sector
emptySector =
    { name = "?"
    , desc = ""
    , images = []
    , routes = []
    }


emptyRoute : Route
emptyRoute =
    { name = "?"
    , grade = ""
    , type_ = ""
    , fa = ""
    , desc = ""
    }


update : Maybe ( Int, Crag ) -> Msg -> Model -> ( Model, Effect Msg )
update maybeCrag msg model =
    case maybeCrag of
        Just ( id, crag ) ->
            updateWithCrag ( id, crag ) msg model

        Nothing ->
            ( model, Effect.none )


updateWithCrag : ( Int, Crag ) -> Msg -> Model -> ( Model, Effect Msg )
updateWithCrag ( id, crag ) msg model =
    let
        noop =
            ( model, Effect.none )
    in
    case msg of
        NoOp ->
            ( model, Effect.none )

        StartEditing ->
            ( { model | editState = NoEdit }
            , Effect.none
            )

        StopEditing ->
            ( { model | editState = NotAllowed }
            , Effect.none
            )

        MouseEnter fieldId ->
            case model.editState of
                NoEdit ->
                    ( { model | editState = HoverEdit fieldId }, Effect.none )

                HoverEdit _ ->
                    ( { model | editState = HoverEdit fieldId }, Effect.none )

                _ ->
                    noop

        MouseLeave _ ->
            case model.editState of
                Editing _ _ ->
                    noop

                _ ->
                    ( { model | editState = NoEdit }, Effect.none )

        EditField fieldId initialText ->
            ( { model | editState = Editing fieldId initialText }, Effect.none )

        FieldTextChange fieldId newText ->
            ( { model | editState = Editing fieldId newText }, Effect.none )

        RemoveThing fieldId ->
            case fieldId of
                TitleField ->
                    ( model
                    , Effect.sendCmd <| sendToBackend (Bridge.RemoveCrag id)
                    )

                _ ->
                    let
                        removeFn sectors =
                            case fieldId of
                                RouteDescField { sectorId, routeId } ->
                                    sectors
                                        |> updateSector
                                            sectorId
                                            (\s -> { s | routes = s.routes |> List.Extra.removeAt (routeId - 1) })

                                SectorTitleField sectorId ->
                                    sectors |> List.Extra.removeAt sectorId

                                _ ->
                                    sectors

                        newCrag =
                            { crag
                                | sectors =
                                    crag.sectors |> removeFn
                            }
                    in
                    ( { model | editState = NoEdit }
                    , Effect.sendCmd <|
                        sendToBackend (Bridge.UpdateCrag id newCrag)
                    )

        EditSave ->
            case model.editState of
                Editing fieldId text ->
                    let
                        newCrag =
                            crag |> updateCrag fieldId text
                    in
                    ( { model | editState = NoEdit }
                    , Effect.sendCmd <|
                        sendToBackend (Bridge.UpdateCrag id newCrag)
                    )

                _ ->
                    ( { model | editState = NoEdit }, Effect.none )

        EditCancel ->
            ( { model | editState = NoEdit }, Effect.none )

        RearrangeRoutes sectorId routes ->
            ( { model | editState = Rearranging sectorId routes }, Effect.none )

        GotoNoEdit ->
            ( { model | editState = NoEdit }, Effect.none )

        SaveRouteArrangement ->
            case model.editState of
                Rearranging sectorId newOrderRoutes ->
                    let
                        newCrag =
                            { crag
                                | sectors =
                                    crag.sectors
                                        |> updateSector sectorId (\s -> { s | routes = newOrderRoutes })
                            }
                    in
                    ( { model | editState = NoEdit }
                    , Effect.sendCmd <|
                        sendToBackend (Bridge.UpdateCrag id newCrag)
                    )

                _ ->
                    noop

        AddRoute sectorId ->
            let
                newCrag =
                    { crag
                        | sectors =
                            crag.sectors
                                |> updateSector sectorId (\s -> { s | routes = s.routes ++ [ emptyRoute ] })
                    }
            in
            ( model
            , Effect.sendCmd <|
                sendToBackend (Bridge.UpdateCrag id newCrag)
            )

        AddSector ->
            let
                newCrag =
                    { crag
                        | sectors =
                            crag.sectors ++ [ emptySector ]
                    }
            in
            ( model
            , Effect.sendCmd <|
                sendToBackend (Bridge.UpdateCrag id newCrag)
            )


updateSector : Int -> (Sector -> Sector) -> List Sector -> List Sector
updateSector sectorId f sectors =
    sectors
        |> List.Extra.updateAt sectorId f


updateCrag : FieldId -> String -> Crag -> Crag
updateCrag fieldId text crag =
    case fieldId of
        TitleField ->
            { crag | name = text }

        CragDescField ->
            { crag | desc = text }

        CragDirectionsField ->
            { crag | directions = text }

        CragAccessField ->
            { crag | access = text }

        SectorTitleField i ->
            { crag | sectors = crag.sectors |> updateSectorTitle i text }

        SectorDescField i ->
            { crag | sectors = crag.sectors |> updateSectorDesc i text }

        SectorImagesField i ->
            { crag | sectors = crag.sectors |> updateSector i (\s -> { s | images = imageListFromString text }) }

        RouteNameField routeId ->
            { crag | sectors = crag.sectors |> updateRoute routeId (\r -> { r | name = text }) }

        RouteGradeField routeId ->
            { crag | sectors = crag.sectors |> updateRoute routeId (\r -> { r | grade = text }) }

        RouteTypeField routeId ->
            { crag | sectors = crag.sectors |> updateRoute routeId (\r -> { r | type_ = text }) }

        RouteFaField routeId ->
            { crag | sectors = crag.sectors |> updateRoute routeId (\r -> { r | fa = text }) }

        RouteDescField routeId ->
            { crag | sectors = crag.sectors |> updateRoute routeId (\r -> { r | desc = text }) }


imageListFromString : String -> List String
imageListFromString text =
    text
        |> String.split "\n"
        |> List.map String.trim


updateRoute : RouteId -> (Route -> Route) -> List Sector -> List Sector
updateRoute routeId f sectors =
    sectors
        |> List.Extra.updateAt routeId.sectorId
            (\sector ->
                { sector
                    | routes =
                        sector.routes
                            |> List.Extra.updateAt (routeId.routeId - 1) f
                }
            )


updateSectorTitle : Int -> String -> List Sector -> List Sector
updateSectorTitle i text sectors =
    sectors
        |> List.Extra.updateAt i (\sector -> { sector | name = text })


updateSectorDesc : Int -> String -> List Sector -> List Sector
updateSectorDesc i text sectors =
    sectors
        |> List.Extra.updateAt i (\sector -> { sector | desc = text })



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


placeholderImage : String
placeholderImage =
    "https://img.freepik.com/free-vector/illustration-gallery-icon_53876-27002.jpg?w=740&t=st=1717778335~exp=1717778935~hmac=f4bcb440b1df493f83fea348891dd7020b268f28206b13d636851172b90c43b4"


titleStyle : List (Element.Attr () msg)
titleStyle =
    [ Font.color (Element.rgb 0.1 0.1 0.1)
    , Font.size 32
    , Font.family [ Font.typeface "Open Sans", Font.sansSerif ]
    ]


routeNameStyle : List (Element.Attr () msg)
routeNameStyle =
    [ Font.color (Element.rgb 0.8 0.3 0.3)
    , Font.family [ Font.typeface "Open Sans", Font.sansSerif ]
    , Element.width Element.fill
    ]


view : Maybe (List Crag) -> Int -> Model -> View Msg
view maybeCrags id model =
    case getCrag maybeCrags id of
        Just crag ->
            View "Valberget" [] (viewBody model crag)

        Nothing ->
            View "Klippan kunde inte hittas" [] (Element.text "Kunde inte hitta klippan")


getCrag : Maybe (List Crag) -> Int -> Maybe Crag
getCrag maybeCrags id =
    maybeCrags |> Maybe.andThen (List.Extra.find (\crag -> crag.id == id))


viewBody : Model -> Crag -> Element Msg
viewBody model crag =
    Element.column
        [ Element.padding 50
        , Element.spacing 20
        , Element.width (Element.px 900)
        ]
        [ editableInlineWithTrash titleStyle crag.name TitleField model.editState
        , if model.editState == NotAllowed && model.canEdit then
            Components.TextButtons.textButton "Gå in i redigeringsläge" StartEditing

          else if model.editState /= NotAllowed && model.canEdit then
            Components.TextButtons.textButton "Gå ur redigeringsläge" StopEditing

          else
            Element.none
        , Element.text crag.createdOn
        , title "Allmän information"
        , editableTextField crag.desc "Ingen beskrivning än..." CragDescField model.editState 800
        , title "Vägbeskrivning"
        , editableTextField crag.directions "Ingen vägbeskrivning än..." CragDirectionsField model.editState 800
        , title "Access"
        , editableTextField crag.access "Ingen accessinfo" CragAccessField model.editState 800
        , title "Leder"
        , viewSectors model.editState crag.sectors
        ]


editableTextFieldWithTrash : String -> String -> FieldId -> EditState -> Int -> Element Msg
editableTextFieldWithTrash =
    editableTextFieldWithOptions True


editableTextField : String -> String -> FieldId -> EditState -> Int -> Element Msg
editableTextField =
    editableTextFieldWithOptions False


editableTextFieldWithOptions : Bool -> String -> String -> FieldId -> EditState -> Int -> Element Msg
editableTextFieldWithOptions withTrash text backupText fieldId editState editFieldWidth =
    let
        visText =
            text |> fillEmpty backupText
    in
    Components.TitleTextEdit.viewTextField
        { onChange = FieldTextChange fieldId
        , onSave = EditSave
        , onCancel = EditCancel
        , text =
            case editState of
                Editing editFieldId currentText ->
                    if editFieldId == fieldId then
                        currentText

                    else
                        visText

                _ ->
                    visText
        , onEnter = MouseEnter fieldId
        , onLeave = MouseLeave fieldId
        , onClickEdit = EditField fieldId text
        , mode =
            case editState of
                Editing editFieldId _ ->
                    if editFieldId == fieldId then
                        Components.TitleTextEdit.Edit

                    else
                        Components.TitleTextEdit.Normal

                HoverEdit hoverFieldId ->
                    if hoverFieldId == fieldId then
                        Components.TitleTextEdit.Hover

                    else
                        Components.TitleTextEdit.Normal

                _ ->
                    Components.TitleTextEdit.Normal
        , style = []
        , editFieldWidth = editFieldWidth
        , withTrash =
            if withTrash then
                Just (RemoveThing fieldId)

            else
                Nothing
        }


editableInlineTight : List (Element.Attribute Msg) -> String -> FieldId -> EditState -> Element Msg
editableInlineTight style text fieldId editState =
    let
        visText =
            if String.trim text == "" then
                " - "

            else
                text
    in
    Components.TitleTextEdit.viewInlineTight
        { onChange = FieldTextChange fieldId
        , onSave = EditSave
        , onCancel = EditCancel
        , text =
            case editState of
                Editing editFieldId currentText ->
                    if editFieldId == fieldId then
                        currentText

                    else
                        visText

                _ ->
                    visText
        , onEnter = MouseEnter fieldId
        , onLeave = MouseLeave fieldId
        , onClickEdit = EditField fieldId text
        , mode =
            case editState of
                Editing editFieldId _ ->
                    if editFieldId == fieldId then
                        Components.TitleTextEdit.Edit

                    else
                        Components.TitleTextEdit.Normal

                HoverEdit hoverFieldId ->
                    if hoverFieldId == fieldId then
                        Components.TitleTextEdit.Hover

                    else
                        Components.TitleTextEdit.Normal

                _ ->
                    Components.TitleTextEdit.Normal
        , style = style
        }


editableInlineWithTrash : List (Element.Attribute Msg) -> String -> FieldId -> EditState -> Element Msg
editableInlineWithTrash =
    editableInlineWithOptions True


editableInline : List (Element.Attribute Msg) -> String -> FieldId -> EditState -> Element Msg
editableInline =
    editableInlineWithOptions False


editableInlineWithOptions : Bool -> List (Element.Attribute Msg) -> String -> FieldId -> EditState -> Element Msg
editableInlineWithOptions withTrash style text fieldId editState =
    let
        visText =
            if String.trim text == "" then
                " - "

            else
                text
    in
    Components.TitleTextEdit.viewTitle
        { onChange = FieldTextChange fieldId
        , onSave = EditSave
        , onCancel = EditCancel
        , text =
            case editState of
                Editing editFieldId currentText ->
                    if editFieldId == fieldId then
                        currentText

                    else
                        visText

                _ ->
                    visText
        , onEnter = MouseEnter fieldId
        , onLeave = MouseLeave fieldId
        , onClickEdit = EditField fieldId text
        , mode =
            case editState of
                Editing editFieldId _ ->
                    if editFieldId == fieldId then
                        Components.TitleTextEdit.Edit

                    else
                        Components.TitleTextEdit.Normal

                HoverEdit hoverFieldId ->
                    if hoverFieldId == fieldId then
                        Components.TitleTextEdit.Hover

                    else
                        Components.TitleTextEdit.Normal

                _ ->
                    Components.TitleTextEdit.Normal
        , style = style
        , withTrash =
            if withTrash then
                Just (RemoveThing fieldId)

            else
                Nothing
        }


editableTitle : String -> FieldId -> EditState -> Element Msg
editableTitle =
    editableInline titleStyle


title : String -> Element Msg
title text =
    Element.el titleStyle (Element.text text)


viewSectors : EditState -> List Sector -> Element Msg
viewSectors editState sectors =
    Element.column [ Element.spacing 30 ]
        (List.indexedMap (viewSector editState) sectors
            ++ [ if editState /= NotAllowed then
                    iconButton MaterialIcons.library_add AddSector "Lägg till sektor"

                 else
                    Element.none
               ]
        )


viewSector : EditState -> Int -> Sector -> Element Msg
viewSector editState i sector =
    Element.column [ Element.spacing 30 ]
        [ editableInlineWithTrash titleStyle (sector.name |> fillEmpty "Namnlös") (SectorTitleField i) editState
        , editableTextField sector.desc "Ingen beskrivning än..." (SectorDescField i) editState 800
        , viewSectorImages editState i sector.images
        , viewRoutes editState i sector.routes
        ]


viewSectorImages : EditState -> Int -> List String -> Element Msg
viewSectorImages editState sectorId origUrls =
    let
        urls =
            if List.length origUrls == 0 && editState /= NotAllowed then
                [ placeholderImage ]

            else
                origUrls
    in
    case isEditing (SectorImagesField sectorId) editState of
        Just text ->
            Components.TitleTextEdit.multilineEditor
                { editFieldWidth = 600
                , onCancel = EditCancel
                , onChange = FieldTextChange (SectorImagesField sectorId)
                , onSave = EditSave
                , text = text
                }

        Nothing ->
            Element.column []
                ((urls |> List.map viewSectorImage)
                    ++ [ if editState /= NotAllowed then
                            iconButton
                                MaterialIcons.edit
                                (EditField (SectorImagesField sectorId) (origUrls |> urlsToText))
                                "Editera sektorns bilder"

                         else
                            Element.none
                       ]
                )


isEditing : FieldId -> EditState -> Maybe String
isEditing fieldId editState =
    case editState of
        Editing editingId text ->
            if editingId == fieldId then
                Just text

            else
                Nothing

        _ ->
            Nothing


urlsToText : List String -> String
urlsToText urls =
    String.join "\n" urls


viewSectorImage : String -> Element msg
viewSectorImage url =
    Element.image [ Element.height (Element.px 350) ] { src = url, description = "Sektor bild" }


fillEmpty : String -> String -> String
fillEmpty replacement original =
    if String.trim original == "" then
        replacement

    else
        original


viewRoutes : EditState -> Int -> List Route -> Element Msg
viewRoutes editState sectorId routes =
    case editState of
        Rearranging rearrangeSectorId rearrangeRoutes ->
            if rearrangeSectorId == sectorId then
                viewRoutesRearrange sectorId rearrangeRoutes

            else
                viewRoutesPlain editState sectorId routes

        _ ->
            viewRoutesPlain editState sectorId routes


type Direction
    = DirUp
    | DirDown


viewRoutesRearrange : Int -> List Route -> Element Msg
viewRoutesRearrange sectorId routes =
    Element.column
        [ Element.Background.color (Element.rgb 0.95 0.95 0.95)
        , Element.width Element.fill
        ]
        ((routes
            |> List.indexedMap
                (\i r ->
                    viewRouteRearrange
                        i
                        r
                        { upMsg = RearrangeRoutes sectorId (routes |> switchUp i)
                        , downMsg = RearrangeRoutes sectorId (routes |> switchUp (i + 1))
                        }
                )
         )
            ++ [ Element.row []
                    [ Components.IconButtons.commitButton SaveRouteArrangement
                    , Components.IconButtons.cancelButton GotoNoEdit
                    ]
               ]
        )


switchUp : Int -> List Route -> List Route
switchUp i routes =
    routes |> List.Extra.swapAt i (i - 1)


viewRouteRearrange : Int -> Route -> { upMsg : Msg, downMsg : Msg } -> Element Msg
viewRouteRearrange num route { upMsg, downMsg } =
    Element.row [ Element.spacing 10 ]
        [ Element.text
            (String.fromInt (num + 1) ++ ". " ++ route.name)
        , dirButton DirUp upMsg
        , dirButton DirDown downMsg
        ]


dirButton : Direction -> msg -> Element msg
dirButton dir msg =
    case dir of
        DirUp ->
            Widget.iconButton
                (Material.iconButton Material.defaultPalette)
                { icon = MaterialIcons.arrow_circle_up |> Icon.elmMaterialIcons Color
                , onPress = Just msg
                , text = "Move up"
                }

        DirDown ->
            Widget.iconButton
                (Material.iconButton Material.defaultPalette)
                { icon = MaterialIcons.arrow_circle_down |> Icon.elmMaterialIcons Color
                , onPress = Just msg
                , text = "Move down"
                }


viewRoutesPlain : EditState -> Int -> List Route -> Element Msg
viewRoutesPlain editState sectorId routes =
    let
        splitAt =
            ceiling (toFloat (List.length routes) / 2)

        left =
            List.take splitAt routes

        right =
            List.drop splitAt routes
    in
    Element.column
        [ Element.Background.color (Element.rgb 0.95 0.95 0.95)
        , Element.width (Element.px 700)
        ]
        [ Element.row
            [ Element.width Element.fill ]
            [ viewRouteColumn sectorId editState 1 left
            , viewRouteColumn sectorId editState (splitAt + 1) right
            ]
        , if editState /= NotAllowed then
            Element.row []
                [ Widget.iconButton
                    (Material.iconButton Material.defaultPalette)
                    { icon = MaterialIcons.reorder |> Icon.elmMaterialIcons Color
                    , onPress = Just (RearrangeRoutes sectorId routes)
                    , text = "Rearrange"
                    }
                , Widget.iconButton
                    (Material.iconButton Material.defaultPalette)
                    { icon = MaterialIcons.library_add |> Icon.elmMaterialIcons Color
                    , onPress = Just (AddRoute sectorId)
                    , text = "Add route"
                    }
                ]

          else
            Element.none
        ]


viewRouteColumn : Int -> EditState -> Int -> List Route -> Element Msg
viewRouteColumn sectorId editState startIndex routes =
    Element.column
        [ Element.spacing 13
        , Element.padding 20
        , Element.Border.dotted
        , Element.Border.width 1
        , Element.alignTop
        , Element.width (Element.fillPortion 1)
        ]
        (routes
            |> List.indexedMap (\i r -> viewSingleRoute editState { sectorId = sectorId, routeId = i + startIndex } r)
        )


viewSingleRoute : EditState -> RouteId -> Route -> Element Msg
viewSingleRoute editState routeId route =
    Element.column
        []
        [ Element.row [ Element.spacing 7, Element.width Element.fill ]
            [ Element.text (String.fromInt routeId.routeId ++ ".")
            , editableInlineTight routeNameStyle route.name (RouteNameField routeId) editState
            , editableInlineTight [] route.grade (RouteGradeField routeId) editState
            , Element.row []
                [ Element.text "("
                , editableInlineTight [] route.type_ (RouteTypeField routeId) editState
                , Element.text ")"
                ]
            , Element.text " 7m"
            ]
        , editableInline [] route.fa (RouteFaField routeId) editState
        , editableTextFieldWithTrash
            route.desc
            "Ingen beskrivning än..."
            (RouteDescField routeId)
            editState
            300
        ]
