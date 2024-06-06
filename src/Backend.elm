module Backend exposing (..)

import Bridge exposing (..)
import ClimbTypes exposing (Crag)
import Html
import Lamdera exposing (ClientId, SessionId)
import List.Extra
import Types exposing (BackendModel, BackendMsg(..), ToFrontend(..))


type alias Model =
    BackendModel


app =
    Lamdera.backend
        { init = init
        , update = update
        , updateFromFrontend = updateFromFrontend
        , subscriptions = \m -> Lamdera.onConnect OnConnect
        }



-- Init


initialCrags : List Crag
initialCrags =
    [ { id = 0
      , name = "Valberget"
      , createdOn = "2024-06-06"
      , desc = """
       Valberget är Blekinges största berg, med fin kristallrik granit som kan slita endel på fingertopparna. Berget är som högst ca 30 meter och har både svaiga, vertikala och rejält överhängande delar riktade åt alla väderstreck.
På berget finns i nuläget ett 60-tal leder, allt från 4- till 8- men fokus ligger kring 6 – 7. Det finns potential för fler leder, och även ett antal boulderproblem (se Valberget-Boulder). Klippan har ganska kort säsong några månader på våren, från att vintern tappar sitt tag om berget fram till att knotten och myggen vaknar till liv. På hösten är det ofta väldigt blött på många ställen. Den korta säsongen gör att det inte klättras regelbundet här, vilket leder till att många leder mossar igen, ta med borstar!
       """
      , directions = """
       Från E22 tag av vid avfarten där det är skyltat mot karlshamn sjukhus, kör norrut mot Asarum. Kör rakt fram genom två små rondeller och sväng höger i tredje rondellen mot Asarums centrum. I ”stora korsningen” vid ica och coop i Asarum viker man av höger och fortsätter mot utkanten av samhället. Ta höger mot Granefors och efter ca 100 meter vänster mot Tararp. Strax efter en liten bro ta höger (återigen mot Granefors) och följ vägen genom S-kurvan. Direkt efter kurvan dyker det upp två småvägar till vänster. Ta den högra av dessa, skyltat mot valberget och följ denna ca 1 kilometer till ett tydligt vägskäl. Här är det skyltat vänster till valberget, parkera bilen här, och se till att det finns plats för markägaren att ta sig förbi med skogsmaskiner. Om man är lite planerande kan man nog trycka in 4 bilar här. Fortsätt till fots på vägen fram till den stora vändplats där man brukade parkera. Fortsätt på stigen som är förlängningen av vägen och ta första förgreningen till vänster. Följ sedan stigen av snett åt vänster när du har ett gammalt (numera uppväxt) hygge på vänster sida. Vik undan för undan mer åt höger, bort från ”hygget”. Strax efteråt syns berget tvärs över en blöt sänka. Om ni fortsätter följa stigen så hittar ni spänger över sänkan.
       """
      , access = """Markägaren är positivt inställd till klättring, men vill inte se några fler bultar på berget. Det har dessutom varit problem med bilar på vändplatsen närmast berget, och det är mycket viktigt att parkera bilen i vägskälet innan vändplatsen, som bara ger ca 300 meter längre anmarsch."""
      , sectors =
            []
      }
    ]


init : ( Model, Cmd BackendMsg )
init =
    ( { smashedLikes = 0, crags = initialCrags }
    , Cmd.none
    )


update : BackendMsg -> Model -> ( Model, Cmd BackendMsg )
update msg model =
    case msg of
        OnConnect sid cid ->
            ( model
            , Lamdera.sendToFrontend cid <|
                AllCrags model.crags
            )


updateFromFrontend : SessionId -> ClientId -> ToBackend -> Model -> ( Model, Cmd BackendMsg )
updateFromFrontend sessionId clientId msg model =
    case msg of
        SmashedLikeButton ->
            let
                newSmashedLikes =
                    model.smashedLikes + 1
            in
            ( { model | smashedLikes = newSmashedLikes }, Lamdera.broadcast <| NewSmashedLikes newSmashedLikes )

        UpdateCrag id crag ->
            let
                newModel =
                    { model | crags = model.crags |> replaceCrag id crag }
            in
            ( newModel
            , Lamdera.sendToFrontend clientId (AllCrags newModel.crags)
            )

        RemoveCrag id ->
            let
                newCrags =
                    model.crags |> List.filter (\c -> c.id /= id)
            in
            ( { model | crags = newCrags }
            , Lamdera.sendToFrontend clientId (AllCrags newCrags)
            )

        AddCrag ->
            let
                newCrags =
                    model.crags ++ [ createNewCrag model.crags ]
            in
            ( { model | crags = newCrags }
            , Lamdera.sendToFrontend clientId (AllCrags newCrags)
            )


createNewCrag : List Crag -> Crag
createNewCrag currentCrags =
    { id = newCragId currentCrags
    , name = ""
    , createdOn = ""
    , desc = ""
    , directions = ""
    , access = ""
    , sectors = []
    }


newCragId : List Crag -> Int
newCragId currentCrags =
    currentCrags
        |> List.map .id
        |> List.maximum
        |> Maybe.withDefault -1
        |> (\id -> id + 1)


replaceCrag : Int -> Crag -> List Crag -> List Crag
replaceCrag id newCrag crags =
    crags
        |> List.map
            (\crag ->
                if crag.id == id then
                    newCrag

                else
                    crag
            )
