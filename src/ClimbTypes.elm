module ClimbTypes exposing (Crag, Route, Sector)


type alias Crag =
    { id : Int
    , name : String
    , createdOn : String
    , desc : String
    , directions : String
    , access : String
    , sectors : List Sector
    }


type alias Sector =
    { name : String
    , desc : String
    , images : List String
    , routes : List Route
    }


type alias Route =
    { name : String
    , grade : String
    , type_ : String
    , fa : String
    , desc : String
    }
