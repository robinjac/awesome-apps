module Main exposing (Msg(..), main, update, view)

import Browser
import Html exposing (Html, div, option, select, text)
import Html.Attributes exposing (class)


type alias Model =
    { id : Int
    , projects : List Project
    , host_repository : String
    }


type alias BranchData =
    { name : String
    , slug : String
    , date : String
    }


type alias Branches =
    { main : List BranchData
    , release : List BranchData
    , user : List BranchData
    , other : List BranchData
    }


type alias Project =
    { name : String
    , repository : String
    , branches : Branches
    }



-- type alias State =
--     { selectedRepo : String
--     , selectedBranch : BranchName
--     }


type Msg
    = NoOp


type alias BranchNames =
    List String


branchNames : BranchNames
branchNames =
    [ "main"
    , "release"
    , "user"
    , "other"
    ]


init : Model -> ( Model, Cmd Msg )
init model =
    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


main : Program Model Model Msg
main =
    Browser.element { init = init, subscriptions = subscriptions, update = update, view = view }


update : Msg -> Model -> ( Model, Cmd Msg )
update _ model =
    ( model, Cmd.none )


layout : Model -> Html Msg
layout model =
    div [ class "md:container md:mx-auto mt-16 border-solid border-2 border-red-300 rounded-md" ] [ content model ]


content : Model -> Html Msg
content { projects } =
    selectField
        [ availableProjects projects
        , availableBranches branchNames
        ]


availableProjects : List Project -> Html Msg
availableProjects projects =
    projects |> List.map .name |> selectDropdown


availableBranches : BranchNames -> Html Msg
availableBranches branches =
    branches |> selectDropdown


selectDropdown : List String -> Html Msg
selectDropdown items =
    let
        render item =
            option [] [ text item ]
    in
    items |> List.map render |> select []


selectField : List (Html msg) -> Html msg
selectField selects =
    div [ class "flex flex-row" ] selects


view : Model -> Html Msg
view model =
    layout model
