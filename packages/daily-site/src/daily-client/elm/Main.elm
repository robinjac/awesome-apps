module Main exposing (Msg(..), main, update, view)

import Browser
import Html exposing (Html, div, h1, text)
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


type Msg
    = NoOp


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
    model
        |> content
        |> div [ class "md:container md:mx-auto mt-16 border-solid border-2 border-red-300 rounded-md" ]


content : Model -> List (Html Msg)
content model =
    List.map header model.projects


header : Project -> Html Msg
header project =
    div [] [ h1 [] [ text project.name ] ]


view : Model -> Html Msg
view model =
    layout model
