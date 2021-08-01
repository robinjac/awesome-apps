module Main exposing (Msg(..), main, update, view)

import Browser
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)


type Msg
    = Increment
    | Decrement


type alias Model =
    Int


init : Model
init =
    0


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }


update : Msg -> Int -> Int
update msg model =
    case msg of
        Increment ->
            model + 1

        Decrement ->
            model - 1


layout : Html Msg -> Html Msg
layout html =
    div [ class "md:container md:mx-auto mt-16 border-solid border-2 border-red-300 rounded-md" ] [ html ]


incrementer : Int -> Html Msg
incrementer model =
    div []
        [ button [ onClick Decrement ] [ text "-" ]
        , div [] [ text (String.fromInt model) ]
        , button [ onClick Increment ] [ text "+" ]
        ]


view : Int -> Html Msg
view model =
    model |> incrementer |> layout
