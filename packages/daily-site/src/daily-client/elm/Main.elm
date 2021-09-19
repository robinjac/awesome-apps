module Main exposing (Msg(..), main, update, view)

import Browser
import Dict exposing (Dict)
import Html exposing (Attribute, Html, div, li, nav, option, select, table, tbody, td, text, th, thead, tr, ul)
import Html.Attributes exposing (class, colspan, value)
import Html.Events exposing (onClick, onInput)
import Maybe exposing (withDefault)


type alias SiteMetaData =
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
    List (Maybe BranchData)


type alias Project =
    { name : String
    , repository : String
    , branches : BranchRecord
    }


type alias BranchRecord =
    { main : Branches
    , release : Branches
    , user : Branches
    , other : Branches
    }


type Page
    = Start
    | End
    | Prev
    | Next


type alias Model =
    { selectedProject : String
    , selectedBranchType : String
    , projects : Dict String Project
    , branches : Branches
    , currentPageIndex : Int
    }


type Msg
    = SelectProject String
    | SelectBranchType String
    | Pagination Page


branchTypes : List String
branchTypes =
    [ "main", "release", "user", "other" ]


defaultProject : Project
defaultProject =
    { name = "N/A"
    , repository = "N/A"
    , branches =
        { main = []
        , release = []
        , user = []
        , other = []
        }
    }


maxRows : Int
maxRows =
    10


getBranchesByType : String -> BranchRecord -> Branches
getBranchesByType branchType branchRecord =
    case branchType of
        "main" ->
            branchRecord.main

        "release" ->
            branchRecord.release

        "user" ->
            branchRecord.user

        _ ->
            branchRecord.other


getBranches : Dict String Project -> String -> String -> Branches
getBranches projects selectedProject selectedBranchType =
    projects
        |> Dict.get selectedProject
        |> withDefault defaultProject
        |> .branches
        |> getBranchesByType selectedBranchType


init : SiteMetaData -> ( Model, Cmd Msg )
init meta =
    let
        firstProject =
            withDefault defaultProject (List.head meta.projects)

        projects =
            meta.projects |> List.map (\project -> ( project.name, project )) |> Dict.fromList

        model =
            { selectedBranchType = "main"
            , selectedProject = firstProject.name
            , projects = projects
            , branches = firstProject.branches.main
            , currentPageIndex = 0
            }
    in
    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


main : Program SiteMetaData Model Msg
main =
    Browser.element { init = init, subscriptions = subscriptions, update = update, view = view }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectBranchType branchType ->
            ( { model | selectedBranchType = branchType, branches = getBranches model.projects model.selectedProject branchType, currentPageIndex = 0 }, Cmd.none )

        SelectProject project ->
            ( { model | selectedProject = project, branches = getBranches model.projects project model.selectedBranchType, currentPageIndex = 0 }, Cmd.none )

        Pagination page ->
            let
                maxIndex =
                    List.length model.branches // maxRows

                nextPageIndex =
                    case page of
                        Start ->
                            0

                        Prev ->
                            if model.currentPageIndex == 0 then
                                model.currentPageIndex

                            else
                                model.currentPageIndex - 1

                        Next ->
                            if model.currentPageIndex >= maxIndex then
                                model.currentPageIndex

                            else
                                model.currentPageIndex + 1

                        End ->
                            maxIndex
            in
            ( { model | currentPageIndex = nextPageIndex }, Cmd.none )


layout : Model -> Html Msg
layout model =
    let
        rowElement rowType data =
            tr [ rowClass rowType ] [ td [] [ text data.name ], td [ class "w-40" ] [ text data.date ], td [ class "w-10 text-center" ] [ text "+" ] ]

        reversed =
            model.branches |> resolveBranches |> List.reverse

        visibleRows =
            reversed
                |> List.drop (model.currentPageIndex * maxRows)
                |> List.take maxRows

        lastRow =
            List.take 1 visibleRows |> List.map (rowElement LastRow)

        firstRows =
            List.drop 1 visibleRows |> List.map (rowElement NotLastRow)

        rows =
            firstRows ++ lastRow
    in
    div [ class "md:container md:mx-auto mt-16 p-2 border min-h-664 border-red-300 rounded-md flex flex-col justify-self-start" ]
        [ content model
        , dailyTable rows
        , pagination model.currentPageIndex (List.length visibleRows) (List.length reversed)
        ]


type RowType
    = LastRow
    | NotLastRow


rowClass : RowType -> Attribute msg
rowClass row =
    case row of
        LastRow ->
            class "h-12"

        NotLastRow ->
            class "h-12 border-b"


tableContent : List (Html Msg) -> List (Html Msg)
tableContent rows =
    if List.length rows == 0 then
        [ tr [ rowClass LastRow ] [ td [ colspan 3, class "text-center" ] [ text "no branches" ] ] ]

    else
        rows


resolveBranches : Branches -> List BranchData
resolveBranches branches =
    let
        defaultBranchData =
            { name = "N/A"
            , slug = "N/A"
            , date = "N/A"
            }
    in
    List.map (\branchData -> withDefault defaultBranchData branchData) branches


dailyTable : List (Html Msg) -> Html Msg
dailyTable rows =
    table [ class "mt-8 w-full box-content" ]
        [ thead []
            [ tr [ rowClass NotLastRow ]
                [ th [ class "text-left" ] [ text "Branch" ]
                , th [ class "text-left w-40" ] [ text "Updated" ]
                , th [ class "text-center w-10" ] [ text "Site" ]
                ]
            ]
        , tbody [] (tableContent rows)
        ]


content : Model -> Html Msg
content model =
    selectField
        [ availableProjects model
        , availableBranches
        ]


availableProjects : Model -> Html Msg
availableProjects model =
    Dict.keys model.projects |> selectDropdown (onInput SelectProject)


availableBranches : Html Msg
availableBranches =
    branchTypes |> selectDropdown (onInput SelectBranchType)


selectDropdown : Attribute Msg -> List String -> Html Msg
selectDropdown handleClick items =
    let
        render item =
            option [ value item ] [ text item ]
    in
    items |> List.map render |> select [ class "cursor-pointer", handleClick ]


selectField : List (Html msg) -> Html msg
selectField selects =
    div [ class "flex flex-row justify-between border border-red-900 rounded-md p-2 w-60 select-none" ] selects


shouldDisable : Bool -> String
shouldDisable bool =
    if bool then
        "pointer-events-none opacity-40"

    else
        ""


pagination : Int -> Int -> Int -> Html Msg
pagination page rows pages =
    let
        visablePages =
            page * maxRows

        liClasses =
            "border border-red-700 rounded-md mr-1 text-center leading-8 w-8 h-8 hover:bg-red-200 cursor-pointer select-none"
    in
    nav [ class "w-full flex justify-between mt-auto" ]
        [ div [ class "flex items-center ml-2" ] [ text (Debug.toString (1 + visablePages) ++ "-" ++ Debug.toString (rows + visablePages)), text (" of " ++ Debug.toString pages) ]
        , ul
            [ class "flex list-none mr-2" ]
            [ li [ class (liClasses ++ shouldDisable (page == 0)), onClick (Pagination Start) ] [ text "|<" ]
            , li [ class (liClasses ++ shouldDisable (page == 0)), onClick (Pagination Prev) ] [ text "<" ]
            , li [ class (liClasses ++ shouldDisable (1 + visablePages + maxRows >= pages)), onClick (Pagination Next) ] [ text ">" ]
            , li [ class (liClasses ++ shouldDisable (1 + visablePages + maxRows >= pages)), onClick (Pagination End) ] [ text ">|" ]
            ]
        ]


view : Model -> Html Msg
view model =
    layout model
