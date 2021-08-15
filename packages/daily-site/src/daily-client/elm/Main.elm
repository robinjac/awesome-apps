module Main exposing (Msg(..), main, update, view)

import Browser
import Dict exposing (Dict)
import Html exposing (Attribute, Html, div, option, select, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class, value)
import Html.Events exposing (onInput)
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


type alias Model =
    { selectedProject : String
    , selectedBranchType : String
    , projects : Dict String Project
    , branches : Branches
    }


type Msg
    = NoOp
    | SelectProject String
    | SelectBranchType String


type BranchType
    = Main
    | Release
    | User
    | Other


branchTypes : List String
branchTypes =
    [ "main", "release", "user", "other" ]


branchTypeToString : BranchType -> String
branchTypeToString branchType =
    case branchType of
        Main ->
            "main"

        Release ->
            "release"

        User ->
            "user"

        Other ->
            "other"


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


branchRecordToDict : BranchRecord -> Dict String Branches
branchRecordToDict branchRecord =
    Dict.fromList [ ( branchTypeToString Main, branchRecord.main ), ( branchTypeToString Release, branchRecord.release ), ( branchTypeToString User, branchRecord.user ), ( branchTypeToString Other, branchRecord.other ) ]


getBranches : String -> BranchRecord -> Branches
getBranches branchType branchRecord =
    case branchType of
        "main" ->
            branchRecord.main

        "release" ->
            branchRecord.release

        "user" ->
            branchRecord.user

        _ ->
            branchRecord.other


init : SiteMetaData -> ( Model, Cmd Msg )
init meta =
    let
        firstProject =
            withDefault defaultProject (List.head meta.projects)

        projects =
            meta.projects |> List.map (\project -> ( project.name, project )) |> Dict.fromList

        model =
            { selectedBranchType = "main", selectedProject = firstProject.name, projects = projects, branches = firstProject.branches.main }
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
        NoOp ->
            ( model, Cmd.none )

        SelectBranchType branchType ->
            let
                currentProject =
                    withDefault defaultProject (Dict.get model.selectedProject model.projects)
            in
            ( { model | selectedBranchType = branchType, branches = getBranches branchType currentProject.branches }, Cmd.none )

        SelectProject project ->
            ( { model | selectedProject = project }, Cmd.none )


layout : Model -> Html Msg
layout model =
    div [ class "md:container md:mx-auto mt-16 p-2 border border-red-300 rounded-md" ]
        [ content model
        , dailyTable model.branches
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


tableContent : Branches -> List (Html Msg)
tableContent branches =
    let
        rowElement rowType data =
            tr [ rowClass rowType ] [ td [] [ text data.name ], td [ class "w-40" ] [ text data.date ], td [ class "w-10 text-center" ] [ text "+" ] ]

        reversed =
            branches |> resolveBranches |> List.reverse

        lastRow =
            List.take 1 reversed |> List.map (rowElement LastRow)

        firstRows =
            List.drop 1 reversed |> List.map (rowElement NotLastRow)
    in
    firstRows ++ lastRow


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


dailyTable : Branches -> Html Msg
dailyTable branches =
    table [ class "mt-8 w-full box-content" ]
        [ thead []
            [ tr [ rowClass NotLastRow ]
                [ th [ class "text-left" ] [ text "Branch" ]
                , th [ class "text-left" ] [ text "Updated" ]
                , th [ class "text-center" ] [ text "Site" ]
                ]
            ]
        , tbody [] (tableContent branches)
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
    items |> List.map render |> select [ handleClick ]


selectField : List (Html msg) -> Html msg
selectField selects =
    div [ class "flex flex-row justify-between border border-red-900 rounded-md p-2 w-60" ] selects


view : Model -> Html Msg
view model =
    layout model
