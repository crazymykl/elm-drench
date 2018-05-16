module Main exposing (main)

import Browser exposing (fullscreen)
import Cmd.Extra exposing (withCmd, withNoCmd)
import Grid exposing (Grid, Point, get2d, neighbors, set2d)
import Html exposing (Html, button, div, input, main_, node, span, text)
import Html.Attributes exposing (class, href, rel, style, type_, value)
import Html.Events exposing (onClick, onInput)
import Random exposing (Generator)


type alias Board =
    Grid Color


type Color
    = Red
    | Green
    | Blue
    | Purple
    | Yellow


type Msg
    = RandomBoard
    | SetBoard Board
    | Advance Color
    | SetSize Point


type alias Model =
    { board : Board
    , moveCount : Int
    , newSize : Point
    }


main : Program () Model Msg
main =
    fullscreen { init = init, view = view, update = update, subscriptions = always Sub.none, onNavigation = Nothing }


init : Browser.Env a -> ( Model, Cmd Msg )
init { url } =
    newModel ( 20, 20 ) Grid.empty
        |> withCmd (randomBoard ( 20, 20 ))


view : Model -> Browser.Page Msg
view model =
    { title = "elm-drench", body = [ body model ] }


body : Model -> Html Msg
body model =
    let
        colorButtons color =
            button [ onClick <| Advance color ] [ text <| colorString color ]

        viewRow =
            div [ class "row" ] << List.map viewCell

        viewCell cell =
            span
                [ style "background-color" <| colorString cell
                , class "cell"
                ]
                []

        ( x, y ) =
            model.newSize

        numInput msg =
            String.toInt >> Maybe.withDefault 0 >> msg |> onInput
    in
    main_ []
        [ css "style.css"
        , div [ class "buttons" ] <| List.map colorButtons [ Red, Green, Blue, Purple, Yellow ]
        , div [ class "grid" ] <| List.map viewRow (Grid.toLists model.board)
        , text <| "Moves: " ++ String.fromInt model.moveCount
        , span [ class "new-game" ]
            [ text "Rows: "
            , input [ type_ "number", value <| String.fromInt x, numInput (\x_ -> SetSize ( x_, y )) ] []
            , text "Cols: "
            , input [ type_ "number", value <| String.fromInt y, numInput (\y_ -> SetSize ( x, y_ )) ] []
            , button [ onClick RandomBoard ] [ text "New Game" ]
            ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Advance color ->
            advance color model |> withNoCmd

        SetBoard board ->
            newModel model.newSize board |> withNoCmd

        RandomBoard ->
            model |> withCmd (randomBoard model.newSize)

        SetSize pt ->
            { model | newSize = pt } |> withNoCmd


newModel : Point -> Board -> Model
newModel newSize board =
    { board = board
    , moveCount = 0
    , newSize = newSize
    }


advance : Color -> Model -> Model
advance color model =
    let
        newBoard =
            makeMove color model.board
    in
    if model.board == newBoard then
        model

    else
        { model
            | board = newBoard
            , moveCount = model.moveCount + 1
        }


makeMove : Color -> Board -> Board
makeMove color board =
    let
        oldColor =
            Maybe.withDefault color <| get2d ( 0, 0 ) board
    in
    if color == oldColor then
        board

    else
        recolor oldColor color ( 0, 0 ) board


css : String -> Html a
css path =
    node "link" [ rel "stylesheet", href path ] []


recolor : Color -> Color -> Point -> Board -> Board
recolor oldColor color point board =
    neighbors point
        |> List.filter (\p -> get2d p board == Just oldColor)
        |> List.foldl (recolor oldColor color) (set2d point color board)


colorString : Color -> String
colorString color =
    case color of
        Red ->
            "Red"

        Green ->
            "Green"

        Blue ->
            "Blue"

        Purple ->
            "Purple"

        Yellow ->
            "Yellow"


randomColor : Generator Color
randomColor =
    Random.uniform Red [ Green, Blue, Purple, Yellow ]


randomBoard : Point -> Cmd Msg
randomBoard ( x, y ) =
    Random.generate SetBoard <| Grid.random x y randomColor
