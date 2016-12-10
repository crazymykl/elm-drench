module Main exposing (..)

import String
import Array exposing (Array)
import Random exposing (Generator)
import Grid exposing (..)
import Html exposing (..)
import Html.Attributes exposing (class, style, rel, href, type_, value)
import Html.Events exposing (onClick, onInput)


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
    | SetX Int
    | SetY Int


type alias Model =
    { board : Board
    , moveCount : Int
    , newSize : Point
    }


main : Program Never Model Msg
main =
    program { init = init, view = view, update = update, subscriptions = always Sub.none }


init : ( Model, Cmd Msg )
init =
    newModel ( 20, 20 ) Grid.empty ! [ randomBoard ( 20, 20 ) ]


view : Model -> Html Msg
view model =
    let
        colorButtons color =
            button [ onClick <| Advance color ] [ text <| toString color ]

        arrMap f xs =
            Array.map f xs |> Array.toList

        viewRow row =
            div [ class "row" ] <| arrMap viewCell row

        viewCell cell =
            span
                [ style [ ( "background-color", toString cell ) ]
                , class "cell"
                ]
                []

        ( x, y ) =
            model.newSize

        numInput msg =
            String.toInt >> Result.withDefault 0 >> msg |> onInput
    in
        main_ []
            [ css "style.css"
            , div [ class "buttons" ] <| List.map colorButtons colors
            , div [ class "grid" ] <| arrMap viewRow model.board
            , text <| "Moves: " ++ toString model.moveCount
            , span [ class "new-game" ]
                [ text "Rows: "
                , input [ type_ "number", value <| toString x, numInput SetX ] []
                , text "Cols: "
                , input [ type_ "number", value <| toString y, numInput SetY ] []
                , button [ onClick RandomBoard ] [ text "New Game" ]
                ]
            ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Advance color ->
            advance color model ! []

        SetBoard board ->
            newModel model.newSize board ! []

        RandomBoard ->
            model ! [ randomBoard model.newSize ]

        SetX x ->
            let
                ( old_x, y ) =
                    model.newSize
            in
                { model | newSize = ( x, y ) } ! []

        SetY y ->
            let
                ( x, old_y ) =
                    model.newSize
            in
                { model | newSize = ( x, y ) } ! []


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


cascade : Color -> Color -> Point -> Board -> Board
cascade color newColor point board =
    List.foldl (recolor color newColor) board <| neighbors point


recolor : Color -> Color -> Point -> Board -> Board
recolor color newColor point board =
    let
        myColor =
            get2d point board

        nextBoard =
            set2d point newColor board
    in
        if myColor == Just color then
            cascade color newColor point nextBoard
        else
            board


sample : Array a -> Generator (Maybe a)
sample arr =
    let
        gen =
            Random.int 0 <| Array.length arr - 1
    in
        Random.map (flip Array.get arr) gen


colors : List Color
colors =
    [ Red, Green, Blue, Purple, Yellow ]


randomColor : Generator Color
randomColor =
    Random.map (Maybe.withDefault Red) <| sample <| Array.fromList colors


randomBoard : Point -> Cmd Msg
randomBoard ( x, y ) =
    Random.generate SetBoard <| randomGrid x y randomColor
