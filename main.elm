module Main exposing (main)
import Array exposing (Array)
import Browser exposing (element)
import Grid exposing (..)
import Html exposing (..)
import Html.Attributes exposing (class, href, rel, style, type_, value)
import Html.Events exposing (onClick, onInput)
import Random exposing (Generator)
import String


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


main : Program () Model Msg
main =
    element { init = always init, view = view, update = update, subscriptions = always Sub.none }


init : ( Model, Cmd Msg )
init =
    ( newModel ( 20, 20 ) Grid.empty
    , randomBoard ( 20, 20 )
    )


view : Model -> Html Msg
view model =
    let
        colorButtons color =
            button [ onClick <| Advance color ] [ text <| colorName color ]

        arrMap f xs =
            Array.map f xs |> Array.toList

        viewRow row =
            div [ class "row" ] <| arrMap viewCell row

        viewCell cell =
            span
                [ style "background-color" (colorName cell)
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
        , div [ class "buttons" ] <| List.map colorButtons colors
        , div [ class "grid" ] <| arrMap viewRow model.board
        , text <| "Moves: " ++ String.fromInt model.moveCount
        , span [ class "new-game" ]
            [ text "Rows: "
            , input [ type_ "number", value <| String.fromInt x, numInput SetX ] []
            , text "Cols: "
            , input [ type_ "number", value <| String.fromInt y, numInput SetY ] []
            , button [ onClick RandomBoard ] [ text "New Game" ]
            ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Advance color ->
            ( advance color model
            , Cmd.none
            )

        SetBoard board ->
            ( newModel model.newSize board
            , Cmd.none
            )

        RandomBoard ->
            ( model
            , randomBoard model.newSize
            )

        SetX x ->
            let
                ( _, y ) =
                    model.newSize
            in
            ( { model | newSize = ( x, y ) }
            , Cmd.none
            )

        SetY y ->
            let
                ( x, _ ) =
                    model.newSize
            in
            ( { model | newSize = ( x, y ) }
            , Cmd.none
            )


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


sample : Array a -> Generator (Maybe a)
sample arr =
    let
        gen =
            Random.int 0 <| Array.length arr - 1
    in
    Random.map (\a -> Array.get a arr) gen


colors : List Color
colors =
    [ Red, Green, Blue, Purple, Yellow ]


colorName : Color -> String
colorName color =
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
    Random.map (Maybe.withDefault Red) <| sample <| Array.fromList colors


randomBoard : Point -> Cmd Msg
randomBoard ( x, y ) =
    Random.generate SetBoard <| randomGrid x y randomColor
