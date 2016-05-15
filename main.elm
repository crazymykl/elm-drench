import Array exposing (Array)
import Random exposing (Generator)
import Random.Array exposing (sample)
import Html exposing (..)
import Html.App exposing (program)
import Html.Attributes exposing (class, style, rel, href)
import Html.Events exposing (onClick)

type alias Board = Array (Array Color)
type alias Point = (Int, Int)
type Color = Red | Green | Blue | Purple | Yellow
type Msg
  = RandomBoard Board
  | Advance Color


main : Program Never
main =
  program { init = init, view = view, update = update, subscriptions = \_ -> Sub.none }


init : (Board, Cmd Msg)
init =
  (Array.empty, randomBoard 20 20)


view : Board -> Html Msg
view board =
  let
    colorButtons color = button [onClick <| Advance color] [text (color |> toString)]
    arrMap f xs = Array.map f xs |> Array.toList
    viewRow row = div [class "row"] (arrMap viewCell row)
    viewCell cell = span
      [ style [("background-color", toString cell)]
      , class "cell"
      ] []
  in
    main' []
      [ css "style.css"
      , div [class "buttons"] (arrMap colorButtons colors)
      , div [class "grid"] (arrMap viewRow board)
      ]


update : Msg -> Board -> (Board, Cmd Msg)
update msg board =
  let
    nextBoard = case msg of
      Advance color ->
        advance color board

      RandomBoard board ->
        board
  in
    (nextBoard, Cmd.none)


advance : Color -> Board -> Board
advance color board =
  let
    oldColor = Maybe.withDefault color <| get2d (0, 0) board
  in
    if color == oldColor then
      board
    else
      recolor oldColor color (0, 0) board


css : String -> Html a
css path =
  node "link" [rel "stylesheet", href path] []


get2d : Point -> Board -> Maybe Color
get2d (x, y) board =
  Array.get y board `Maybe.andThen` Array.get x


set2d : Point -> Color -> Board -> Board
set2d (x, y) color board =
  Array.get y board
    |> Maybe.map (flip (Array.set y << Array.set x color) board)
    |> Maybe.withDefault board


cascade : Color -> Color -> Point -> Board -> Board
cascade color newColor point board =
  List.foldl (recolor color newColor) board (neighbors point)


recolor : Color -> Color -> Point -> Board -> Board
recolor color newColor point board =
  let
    myColor = get2d point board
    nextBoard = set2d point newColor board
  in
    if myColor == Just color then
      cascade color newColor point nextBoard
    else
      board


neighbors : Point -> List Point
neighbors (x, y) =
  [ (x, y-1), (x-1, y), (x+1, y), (x, y+1) ]


colors : Array Color
colors = Array.fromList [Red, Green, Blue, Purple, Yellow]


randomColor : Generator Color
randomColor =
  Random.map (Maybe.withDefault Red) (sample colors)


randomBoard : Int -> Int -> Cmd Msg
randomBoard x y =
  let
    randArray n q = Array.fromList `Random.map` Random.list n q
    randoms = randArray x <| randArray y randomColor
  in
    Random.generate RandomBoard randoms
