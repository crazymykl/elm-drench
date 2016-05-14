import Array exposing (Array)
import Random exposing (Generator)
import Html exposing (..)
import Html.App exposing (beginnerProgram)
import Html.Attributes exposing (class, style, rel, href)
import Html.Events exposing (onClick)

type alias Model = Array (Array Color)
type alias Point = (Int, Int)
type Color = Red | Green | Blue | Purple

main : Program Never
main =
  beginnerProgram { model = model, view = view, update = update }


model : Model
model =
  randomModel 20 20 (Random.initialSeed 4)


view : Model -> Html (Color)
view model =
  let
    colorButtons color = button [onClick color] [text (color |> toString)]
    arrMap f xs = Array.map f xs |> Array.toList
    viewRow row = div [class "row"] (arrMap viewCell row)
    viewCell cell = span
      [ style [("background-color", toString cell)]
      , class "cell"
      ] []
  in
    main' []
      [ css "style.css"
      , div [class "buttons"] (List.map colorButtons [Red, Green, Blue, Purple])
      , div [class "grid"] (arrMap viewRow model)
      ]


update : Color -> Model -> Model
update color model =
  case get2d (0, 0) model of
    Just oldColor ->
      if color == oldColor then
        model
      else
        recolor oldColor color (0, 0) model

    Nothing ->
      model


css : String -> Html a
css path =
  node "link" [rel "stylesheet", href path] []


get2d : Point -> Model -> Maybe Color
get2d (x, y) model =
  Array.get y model `Maybe.andThen` Array.get x


set2d : Point -> Color -> Model -> Model
set2d (x, y) color model =
  case Array.get y model of
    Just row ->
      model |> Array.set y (Array.set x color row)

    Nothing ->
      model


cascade : Color -> Color -> Point -> Model -> Model
cascade color newColor point model =
  List.foldl (recolor color newColor) model (neighbors point)


recolor : Color -> Color -> Point -> Model -> Model
recolor color newColor point model =
  let
    myColor = get2d point model
    nextModel = set2d point newColor model
  in
    if myColor == Just color then
      cascade color newColor point nextModel
    else
      model


neighbors : Point -> List Point
neighbors (x, y) =
  [ (x, y-1), (x-1, y), (x+1, y), (x, y+1) ]


randomColor : Generator Color
randomColor = Random.map (\x ->
  case x of
    0 -> Red
    1 -> Blue
    2 -> Green
    3 -> Purple
    _ -> Debug.crash "Not enough colors!"
  ) (Random.int 0 3)


randomModel : Int -> Int -> Random.Seed -> Model
randomModel x y seed =
  let
    randoms = fst <| Random.step (Random.list x <| Random.list y randomColor) seed
  in
    List.map Array.fromList randoms |> Array.fromList
