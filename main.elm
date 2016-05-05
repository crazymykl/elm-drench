import Array exposing (Array)
import Random exposing (Generator)
import Html exposing (..)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import StartApp.Simple exposing (start)

type NextModel = NextModel (Maybe Model)

type alias Model =
  Array (Array Color)
type Color = Red | Green


main =
  start { model = model, view = view, update = update }

model : Model
model =
  randomModel 10 10 (Random.initialSeed 5)


view : Signal.Address (Int, Int) -> Model -> Html.Html
view address model =
  let
    enumerate f xs = Array.indexedMap f xs |> Array.toList
    viewRow i row = div [style [("height", "25px")]] (enumerate (viewCell i) row)
    viewCell i j cell = a
      [ onClick address (j, i)
      , style
        [ ("background-color", toString cell)
        , ("height", "25px")
        , ("width", "25px")
        , ("display", "inline-block")
        ]
      ]
      []
  in
    div [] (enumerate viewRow model)

update : (Int, Int) -> Model -> Model
update (x, y) model =
  let
    cell = get2d (x, y) model
  in
    case cell of
      Just cell ->
        model |> set2d (x, y) (successor cell)

      Nothing ->
        model


get2d : (Int, Int) -> Model -> Maybe Color
get2d (x, y) model =
  Array.get y model `Maybe.andThen` Array.get x


set2d : (Int, Int) -> Color -> Model -> Model
set2d (x, y) color model =
  let
    row = Array.get y model
  in
    case row of
      Just row ->
        model |> Array.set y (Array.set x color row)

      Nothing ->
        model

successor : Color -> Color
successor color =
  case color of
    Red -> Green
    Green -> Red


randomColor : Generator Color
randomColor = Random.map (\x ->
  if x then
    Red
  else
    Green) Random.bool


randomModel : Int -> Int -> Random.Seed -> Model
randomModel x y seed =
  let
    randoms = fst <| Random.generate (Random.list x (Random.list y randomColor)) seed
  in
    List.map Array.fromList randoms |> Array.fromList
