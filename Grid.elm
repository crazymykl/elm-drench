module Grid exposing (..)

import Array exposing (Array)
import Random exposing (Generator)


type Grid a
    = Grid (Array (Array a))


type alias Point =
    ( Int, Int )


get2d : Point -> Grid a -> Maybe a
get2d ( x, y ) (Grid grid) =
    Array.get y grid |> Maybe.andThen (Array.get x)


set2d : Point -> a -> Grid a -> Grid a
set2d ( x, y ) value (Grid grid) =
    Array.get y grid
        |> Maybe.map (\a -> (Array.set y << Array.set x value) a grid)
        |> Maybe.withDefault grid
        |> Grid


neighbors : Point -> List Point
neighbors ( x, y ) =
    [ ( x, y - 1 ), ( x - 1, y ), ( x + 1, y ), ( x, y + 1 ) ]


random : Int -> Int -> Generator a -> Generator (Grid a)
random x y gen =
    let
        randArray n g =
            Random.map Array.fromList <| Random.list n g
    in
    gen
        |> randArray y
        |> randArray x
        |> Random.map Grid


toLists : Grid a -> List (List a)
toLists (Grid grid) =
    Array.toList grid
        |> List.map Array.toList


empty : Grid a
empty =
    Grid Array.empty
