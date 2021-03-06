module Grid exposing (Grid, Point, empty, get2d, neighbors, randomGrid, set2d)

import Array exposing (Array)
import Random exposing (Generator)


type alias Grid a =
    Array (Array a)


type alias Point =
    ( Int, Int )


get2d : Point -> Grid a -> Maybe a
get2d ( x, y ) grid =
    Array.get y grid |> Maybe.andThen (Array.get x)


set2d : Point -> a -> Grid a -> Grid a
set2d ( x, y ) value grid =
    Array.get y grid
        |> Maybe.map (\a -> (Array.set y << Array.set x value) a grid)
        |> Maybe.withDefault grid


neighbors : Point -> List Point
neighbors ( x, y ) =
    [ ( x, y - 1 ), ( x - 1, y ), ( x + 1, y ), ( x, y + 1 ) ]


randomGrid : Int -> Int -> Generator a -> Generator (Grid a)
randomGrid x y gen =
    let
        randArray n g =
            Random.map Array.fromList <| Random.list n g
    in
    randArray x <| randArray y gen


empty : Grid a
empty =
    Array.empty
