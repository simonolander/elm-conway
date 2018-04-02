module World exposing (..)

import Array
import Collage
import Color
import Types exposing (..)
import Random


emptyWorld : Int -> Int -> World
emptyWorld width height =
    Array.repeat height (Array.repeat width False)


renderWorld : World -> Collage.Form
renderWorld world =
    points world
    |> List.map (flip renderPoint world)
    |> Collage.group


renderCitizen : Citizen -> Collage.Form
renderCitizen (x, y) =
    Collage.square 0.9
    |> Collage.filled Color.yellow
    |> Collage.move (toFloat x, toFloat y)


renderPoint : Point -> World -> Collage.Form
renderPoint (ri, ci) world =
    Collage.square 0.9
    |> Collage.filled (if isCitizen ri ci world then Color.yellow else Color.black)
    |> Collage.move (toFloat ci, toFloat ri)


isCitizen : Int -> Int -> World -> Bool
isCitizen ri ci world =
    Array.get ri world
    |> Maybe.andThen (Array.get ci)
    |> Maybe.withDefault False


getCitizens : World -> List Citizen
getCitizens world =
    let
        isCitizenRow ci row =
            Array.get ci row |> Maybe.withDefault False
        getCitizensCol row =
            List.range 0 (Array.length row)
            |> List.filter (flip isCitizenRow row)
        getCitizensRow ri =
            Array.get ri world
            |> Maybe.map (getCitizensCol)
            |> Maybe.map (List.map (flip (,) ri))
            |> Maybe.withDefault []
    in
        List.range 0 (Array.length world)
        |> List.concatMap getCitizensRow


getNonCitizens : World -> List Point
getNonCitizens world = []

fromList : List (List Bool) -> World
fromList lists =
    List.map Array.fromList lists
    |> Array.fromList


toList : World -> List (List Bool)
toList world =
    Array.map Array.toList world
    |> Array.toList


mapPosition : (Point -> b) -> Array.Array (Array.Array a) -> Array.Array (Array.Array b)
mapPosition func world =
    Array.map Array.length world
    |> Array.indexedMap (\ri width -> Array.initialize width (\ ci -> func (ri, ci)) )


points : World -> List Point
points world =
    Array.toIndexedList world
    |> List.concatMap (\(ri, row) -> List.map ((,) ri) (List.range 0 (Array.length row)))


stepWorld : World -> World
stepWorld world =
    let
        neighbourLocations (row, col) =
            [ (row + 0, col + 1)
            , (row - 1, col + 1)
            , (row - 1, col + 0)
            , (row - 1, col - 1)
            , (row + 0, col - 1)
            , (row + 1, col - 1)
            , (row + 1, col + 0)
            , (row + 1, col + 1)
            ]

        isAlive (row, col) =
            isCitizen row col world

        numAliveNeighbours position =
            neighbourLocations position
            |> List.filter isAlive
            |> List.length

        willBeAlive (row, col) =
            let
                aliveNeighbours = numAliveNeighbours (row, col)
                alive = isAlive (row, col)
            in
                (alive && (aliveNeighbours == 2 || aliveNeighbours == 3)) || (not alive && aliveNeighbours == 3)
    in
        mapPosition willBeAlive world


randomizeWorld : World -> Cmd Msg
randomizeWorld world =
    let
        flattenList : List (Random.Generator a) -> Random.Generator (List a)
        flattenList generators =
            case generators of
                [] -> Random.map (\_ -> []) Random.bool
                g :: gs -> Random.map2 (::) g (flattenList gs)

        rowGenerator : Int -> Random.Generator (List Bool)
        rowGenerator width =
            Random.list width Random.bool

        worldGenerator : List Int -> Random.Generator World
        worldGenerator widths =
            List.map rowGenerator widths
            |> flattenList
            |> Random.map fromList
    in
        List.map (List.length) (toList world)
        |> worldGenerator
        |> Random.generate ReceiveWorld
