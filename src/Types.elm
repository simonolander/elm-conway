module Types exposing (..)

import Array exposing (Array)
import Time

type State = Running | Paused
type alias Point = (Int, Int)
type alias World = Array (Array Bool)
type alias Citizen = Point

type alias Model =
    { state: State
    , world: World
    }

type Msg =
    Tick Time.Time
    | ReceiveWorld World
