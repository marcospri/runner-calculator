module Pace exposing (Pace, pacePlus, paceMinus, roundPace)

import Quantity
import Length
import Duration


type alias Pace =
    Quantity.Quantity Float (Quantity.Rate Duration.Seconds Length.Meters)


pacePlus : Length.Length -> Pace -> Duration.Duration -> Pace
pacePlus distance pace duration =
    Quantity.plus (distance |> Quantity.at pace) duration
        |> Quantity.per distance


paceMinus : Length.Length -> Pace -> Duration.Duration -> Pace
paceMinus distance pace duration =
    Quantity.minus duration (distance |> Quantity.at pace)
        |> Quantity.per distance


roundPace : Length.Length -> Pace -> Pace
roundPace distance pace =
    let
        roundedSeconds =
            distance
                |> Quantity.at pace
                |> Duration.inSeconds
                |> round
                |> toFloat
    in
        Duration.seconds roundedSeconds
            |> Quantity.per distance
