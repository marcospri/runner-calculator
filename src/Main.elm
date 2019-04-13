module Main exposing (main)

import String
import Browser
import Html exposing (Html, div, text, table, th, tr, thead, tbody, td, a, option, select, label, span, i)
import Html.Attributes exposing (class, attribute, value)
import Html.Events exposing (onClick, onInput)
import Length
import Quantity
import Duration
import Pace exposing (Pace, pacePlus, paceMinus, roundPace)


type Unit
    = Metric
    | Imperial


type alias Model =
    { unit : Unit
    , unitLength : Length.Length
    , chartPaceFrom : Pace
    , chartPaceSteps : Int
    , desiredSeconds : Int
    , desiredMinutes : Int
    , desiredHours : Int
    , desiredDistance : Length.Length
    , jumpToMinutes : Int
    , jumpToSeconds : Int
    }


fastPaceLimit : Pace
fastPaceLimit =
    Duration.minutes 2 |> Quantity.per (Length.kilometers 1)


slowPaceLimit : Pace
slowPaceLimit =
    Duration.minutes 10 |> Quantity.per (Length.kilometers 1)


chartDistances : List ( Length.Length, String )
chartDistances =
    [ ( Length.kilometers 1, "1 K" )
    , ( Length.miles 1, "1 Mi" )
    , ( Length.miles 3, "3 Mi" )
    , ( Length.kilometers 5, "5 K" )
    , ( Length.kilometers 10, "10 K" )
    , ( Length.miles 10, "10 Mi" )
    , ( Length.kilometers 21.0975, "1/2 Mar" )
    , ( Length.kilometers 42.195, "Mar" )
    ]


initialModel : Model
initialModel =
    { unit = Metric
    , unitLength = Length.kilometers 1
    , chartPaceFrom = Duration.minutes 5 |> Quantity.per (Length.kilometers 1)
    , chartPaceSteps = 30
    , desiredSeconds = 0
    , desiredMinutes = 0
    , desiredHours = 0
    , desiredDistance = Length.meters 1000
    , jumpToMinutes = 0
    , jumpToSeconds = 0
    }


type Msg
    = ChangeUnit Unit
    | Slower Int
    | Faster Int
    | ChangeJumpToMinutes String
    | ChangeJumpToSeconds String
    | JumpToPace
    | ChangeFinishTimeHours String
    | ChangeFinishTimeMinutes String
    | ChangeFinishTimeSeconds String
    | ChangeFinishTimeDistance String
    | JumpToFinishTime


update : Msg -> Model -> Model
update msg model =
    case msg of
        ChangeUnit unit ->
            case unit of
                Metric ->
                    { model | unit = unit, unitLength = Length.kilometers 1 }

                Imperial ->
                    { model | unit = unit, unitLength = Length.miles 1 }

        Slower more ->
            { model | chartPaceSteps = model.chartPaceSteps + more }

        Faster more ->
            let
                newStartPace =
                    paceMinus model.unitLength model.chartPaceFrom (Duration.seconds (toFloat more))
            in
                { model | chartPaceSteps = model.chartPaceSteps + more, chartPaceFrom = newStartPace }

        ChangeJumpToMinutes minutes ->
            { model | jumpToMinutes = minutes |> String.toInt |> Maybe.withDefault 0 }

        ChangeJumpToSeconds seconds ->
            { model | jumpToSeconds = seconds |> String.toInt |> Maybe.withDefault 0 }

        JumpToPace ->
            let
                jumpToDuration =
                    Duration.seconds (toFloat (model.jumpToSeconds + (model.jumpToMinutes * 60)))
            in
                { model | chartPaceFrom = jumpToDuration |> Quantity.per model.unitLength }

        ChangeFinishTimeHours hours ->
            { model | desiredHours = hours |> String.toInt |> Maybe.withDefault 0 }

        ChangeFinishTimeMinutes minutes ->
            { model | desiredMinutes = minutes |> String.toInt |> Maybe.withDefault 0 }

        ChangeFinishTimeSeconds seconds ->
            { model | desiredSeconds = seconds |> String.toInt |> Maybe.withDefault 0 }

        ChangeFinishTimeDistance distance ->
            { model | desiredDistance = Length.meters (distance |> String.toFloat |> Maybe.withDefault 0) }

        JumpToFinishTime ->
            let
                seconds =
                    (model.desiredSeconds + (model.desiredMinutes * 60) + (model.desiredHours * 3600))
                        |> toFloat

                desiredFinishTime =
                    Duration.seconds seconds

                calculatedPace =
                    desiredFinishTime |> Quantity.per model.desiredDistance |> roundPace model.unitLength
            in
                { model | chartPaceFrom = calculatedPace }


leadingZero : Int -> String
leadingZero value =
    if value < 10 then
        "0" ++ String.fromInt value
    else
        String.fromInt value


renderDuration : Duration.Duration -> String
renderDuration duration =
    let
        totalSeconds =
            duration |> Duration.inSeconds |> round

        hours =
            totalSeconds // 3600

        minutes =
            (totalSeconds - hours * 3600) // 60

        seconds =
            totalSeconds - (hours * 3600 + minutes * 60)

        secondsStr =
            leadingZero seconds

        minutesStr =
            if minutes == 0 then
                "00:"
            else
                (minutes |> leadingZero) ++ ":"

        hoursStr =
            if hours == 0 then
                ""
            else
                String.fromInt hours ++ ":"
    in
        hoursStr ++ minutesStr ++ secondsStr


renderDistanceTimesRow : Model -> Pace -> List Length.Length -> List (Html msg)
renderDistanceTimesRow model pace distances =
    let
        duration =
            model.unitLength
                |> Quantity.at pace

        roundedPace =
            roundPace model.unitLength pace

        times =
            distances |> List.map (\distance -> td [] [ text (distance |> Quantity.at roundedPace |> renderDuration) ])
    in
        th [] [ text (renderDuration duration) ] :: times


rangePaces : Model -> Pace -> Int -> List Pace
rangePaces model paceFrom steps =
    List.range 0 steps
        |> List.map (\i -> pacePlus model.unitLength paceFrom (Duration.seconds (i |> toFloat)))


renderFasterButton : Model -> Html Msg
renderFasterButton model =
    let
        currentFastestDuration =
            model.unitLength |> Quantity.at model.chartPaceFrom

        fastestDurationAllowed =
            model.unitLength |> Quantity.at fastPaceLimit
    in
        if Quantity.lessThan currentFastestDuration fastestDurationAllowed then
            a [ class "button is-large is-fullwidth is-warning", onClick (Faster 10) ]
                [ span [ class "icon is-medium" ]
                    [ i [ class "fas fa-arrow-up" ] []
                    ]
                , span [] [ text "Faster" ]
                , span [ class "icon is-medium" ]
                    [ i [ class "fas fa-arrow-up" ] []
                    ]
                ]
        else
            a [ class "button is-large is-fullwidth", attribute "disabled" "" ] [ text "Too fast" ]


renderSlowerButton : Model -> Html Msg
renderSlowerButton model =
    let
        currentSlowestDuration =
            model.unitLength |> Quantity.at (pacePlus model.unitLength model.chartPaceFrom (Duration.seconds (toFloat model.chartPaceSteps)))

        slowestDurationAllowed =
            model.unitLength |> Quantity.at slowPaceLimit
    in
        if Quantity.greaterThan currentSlowestDuration slowestDurationAllowed then
            a [ class "button is-large is-fullwidth is-warning", onClick (Slower 10) ]
                [ span [ class "icon is-medium" ]
                    [ i [ class "fas fa-arrow-down" ] []
                    ]
                , span [] [ text "Slower" ]
                , span [ class "icon is-medium" ]
                    [ i [ class "fas fa-arrow-down" ] []
                    ]
                ]
        else
            a [ class "button is-large is-fullwidth", attribute "disabled" "" ] [ text "Too slow" ]


renderDesiredFinishTimeForm : Model -> Html Msg
renderDesiredFinishTimeForm model =
    div [ class "field has-addons one-line-form" ]
        [ label [ class "label" ] [ text "Pace to finish:" ]
        , div [ class "control" ]
            [ div [ class "select is-fullwidth" ]
                [ select [ onInput ChangeFinishTimeDistance ]
                    (List.map
                        (\( distance, name ) ->
                            option
                                [ value
                                    (distance
                                        |> Length.inMeters
                                        |> String.fromFloat
                                    )
                                ]
                                [ text name ]
                        )
                        chartDistances
                    )
                ]
            ]
        , label [ class "label secondary" ] [ text "in" ]
        , div [ class "control" ]
            [ div [ class "select is-fullwidth" ]
                [ renderOnChangeSelect
                    (List.range 0 5 |> List.map (\x -> option [] [ text (String.fromInt x) ]))
                    ChangeFinishTimeHours
                ]
            ]
        , label [ class "label secondary" ] [ text "hours" ]
        , div [ class "control" ]
            [ div [ class "select is-fullwidth" ] [ renderOnChangeSelect (List.range 0 59 |> List.map (\x -> option [] [ text (String.fromInt x) ])) ChangeFinishTimeMinutes ]
            ]
        , label [ class "label secondary" ] [ text "minutes" ]
        , div [ class "control" ]
            [ div [ class "select is-fullwidth" ] [ renderOnChangeSelect (List.range 0 59 |> List.map (\x -> option [] [ text (String.fromInt x) ])) ChangeFinishTimeSeconds ]
            ]
        , label [ class "label secondary" ] [ text "minutes" ]
        , renderDesiredFinishTimeFormButton model
        ]


renderDesiredFinishTimeFormButton : Model -> Html Msg
renderDesiredFinishTimeFormButton model =
    if model.desiredSeconds /= 0 || model.desiredMinutes /= 0 || model.desiredHours /= 0 then
        div [ class "control" ]
            [ a [ class "button is-success", onClick JumpToFinishTime ] [ text "Go" ] ]
    else
        div [ class "control" ]
            [ a [ class "button is-success", attribute "disabled" "" ] [ text "Go" ] ]


renderJumpTo : Model -> Html Msg
renderJumpTo model =
    div [ class "field has-addons one-line-form" ]
        [ label [ class "label" ] [ text "Jump to:" ]
        , div [ class "control" ]
            [ div [ class "select is-fullwidth" ] [ renderOnChangeSelect (List.range 0 59 |> List.map (\x -> option [] [ text (String.fromInt x) ])) ChangeJumpToMinutes ]
            ]
        , label [ class "label secondary" ] [ text "minutes" ]
        , div [ class "control" ]
            [ div [ class "select is-fullwidth" ] [ renderOnChangeSelect (List.range 0 59 |> List.map (\x -> option [] [ text (String.fromInt x) ])) ChangeJumpToSeconds ]
            ]
        , label [ class "label secondary" ] [ text "seconds" ]
        , renderJumpToButton model
        ]


renderJumpToButton : Model -> Html Msg
renderJumpToButton model =
    if model.jumpToMinutes /= 0 || model.jumpToSeconds /= 0 then
        div [ class "control" ]
            [ a [ class "button is-success", onClick JumpToPace ] [ text "Go" ] ]
    else
        div [ class "control" ]
            [ a [ class "button is-success", attribute "disabled" "" ] [ text "Go" ] ]


renderOnChangeSelect : List (Html msg) -> (String -> msg) -> Html msg
renderOnChangeSelect options toMsg =
    select [ onInput toMsg ] options


view : Model -> Html Msg
view model =
    div []
        [ div [ class "box" ] [ renderUnitSelector, renderDesiredFinishTimeForm model, renderJumpTo model ]
        , renderFasterButton model
        , table [ class "table is-fullwidth is-striped" ]
            [ thead []
                [ tr []
                    (th
                        []
                        [ text "" ]
                        :: List.map (\( _, distanceName ) -> th [] [ text distanceName ]) chartDistances
                    )
                ]
            , tbody [] (List.map (\pace -> tr [] (renderDistanceTimesRow model pace (List.map Tuple.first chartDistances))) (rangePaces model model.chartPaceFrom model.chartPaceSteps))
            ]
        , renderSlowerButton model
        ]


renderUnitSelector : Html Msg
renderUnitSelector =
    div [ class "columns" ]
        [ div [ class "column" ] [ a [ class "button is-large is-fullwidth is-success", onClick (ChangeUnit Metric) ] [ text "Kilometers" ] ]
        , div [ class "column" ] [ a [ class "button is-large is-fullwidth is-success", onClick (ChangeUnit Imperial) ] [ text "Miles" ] ]
        ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }
