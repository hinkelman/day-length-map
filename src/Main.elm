module Main exposing (main)

import Browser
import Dict exposing (Dict)
import Html exposing (Html, div, h1, input, label, p, span, text)
import Html.Attributes as HA
import Html.Events exposing (onInput)
import Json.Decode as D
import Solar exposing (Definition(..))
import Svg exposing (Svg)
import Svg.Attributes as SA
import Svg.Events
import Svg.Lazy



-- GEOMETRY (pre-projected in R, see data-raw/prepare_geometry.R)


type alias Geometry =
    { width : Float
    , height : Float
    , nation : String
    , states : String
    , bands : List Band
    , parallels : List Parallel
    }


{-| A 0.1° latitude band clipped to the outline, keyed by its central
latitude in tenths of a degree.
-}
type alias Band =
    { latKey : Int
    , path : String
    }


type alias Parallel =
    { lat : Int
    , path : String
    , labelX : Float
    , labelY : Float
    }


geometryDecoder : D.Decoder Geometry
geometryDecoder =
    D.map6 Geometry
        (D.field "width" D.float)
        (D.field "height" D.float)
        (D.field "nation" D.string)
        (D.field "states" D.string)
        (D.field "bands" (D.list bandDecoder))
        (D.field "parallels" (D.list parallelDecoder))


bandDecoder : D.Decoder Band
bandDecoder =
    D.map2 Band
        (D.field "lat" D.int)
        (D.field "path" D.string)


parallelDecoder : D.Decoder Parallel
parallelDecoder =
    D.map4 Parallel
        (D.field "lat" D.int)
        (D.field "path" D.string)
        (D.field "labelX" D.float)
        (D.field "labelY" D.float)



-- MODEL


type alias Model =
    { geometry : Result String Geometry
    , latKeys : List Int
    , definition : Definition
    , threshold : Float
    , profiles : Dict Int (List Float)
    , counts : Dict Int Int
    , hovered : Maybe Int
    }


init : D.Value -> ( Model, Cmd Msg )
init flags =
    let
        geometry =
            D.decodeValue geometryDecoder flags
                |> Result.mapError D.errorToString

        latKeys =
            case geometry of
                Ok geo ->
                    let
                        keys =
                            List.map .latKey geo.bands
                    in
                    Maybe.map2 List.range (List.minimum keys) (List.maximum keys)
                        |> Maybe.withDefault []

                Err _ ->
                    []
    in
    ( { geometry = geometry
      , latKeys = latKeys
      , definition = SunriseSunset
      , threshold = 14
      , profiles = Dict.empty
      , counts = Dict.empty
      , hovered = Nothing
      }
        |> recomputeProfiles
    , Cmd.none
    )


{-| Day lengths for every latitude on the map, for the current definition.
Only depends on the definition, so the slider never touches the trig.
-}
recomputeProfiles : Model -> Model
recomputeProfiles model =
    { model
        | profiles =
            model.latKeys
                |> List.map (\k -> ( k, Solar.dayLengths model.definition (toFloat k / 10) ))
                |> Dict.fromList
    }
        |> recomputeCounts


recomputeCounts : Model -> Model
recomputeCounts model =
    { model | counts = Dict.map (\_ -> daysAtLeast model.threshold) model.profiles }


daysAtLeast : Float -> List Float -> Int
daysAtLeast hours lengths =
    List.foldl
        (\h n ->
            if h >= hours then
                n + 1

            else
                n
        )
        0
        lengths



-- UPDATE


type Msg
    = SetThreshold String
    | SetDefinition Definition
    | Hover (Maybe Int)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetThreshold str ->
            case String.toFloat str of
                Just t ->
                    ( recomputeCounts { model | threshold = t }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        SetDefinition d ->
            ( recomputeProfiles { model | definition = d }, Cmd.none )

        Hover key ->
            ( { model | hovered = key }, Cmd.none )



-- COLOR CLASSES (fills live in style.css so light and dark mode each get their own ramp)


{-| 0 = never, 1–12 = roughly one month of days each, 13 = every day.
-}
binOf : Int -> Int
binOf days =
    if days <= 0 then
        0

    else if days >= 365 then
        13

    else
        clamp 1 12 (ceiling (toFloat (days * 12) / 365))


binClass : Int -> String
binClass days =
    "b" ++ String.fromInt (binOf days)


binRange : Int -> String
binRange bin =
    let
        edge k =
            (365 * k) // 12
    in
    case bin of
        0 ->
            "0 days"

        13 ->
            "365 days"

        _ ->
            String.fromInt (edge (bin - 1) + 1) ++ "–" ++ String.fromInt (min 364 (edge bin)) ++ " days"


binLabel : Int -> String
binLabel bin =
    case bin of
        0 ->
            "Never"

        3 ->
            "3 mo"

        6 ->
            "6 mo"

        9 ->
            "9 mo"

        13 ->
            "Every day"

        _ ->
            ""



-- VIEW


view : Model -> Html Msg
view model =
    case model.geometry of
        Err err ->
            div [ HA.class "app" ]
                [ h1 [] [ text "Could not load geometry" ]
                , p [] [ text "Run Rscript data-raw/prepare_geometry.R to create public/geometry.js." ]
                , Html.pre [] [ text err ]
                ]

        Ok geo ->
            div [ HA.class "app" ]
                [ h1 []
                    [ text "Days per year with at least "
                    , span [ HA.class "num" ] [ text (formatHours model.threshold) ]
                    , text " of daylight"
                    ]
                , p [ HA.class "sub" ]
                    [ text "Day length depends only on latitude and date, so the map is stripes that bend with the Albers projection. Drag the slider to move them." ]
                , viewControls model
                , viewReadout model
                , div [ HA.class "panels" ]
                    [ div [ HA.class "map-panel" ]
                        [ viewMap geo model
                        , viewLegend
                        ]
                    , viewProfile model
                    ]
                ]


viewControls : Model -> Html Msg
viewControls model =
    div [ HA.class "controls" ]
        [ label [ HA.class "slider" ]
            [ span [ HA.class "control-label" ] [ text "Threshold" ]
            , input
                [ HA.type_ "range"
                , HA.min "8"
                , HA.max "18"
                , HA.step "0.05"
                , HA.value (String.fromFloat model.threshold)
                , onInput SetThreshold
                , HA.attribute "aria-label" "Daylight threshold in hours"
                ]
                []
            , span [ HA.class "num value" ] [ text (formatHours model.threshold) ]
            ]
        , div [ HA.class "segmented", HA.attribute "role" "radiogroup" ]
            (List.map (definitionButton model.definition) [ SunriseSunset, CivilTwilight ])
        ]


definitionButton : Definition -> Definition -> Html Msg
definitionButton current d =
    Html.button
        [ HA.classList [ ( "active", current == d ) ]
        , HA.attribute "role" "radio"
        , HA.attribute "aria-checked"
            (if current == d then
                "true"

             else
                "false"
            )
        , Html.Events.onClick (SetDefinition d)
        ]
        [ text (Solar.definitionLabel d) ]


viewMap : Geometry -> Model -> Html Msg
viewMap geo model =
    let
        hoveredBands =
            case model.hovered of
                Just k ->
                    List.filter (\b -> b.latKey == k) geo.bands

                Nothing ->
                    []
    in
    Svg.svg
        [ SA.viewBox ("0 0 " ++ String.fromFloat geo.width ++ " " ++ String.fromFloat geo.height)
        , SA.class "map"
        , Svg.Events.on "mouseleave" (D.succeed (Hover Nothing))
        ]
        [ Svg.Lazy.lazy2 viewBands geo.bands model.counts
        , Svg.g [ SA.class "highlight" ] (List.map (\b -> Svg.path [ SA.d b.path ] []) hoveredBands)
        , Svg.path [ SA.d geo.states, SA.class "states" ] []
        , Svg.path [ SA.d geo.nation, SA.class "outline" ] []
        , Svg.g [ SA.class "parallels" ] (List.map viewParallel geo.parallels)
        ]


viewBands : List Band -> Dict Int Int -> Svg Msg
viewBands bands counts =
    Svg.g [ SA.class "bands" ]
        (List.map
            (\b ->
                Svg.path
                    [ SA.d b.path
                    , SA.class (binClass (Dict.get b.latKey counts |> Maybe.withDefault 0))
                    , Svg.Events.onMouseOver (Hover (Just b.latKey))
                    ]
                    []
            )
            bands
        )


viewParallel : Parallel -> Svg msg
viewParallel par =
    Svg.g []
        [ Svg.path [ SA.d par.path ] []
        , Svg.text_
            [ SA.x (String.fromFloat (par.labelX + 2))
            , SA.y (String.fromFloat (par.labelY - 3))
            ]
            [ Svg.text (String.fromInt par.lat ++ "°N") ]
        ]


viewLegend : Html msg
viewLegend =
    div [ HA.class "legend" ]
        [ span [ HA.class "legend-title" ] [ text "Days per year" ]
        , div [ HA.class "legend-strip" ]
            (List.map
                (\bin ->
                    span [ HA.class "legend-step", HA.title (binRange bin) ]
                        [ span [ HA.class ("swatch b" ++ String.fromInt bin) ] []
                        , span [ HA.class "legend-label" ] [ text (binLabel bin) ]
                        ]
                )
                (List.range 0 13)
            )
        ]




-- PROFILE CHART: latitude (y) vs. days (x), the whole map in one line


viewProfile : Model -> Html Msg
viewProfile model =
    let
        w =
            280

        h =
            420

        left =
            40

        right =
            14

        top =
            12

        bottom =
            34

        minKey =
            List.head model.latKeys |> Maybe.withDefault 240

        maxKey =
            List.reverse model.latKeys |> List.head |> Maybe.withDefault 500

        xOf days =
            left + toFloat days / 365 * (w - left - right)

        yOf key =
            top + toFloat (maxKey - key) / toFloat (max 1 (maxKey - minKey)) * (h - top - bottom)

        rowH =
            (h - top - bottom) / toFloat (max 1 (maxKey - minKey))

        points =
            Dict.toList model.counts
                |> List.map (\( k, n ) -> String.fromFloat (xOf n) ++ "," ++ String.fromFloat (yOf k))
                |> String.join " "

        latTicks =
            List.filter (\l -> l * 10 >= minKey && l * 10 <= maxKey) [ 25, 30, 35, 40, 45 ]

        dayTicks =
            [ 0, 100, 200, 300, 365 ]

        num =
            String.fromFloat

        crosshair =
            case model.hovered of
                Just k ->
                    let
                        n =
                            Dict.get k model.counts |> Maybe.withDefault 0
                    in
                    [ Svg.line [ SA.class "crosshair", SA.x1 (num left), SA.x2 (num (w - right)), SA.y1 (num (yOf k)), SA.y2 (num (yOf k)) ] []
                    , Svg.circle [ SA.class "dot", SA.cx (num (xOf n)), SA.cy (num (yOf k)), SA.r "4.5" ] []
                    ]

                Nothing ->
                    []
    in
    div [ HA.class "profile-panel" ]
        [ p [ HA.class "chart-title" ] [ text "Same data by latitude" ]
        , Svg.svg
            [ SA.viewBox ("0 0 " ++ num w ++ " " ++ num h)
            , SA.class "profile"
            , Svg.Events.on "mouseleave" (D.succeed (Hover Nothing))
            ]
            (List.concat
                [ List.map
                    (\d ->
                        Svg.g [ SA.class "grid" ]
                            [ Svg.line [ SA.x1 (num (xOf d)), SA.x2 (num (xOf d)), SA.y1 (num top), SA.y2 (num (h - bottom)) ] []
                            , Svg.text_ [ SA.x (num (xOf d)), SA.y (num (h - bottom + 16)), SA.textAnchor "middle" ] [ Svg.text (String.fromInt d) ]
                            ]
                    )
                    dayTicks
                , List.map
                    (\l ->
                        Svg.g [ SA.class "grid" ]
                            [ Svg.line [ SA.x1 (num left), SA.x2 (num (w - right)), SA.y1 (num (yOf (l * 10))), SA.y2 (num (yOf (l * 10))) ] []
                            , Svg.text_ [ SA.x (num (left - 6)), SA.y (num (yOf (l * 10) + 4)), SA.textAnchor "end" ] [ Svg.text (String.fromInt l ++ "°") ]
                            ]
                    )
                    latTicks
                , [ Svg.text_ [ SA.class "axis-title", SA.x (num ((left + w - right) / 2)), SA.y (num (h - 4)), SA.textAnchor "middle" ] [ Svg.text "days per year" ]
                  , Svg.polyline [ SA.points points, SA.class "profile-line" ] []
                  ]
                , crosshair
                , List.map
                    (\k ->
                        Svg.rect
                            [ SA.class "hit"
                            , SA.x (num left)
                            , SA.y (num (yOf k - rowH / 2))
                            , SA.width (num (w - left - right))
                            , SA.height (num rowH)
                            , Svg.Events.onMouseOver (Hover (Just k))
                            ]
                            []
                    )
                    model.latKeys
                ]
            )
        ]


viewReadout : Model -> Html msg
viewReadout model =
    div [ HA.class "readout", HA.attribute "aria-live" "polite" ]
        (case model.hovered |> Maybe.andThen (\k -> Maybe.map (Tuple.pair k) (Dict.get k model.profiles)) of
            Just ( k, lengths ) ->
                let
                    n =
                        Dict.get k model.counts |> Maybe.withDefault 0

                    shortest =
                        List.minimum lengths |> Maybe.withDefault 0

                    longest =
                        List.maximum lengths |> Maybe.withDefault 0
                in
                [ span [ HA.class "num strong" ] [ text (formatLat k) ]
                , text " · "
                , span [ HA.class "num strong" ] [ text (String.fromInt n) ]
                , text (" days with at least " ++ formatHours model.threshold ++ " · day length runs from ")
                , span [ HA.class "num" ] [ text (formatHours shortest) ]
                , text " in December to "
                , span [ HA.class "num" ] [ text (formatHours longest) ]
                , text " in June"
                ]

            Nothing ->
                [ text "Hover the map or the chart to read a latitude." ]
        )



-- FORMATTING


formatHours : Float -> String
formatHours hours =
    let
        totalMinutes =
            round (hours * 60)
    in
    String.fromInt (totalMinutes // 60)
        ++ " h "
        ++ String.padLeft 2 '0' (String.fromInt (modBy 60 totalMinutes))
        ++ " m"


formatLat : Int -> String
formatLat key =
    String.fromInt (key // 10) ++ "." ++ String.fromInt (modBy 10 key) ++ "°N"



-- MAIN


main : Program D.Value Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }
