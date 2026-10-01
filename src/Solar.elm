module Solar exposing (Definition(..), dayLength, dayLengths, definitionLabel, horizonAltitude)

{-| Day length from latitude and day of year.

Uses the NOAA (Spencer 1971) Fourier series for solar declination and the
standard hour-angle equation for when the sun's centre crosses a given
altitude. Accurate to a minute or two, which is far below what the map can
show.

-}


type Definition
    = SunriseSunset
    | CivilTwilight


definitionLabel : Definition -> String
definitionLabel definition =
    case definition of
        SunriseSunset ->
            "Sunrise to sunset"

        CivilTwilight ->
            "Civil dawn to civil dusk"


{-| Solar altitude (degrees) that marks the start and end of "day".

Sunrise/sunset is −0.833°: 34′ of atmospheric refraction plus 16′ for the
sun's upper limb. Civil twilight ends when the centre is 6° below the horizon.

-}
horizonAltitude : Definition -> Float
horizonAltitude definition =
    case definition of
        SunriseSunset ->
            -0.833

        CivilTwilight ->
            -6


{-| Solar declination (radians) for day of year 1–365.
-}
declination : Int -> Float
declination doy =
    let
        g =
            2 * pi / 365 * toFloat (doy - 1)
    in
    0.006918
        - 0.399912 * cos g
        + 0.070257 * sin g
        - 0.006758 * cos (2 * g)
        + 0.000907 * sin (2 * g)
        - 0.002697 * cos (3 * g)
        + 0.00148 * sin (3 * g)


{-| Hours between the sun rising through and setting through the
definition's altitude, at a latitude (degrees) on a day of year.
-}
dayLength : Definition -> Float -> Int -> Float
dayLength definition latDeg doy =
    let
        phi =
            degrees latDeg

        delta =
            declination doy

        h0 =
            degrees (horizonAltitude definition)

        cosHourAngle =
            (sin h0 - sin phi * sin delta) / (cos phi * cos delta)
    in
    if cosHourAngle >= 1 then
        0

    else if cosHourAngle <= -1 then
        24

    else
        2 * (acos cosHourAngle * 180 / pi) / 15


{-| Day lengths for every day of a 365-day year.
-}
dayLengths : Definition -> Float -> List Float
dayLengths definition latDeg =
    List.map (dayLength definition latDeg) (List.range 1 365)
