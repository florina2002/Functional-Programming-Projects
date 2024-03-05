module Util.Time exposing (..)

import Time exposing (Posix, Zone, Month(..), toYear, toMonth, toDay, toHour, toMinute, posixToMillis)
import Basics exposing (remainderBy)


type Date
    = Date { year : Int, month : Month, day : Int }


monthToString : Month -> String
monthToString month =
    case month of
        Jan ->
            "Jan"

        Feb ->
            "Feb"

        Mar ->
            "Mar"

        Apr ->
            "Apr"

        May ->
            "May"

        Jun ->
            "Jun"

        Jul ->
            "Jul"

        Aug ->
            "Aug"

        Sep ->
            "Sep"

        Oct ->
            "Oct"

        Nov ->
            "Nov"

        Dec ->
            "Dec"


posixToDate : Zone -> Posix -> Date
posixToDate tz time =
    let
        year =
            toYear tz time

        month =
            toMonth tz time

        day =
            toDay tz time
    in
    Date { year = year, month = month, day = day }


formatDate : Date -> String
formatDate (Date date) =
    let
        year =
            String.fromInt date.year

        month =
            monthToString date.month

        day =
            String.fromInt date.day |> String.padLeft 2 '0'
    in
    year ++ " " ++ month ++ " " ++ day 


formatTime : Zone -> Posix -> String
formatTime tz time =
    let
        date =
            posixToDate tz time

        hour =
            toHour tz time |> String.fromInt |> String.padLeft 2 '0'

        minute =
            toMinute tz time |> String.fromInt |> String.padLeft 2 '0'
    in
    formatDate date ++ " " ++ hour ++ ":" ++ minute


type alias Duration =
    { seconds : Int
    , minutes : Int
    , hours : Int
    , days : Int
    }


durationBetween : Posix -> Posix -> Maybe Duration
durationBetween t1 t2 =
    let
        millis =
            posixToMillis t2 - posixToMillis t1
    in
    if millis <= 0 then  -- Adjusted condition here
        Nothing
    else
        Just
            { seconds = remainderBy 60 (millis // 1000)
            , minutes = remainderBy 60 (millis // (1000 * 60))
            , hours = remainderBy 24 (millis // (1000 * 60 * 60))
            , days = millis // (1000 * 60 * 60 * 24)
            }



formatDuration : Duration -> String
formatDuration duration =
    let
        formatUnit unit singular =
            if unit == 0 then
                ""
            else if unit == 1 then
                String.fromInt unit ++ " " ++ singular ++ " "
            else
                String.fromInt unit ++ " " ++ singular ++ "s "
    in
    (formatUnit duration.days "day")
        ++ (formatUnit duration.hours "hour")
        ++ (formatUnit duration.minutes "minute")
        ++ (formatUnit duration.seconds "second")
        ++ "ago"
