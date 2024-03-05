module Model.Post exposing (..)

import Json.Decode as De
import Time exposing (Posix, millisToPosix)


type alias Post =
    { by : String, id : Int, score : Int, title : String, url : Maybe String, time : Posix, type_ : String }


decode : De.Decoder Post
decode =
    De.map7 Post
        (De.field "by" De.string)
        (De.field "id" De.int)
        (De.field "score" De.int)
        (De.field "title" De.string)
        (De.field "url" (De.maybe De.string))
        (De.field "time" (De.map (millisToPosix << (*) 1000) De.int))  
        (De.field "type" De.string)
