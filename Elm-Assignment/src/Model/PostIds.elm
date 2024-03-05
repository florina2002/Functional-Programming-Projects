module Model.PostIds exposing (..)

import Cursor exposing (Cursor, withSelectedElement, current, forward, fromList)
import Json.Decode as De exposing (Decoder, list, int, andThen)


type HackerNewsItem
    = Top
    | New
    | Show
    | Ask
    | Jobs


itemName : HackerNewsItem -> String
itemName item =
    case item of
        Top ->
            "top"

        New ->
            "new"

        Show ->
            "show"

        Ask ->
            "ask"

        Jobs ->
            "job"


type PostIds
    = PostIds (Cursor Int)


first : PostIds -> Int
first (PostIds ids) =
    current ids

advance : PostIds -> Maybe (Int, PostIds)
advance (PostIds ids) =
    case Cursor.forward ids of
        Just newerCursor ->
            Just (Cursor.current newerCursor, PostIds newerCursor)

        Nothing ->
            Nothing

fromList : List Int -> Maybe PostIds
fromList ids =
    Cursor.fromList ids
        |> Maybe.map PostIds


decode : Decoder (Maybe PostIds)
decode =
    list int
        |> andThen (fromList >> De.succeed)
