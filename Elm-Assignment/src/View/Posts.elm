

module View.Posts exposing (..)
import Html.Attributes exposing (href)
import Html.Events
import Model.PostsConfig exposing (Change(..), PostsConfig, SortBy(..), filterPosts, sortFromString, sortOptions, sortToCompareFn, sortToString)
import Time
import Util.Time

import Html exposing (Html, div, text, table, thead, tbody, tr, th, td, a)
import Html.Attributes exposing (href, class)
import Model exposing (Msg(..))
import Model.Post exposing (Post)
import Model.PostsConfig exposing (PostsConfig, SortBy(..), filterPosts)
import Time exposing (Posix)
import Util.Time exposing (formatTime)
import Html exposing (Html, div, text, table, thead, tbody, tr, th, td, a, label, select, option, input)
import Html.Attributes exposing (href, class, id, for, value, selected, type_)
import Html.Events exposing (onInput, onCheck)
import Model.PostsConfig exposing (Change(..), PostsConfig, SortBy(..), filterPosts)
import Time exposing (Posix)
import Util.Time exposing (formatTime)
import Maybe exposing (withDefault)
import Debug exposing (todo)


{-| Show posts as an HTML [table](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/table)
-}
postTable : PostsConfig -> Posix -> List Post -> Html Msg
postTable config currentTime posts =
    let
        sortedPosts = filterPosts config posts
    in
    table []
        [ thead []
            [ tr []
                [ th [class "post-score"] [text "Score"]
                , th [class "post-title"] [text "Title"]
                , th [class "post-type"] [text "Type"]
                , th [class "post-time"] [text "Posted Date"]
                , th [class "post-url"] [text "Link"]
                ]
            ]
        , tbody [] (List.map (postRow config currentTime) sortedPosts)
        ]


{-| Show a row for each post in the table
-}
postRow : PostsConfig -> Time.Posix -> Post -> Html Msg
postRow config currentTime post =
    let
        date =
            Util.Time.posixToDate Time.utc post.time

        formatDate =
            Util.Time.formatDate date

        postLink =
            a [ href <| Maybe.withDefault "" post.url, class "post-url" ] [ text <| Maybe.withDefault "" post.url ]
    in
    tr []
        [ td [ class "post-score" ] [ text <| String.fromInt post.score ]
        , td [ class "post-title" ] [ text post.title ]
        , td [ class "post-type" ] [ text post.type_ ]
        , td [ class "post-time" ] [ text formatDate ]
        , td [ class "post-url" ] [ postLink ]
        ]



postsConfigView : PostsConfig -> Html Msg
postsConfigView config =
    div []
        [ select [ Html.Attributes.id "select-posts-per-page" ]
            [ option [ value "10", Html.Attributes.selected (config.postsToShow == 10) ] [ text "10" ]
            , option [ value "25", Html.Attributes.selected (config.postsToShow == 25) ] [ text "25" ]
            , option [ value "50", Html.Attributes.selected (config.postsToShow == 50) ] [ text "50" ]
            ]
        , select [ Html.Attributes.id "select-sort-by" ]
            [ option [ value (sortToString Score), Html.Attributes.selected (config.sortBy == Score) ] [ text "Score" ]
            , option [ value (sortToString Title), Html.Attributes.selected (config.sortBy == Title) ] [ text "Title" ]
            , option [ value (sortToString Posted), Html.Attributes.selected (config.sortBy == Posted) ] [ text "Date Posted" ]
            , option [ value (sortToString None), Html.Attributes.selected (config.sortBy == None) ] [ text "None" ]
            ]
        , div []
            [ input [ type_ "checkbox", id "checkbox-show-job-posts" ] []
            , label [ Html.Attributes.for "checkbox-show-job-posts" ] [ text "Show job posts" ]
            ]
        , div []
            [ input [ type_ "checkbox", id "checkbox-show-text-only-posts" ] []
            , label [ Html.Attributes.for "checkbox-show-text-only-posts" ] [ text "Show text only posts" ]
            ]
        ]

