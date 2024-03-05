module Model.PostsConfig exposing (Change(..), PostsConfig, SortBy(..), applyChanges, defaultConfig, filterPosts, sortOptions, sortToCompareFn, sortToString, sortFromString)

import Model.Post exposing (Post)
import Time exposing (Posix)


type SortBy
    = Score
    | Title
    | Posted
    | None


sortOptions : List SortBy
sortOptions =
    [ Score, Title, Posted, None ]


sortToString : SortBy -> String
sortToString sort =
    case sort of
        Score ->
            "Score"

        Title ->
            "Title"

        Posted ->
            "Posted"

        None ->
            "None"


sortFromString : String -> Maybe SortBy
sortFromString str =
    case str of
        "Score" -> Just Score
        "Title" -> Just Title
        "Posted" -> Just Posted
        "None" -> Just None
        _ -> Nothing


type alias PostsConfig =
    { postsToFetch : Int
    , postsToShow : Int
    , sortBy : SortBy
    , showJobs : Bool
    , showTextOnly : Bool
    }


defaultConfig : PostsConfig
defaultConfig =
    PostsConfig 50 10 None False True


type Change
    = SetPostsToFetch Int
    | SetPostsToShow Int
    | SetSortBy SortBy
    | ToggleShowJobs
    | ToggleShowTextOnly


applyChanges : Change -> PostsConfig -> PostsConfig
applyChanges change config =
    case change of
        SetPostsToFetch n -> { config | postsToFetch = n }
        SetPostsToShow n -> { config | postsToShow = n }
        SetSortBy sortBy -> { config | sortBy = sortBy }
        ToggleShowJobs -> { config | showJobs = not config.showJobs }
        ToggleShowTextOnly -> { config | showTextOnly = not config.showTextOnly }

sortToCompareFn : SortBy -> (Post -> Post -> Order)
sortToCompareFn sortBy =
    case sortBy of
        Score ->
            \postA postB -> compare postB.score postA.score

        Title ->
            \postA postB -> compare postA.title postB.title

        Posted ->
            \postA postB -> compare (Time.posixToMillis postB.time) (Time.posixToMillis postA.time)

        None ->
            \_ _ -> EQ


filterPosts : PostsConfig -> List Post -> List Post
filterPosts config posts =
    let
        comparator = sortToCompareFn config.sortBy
        sortedPosts = List.sortWith (\postA postB -> comparator postA postB) posts
        filteredPosts =
            List.filter (\post -> (config.showJobs || post.type_ /= "job") && (config.showTextOnly || Maybe.withDefault "" post.url /= "")) sortedPosts
    in
    List.take config.postsToShow filteredPosts
