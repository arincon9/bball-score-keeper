module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


-- Model


type alias Model =
    { players : List Player
    , name : String
    , playerId : Maybe Int
    , plays : List Play
    }


type alias Player =
    { id : Int
    , name : String
    , points : Int
    }


type alias Play =
    { id : Int
    , playerId : Int
    , name : String
    , points : Int
    }


initModel : Model
initModel =
    { players = []
    , name = ""
    , playerId = Nothing
    , plays = []
    }



-- Update


type Msg
    = Edit Player
    | Score Player Int
    | Input String
    | Save
    | Cancel
    | DeletePlay Play


update : Msg -> Model -> Model
update msg model =
    case msg of
        Input name ->
            Debug.log "Input Update Model"
                { model | name = name }

        Cancel ->
            { model | name = "", playerId = Nothing }

        Save ->
            if String.isEmpty model.name then
                model
            else
                save model

        Score player points ->
            score model player points

        Edit player ->
            { model | name = player.name, playerId = Just player.id }

        DeletePlay play ->
            deletePlay model play


deletePlay : Model -> Play -> Model
deletePlay model play =
    let
        newPlays =
            List.filter (\p -> p.id /= play.id) model.plays

        newPlayers =
            List.map
                (\player ->
                    if player.id == play.playerId then
                        { player | points = player.points - 1 * play.points }
                    else
                        player
                )
                model.players
    in
    { model
        | plays = newPlays
        , players = newPlayers
    }


score : Model -> Player -> Int -> Model
score model scorer points =
    let
        newPlayers =
            List.map
                (\player ->
                    if player.id == scorer.id then
                        { player | points = player.points + points }
                    else
                        player
                )
                model.players

        play =
            Play (List.length model.plays) scorer.id scorer.name points
    in
    { model
        | players = newPlayers
        , plays = play :: model.plays
    }


save : Model -> Model
save model =
    case model.playerId of
        Just id ->
            edit model id

        Nothing ->
            add model


add : Model -> Model
add model =
    let
        player =
            Player (List.length model.players) model.name 0

        newPlayers =
            player :: model.players
    in
    { model
        | players = newPlayers
        , name = ""
    }


edit : Model -> Int -> Model
edit model id =
    let
        newPlayers =
            List.map
                (\player ->
                    if player.id == id then
                        { player | name = model.name }
                    else
                        player
                )
                model.players

        newPlays =
            List.map
                (\play ->
                    if play.playerId == id then
                        { play | name = model.name }
                    else
                        play
                )
                model.plays
    in
    { model
        | players = newPlayers
        , plays = newPlays
        , name = ""
        , playerId = Nothing
    }



-- View


view : Model -> Html Msg
view model =
    div
        [ class "container mt-5" ]
        [ h1 [ class "row justify-content-md-center mb-5" ] [ text "🏀 Score Keeper 🏀" ]
        , p [ class "row justify-content-md-center mb-5" ]
            [ text "© Made with ☕️ and Elm by "
            , a [ href "https://twitter.com/alejandroRINC0N", rel "external", target "_blank" ]
                [ text "@alejandroRINC0N" ]
            ]
        , playerSection model
        , playerForm model
        , playSection model
        ]


playSection : Model -> Html Msg
playSection model =
    div [ class "row justify-content-md-center" ]
        [ table [ class "table table-striped table-hover" ]
            [ playListHeader
            , playList model
            ]
        ]


playListHeader : Html Msg
playListHeader =
    thead [ class "thead-inverse" ]
        [ tr []
            [ th [] [ text "" ]
            , th [] [ text "Plays" ]
            , th [] [ text "Points" ]
            ]
        ]


playList : Model -> Html Msg
playList model =
    model.plays
        |> List.map play
        |> tbody []


play : Play -> Html Msg
play play =
    tr []
        [ td []
            [ div [ class "col-md-3 text-center" ]
                [ button
                    [ attribute "aria-label" "Close"
                    , class "close justify-content-center"
                    , type_ "button"
                    , onClick (DeletePlay play)
                    ]
                    [ span [ attribute "aria-hidden" "true" ]
                        [ text "×" ]
                    ]
                ]
            ]
        , td []
            [ text play.name ]
        , td []
            [ text (toString play.points) ]
        ]


playerSection : Model -> Html Msg
playerSection model =
    div [ class "row justify-content-md-center" ]
        [ table [ class "table table-hover" ]
            [ playerListHeader
            , playerList model
            , pointTotal model
            ]
        ]


playerListHeader : Html Msg
playerListHeader =
    thead [ class "thead-default" ]
        [ tr []
            [ th [] [ text "" ]
            , th [] [ text "Player Name" ]
            , th [] [ text "" ]
            , th [] [ text "Points" ]
            ]
        ]


playerList : Model -> Html Msg
playerList model =
    model.players
        |> List.sortBy .name
        |> List.map player
        |> tbody []


player : Player -> Html Msg
player player =
    tr []
        [ td []
            [ div [ class "col-md-3 text-center" ]
                [ i
                    [ class "fa fa-pencil-square-o fa-2x justify-content-center"
                    , attribute "aria-hidden" "true"
                    , onClick (Edit player)
                    ]
                    []
                ]
            ]
        , td []
            [ text player.name ]
        , td []
            [ button
                [ type_ "button"
                , class "btn btn-outline-success mx-2"
                , onClick (Score player 2)
                ]
                [ text "+ 2pt" ]
            , button
                [ type_ "button"
                , class "btn btn-outline-info mx-2"
                , onClick (Score player 3)
                ]
                [ text "+ 3pt" ]
            ]
        , td []
            [ text (toString player.points) ]
        ]


pointTotal : Model -> Html Msg
pointTotal model =
    let
        total =
            List.map .points model.plays
                |> List.sum
    in
    tr []
        [ td [] []
        , td [] []
        , td [] [ h2 [ class "text-right" ] [ text "Total:" ] ]
        , td [] [ h4 [] [ text (toString total) ] ]
        ]


playerForm : Model -> Html Msg
playerForm model =
    Html.form [ onSubmit Save ]
        [ div [ class "row mb-5" ]
            [ input
                [ type_ "text"
                , placeholder "Add/Edit Player..."
                , onInput Input
                , value model.name
                , class "form-control col"
                , attribute "id" "inputDefault"
                ]
                []
            , button [ class "btn btn-primary", type_ "submit" ] [ text "Save" ]
            , button [ class "btn btn-outline-danger", type_ "button", onClick Cancel ] [ text "Cancel" ]
            ]
        ]


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = initModel
        , view = view
        , update = update
        }
