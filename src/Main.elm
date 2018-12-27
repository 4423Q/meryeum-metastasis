module Main exposing (Model, main)

import Browser
import Html exposing (Html, div, text)
import Random
import Student


main =
    Browser.element { init = init, update = update, subscriptions = subscriptions, view = view }


type Model
    = Stdt Student.Student
    | None


init : () -> ( Model, Cmd Msg )
init _ =
    ( None, Random.generate NewStudent (Student.newStudent (Student.StudentBody [] 0)) )


type Msg
    = NoOp
    | NewStudent Student.Student


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        NewStudent x ->
            ( Stdt x, Cmd.none )


viewStats { fight, hot, sharp, extra } =
    let
        mapper =
            \( x, y ) -> div [] [ text (x ++ String.fromInt y) ]
    in
    div []
        (List.map
            mapper
            [ ( "fight - ", fight )
            , ( "hot - ", hot )
            , ( "sharp - ", sharp )
            , ( "extra - ", extra )
            ]
        )


view : Model -> Html Msg
view y =
    case y of
        Stdt x ->
            let
                stats =
                    Student.getStats x
            in
            div []
                [ div [] [ text ("Name: " ++ Student.getName x) ]
                , viewStats stats
                ]

        _ ->
            div [] [ text "Nah" ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
