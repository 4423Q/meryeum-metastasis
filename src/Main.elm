module Main exposing (Model, main)

import Browser
import Html exposing (Html, div, text)
import Random
import Student
import Util
import Weapon


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


viewStats { danger, hot, sharp, extra } =
    let
        mapper =
            \( x, y ) -> div [] [ text (x ++ String.fromInt y) ]
    in
    div []
        (List.map
            mapper
            [ ( "dangerous - ", danger )
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

                prons =
                    Student.getPronouns x

                weptext =
                    case Student.getWeapon x of
                        Weapon.Pilot machine ->
                            Util.ana machine ++ " " ++ machine ++ " pilot"

                        Weapon.Soldier ->
                            "some kinda soldier"

                        Weapon.Weapon wep ->
                            "a person who is also " ++ Util.ana wep ++ " " ++ wep
            in
            div []
                [ div [] [ text ("Name: " ++ Student.getName x ++ " (" ++ .subj prons ++ "/" ++ .obj prons ++ ")") ]
                , viewStats stats
                , div [] [ text (Student.getName x ++ " is " ++ weptext) ]
                ]

        _ ->
            div [] [ text "Nah" ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
