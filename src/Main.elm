module Main exposing (Model, main)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Random
import Student
import StudentBody
import Util
import Weapon


main =
    Browser.element { init = init, update = update, subscriptions = subscriptions, view = view }


type alias Model =
    StudentBody.StudentBody


init : () -> ( Model, Cmd Msg )
init _ =
    ( StudentBody.empty, Random.generate NewStudents (StudentBody.addStudents 6 StudentBody.empty) )


type Msg
    = NoOp
    | NewStudents StudentBody.StudentBody
    | GetNewStudents Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        NewStudents x ->
            ( x, Cmd.none )

        GetNewStudents n ->
            ( model, Random.generate NewStudents (StudentBody.addStudents n model) )


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


renderStudent : Student.Student -> Html Msg
renderStudent x =
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
        [ div [] [ text ("Name: " ++ Student.getName x ++ " [#" ++ String.fromInt (Student.getNumber x) ++ "]" ++ " (" ++ .subj prons ++ "/" ++ .obj prons ++ ")") ]
        , viewStats stats
        , div [] [ text (Student.getGivenName x ++ " is " ++ weptext) ]
        ]


view : Model -> Html Msg
view y =
    div []
        [ button [ onClick (GetNewStudents 1) ] [ text "NEW!!!" ]
        , div [] (List.map renderStudent (StudentBody.asList y) |> List.intersperse (div [] [ text "------" ]))
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
