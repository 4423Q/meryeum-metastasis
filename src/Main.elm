module Main exposing (Model, main)

import Bonds
import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Random
import String.Extra
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
    | MaybeNewStudents (Maybe StudentBody.StudentBody)
    | NewStudents StudentBody.StudentBody
    | GetNewStudents Int
    | GetNewRelative Int


getNewRelative : Model -> Int -> Cmd Msg
getNewRelative model id =
    let
        newStudentBody =
            StudentBody.addRelative id model
    in
    case newStudentBody of
        Nothing ->
            Cmd.none

        Just x ->
            Random.generate MaybeNewStudents x


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        MaybeNewStudents (Just x) ->
            ( x, Cmd.none )

        MaybeNewStudents Nothing ->
            ( model, Cmd.none )

        NewStudents x ->
            ( x, Cmd.none )

        GetNewStudents n ->
            ( model, Random.generate NewStudents (StudentBody.addStudents n model) )

        GetNewRelative id ->
            ( model, getNewRelative model id )


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


bondToString : Student.Student -> Student.Student -> Bonds.Bond -> String
bondToString source target bond =
    let
        sProns =
            Student.getPronouns source

        tName =
            Student.getName target

        sFriendly =
            Student.getGivenName source
    in
    case bond of
        Bonds.Relative Bonds.Sibling _ ->
            String.Extra.toSentenceCase sProns.pos ++ " sibling is " ++ tName ++ "."

        Bonds.Relative Bonds.Cousin _ ->
            String.Extra.toSentenceCase sProns.subj ++ " " ++ Util.isare sProns.subj ++ " " ++ tName ++ "'s cousin."

        Bonds.InLove _ ->
            String.Extra.toSentenceCase sProns.subj ++ " " ++ Util.isare sProns.subj ++ " in love with " ++ tName ++ "!"

        Bonds.Friend _ ->
            String.Extra.toSentenceCase sProns.subj ++ " " ++ Util.isare sProns.subj ++ " friends with " ++ tName ++ "."

        Bonds.Enemy _ ->
            String.Extra.toSentenceCase sProns.subj ++ " " ++ Util.deplural sProns.subj "despises" ++ " " ++ tName ++ "."

        Bonds.Rival _ ->
            tName ++ " has a rivalry with " ++ sFriendly ++ "."


renderBond : Student.Student -> ( Bonds.Bond, Student.Student ) -> Html Msg
renderBond studentSource ( bond, studentTarget ) =
    div []
        [ text <| bondToString studentSource studentTarget bond ]


renderStudent : StudentBody.StudentBody -> Student.Student -> Html Msg
renderStudent body x =
    let
        bonds =
            Student.getBonds x
                |> List.foldr
                    (\val acc ->
                        case StudentBody.getBondTarget val body of
                            Just y ->
                                ( val, y ) :: acc

                            Nothing ->
                                acc
                    )
                    []

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

                Weapon.Commander ->
                    "a Future Commander"

                Weapon.Tactician ->
                    "a Tactician in Training"

                Weapon.Special ->
                    "something special !!!"

                Weapon.Weapon wep ->
                    "a person who is also " ++ Util.ana wep ++ " " ++ wep
    in
    div []
        [ div [] [ text ("Name: " ++ Student.getName x ++ " [#" ++ String.fromInt (Student.getNumber x) ++ "]" ++ " (" ++ .subj prons ++ "/" ++ .obj prons ++ ")") ]
        , viewStats stats
        , div []
            [ text (Student.getGivenName x ++ " is " ++ weptext)
            ]
        , div [] (List.map (renderBond x) bonds)
        ]


view : Model -> Html Msg
view y =
    div []
        [ button [ onClick (GetNewStudents 1) ] [ text "NEW!!!" ]
        , div []
            (StudentBody.asList y
                |> List.sortBy (\x -> 1 - Student.getNumber x)
                |> List.map (renderStudent y)
                |> List.intersperse (div [] [ text "------" ])
            )
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
