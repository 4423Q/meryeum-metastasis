module Example exposing (defProns, suite)

import Bonds
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Interactions
import Name
import Pronouns
import Random
import Student
import StudentBody
import Test exposing (..)
import Weapon


defProns =
    Pronouns.Pronoun "a" "b" "c" "d"


suite : Test
suite =
    describe "Regression tests"
        [ test "STOP COUSINS BEING ABLE TO GO ON DATES!!!!" <|
            \_ ->
                let
                    students =
                        [ ( 1, [ Bonds.Relative Bonds.Cousin 2, Bonds.InLove 3 ] )
                        , ( 2, [ Bonds.Relative Bonds.Cousin 1, Bonds.InLove 3 ] )
                        , ( 3, [ Bonds.InLove 1 ] )
                        ]
                            |> List.map
                                (\( id, bonds ) ->
                                    Student.fromRecord
                                        { num = id
                                        , name = Name.testDefault
                                        , pronouns = defProns
                                        , weapontype = Weapon.Commander
                                        , visible = { sharp = 0, danger = 0, hot = 0, extra = 0 }
                                        , invisible = { horny = 0, angry = 0, ambition = 0, diversion = 0 }
                                        , bonds = Bonds.fromList bonds
                                        , classes = []
                                        , quirks = []
                                        , value = 0
                                        , nickname = Nothing
                                        }
                                )
                in
                StudentBody.fromList students
        , test "Test case where students cannot be lustful for people with cousins" <|
            \_ ->
                let
                    students =
                        [ ( 1, [ Bonds.Relative Bonds.Sibling 2 ] )
                        , ( 2, [ Bonds.Relative Bonds.Sibling 1 ] )
                        , ( 3, [] )
                        ]
                            |> List.map
                                (\( id, bonds ) ->
                                    Student.fromRecord
                                        { num = id
                                        , name = Name.testDefault
                                        , pronouns = defProns
                                        , weapontype = Weapon.Commander
                                        , visible = { sharp = 0, danger = 0, hot = 0, extra = 0 }
                                        , invisible = { horny = 0, angry = 0, ambition = 0, diversion = 0 }
                                        , bonds = Bonds.fromList bonds
                                        , classes = []
                                        , quirks = []
                                        , value = 0
                                        , nickname = Nothing
                                        }
                                )
                in
                [ Interactions.BondsFormed 3 [ Bonds.Lustful 2 ] ]
                    |> StudentBody.applyResults (StudentBody.fromList students)
                    |> (\z -> StudentBody.getStudentById z 3)
                    |> Maybe.map Student.getBonds
                    |> Expect.equal (Just [ Bonds.Lustful 2 ])
        ]
