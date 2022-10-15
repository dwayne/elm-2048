module Test.App.Lib.Random exposing (suite)

import App.Lib.Random as R
import Expect
import Fuzz
import Random
import Set
import Test exposing (..)


suite : Test
suite =
    describe "App.Lib.Random"
        [ selectSuite
        , selectAtMostNSuite
        ]


selectSuite : Test
selectSuite =
    describe "select"
        [ fuzz Fuzz.int "it returns Nothing for the empty list" <|
            \s ->
                Random.step (R.select []) (Random.initialSeed s)
                    |> Tuple.first
                    |> Expect.equal ( Nothing, [] )
        , let
            fuzzer =
                Fuzz.tuple ( Fuzz.int, Fuzz.int )
          in
          fuzz fuzzer "it returns the only element from the singleton list" <|
            \( x, s ) ->
                Random.step (R.select [ x ]) (Random.initialSeed s)
                    |> Tuple.first
                    |> Expect.equal ( Just x, [] )
        , let
            fuzzer =
                Fuzz.tuple ( Fuzz.list Fuzz.int, Fuzz.int )

            toSet ( maybeX, list ) =
                Set.fromList <|
                    case maybeX of
                        Nothing ->
                            list

                        Just x ->
                            x :: list
          in
          fuzz fuzzer "it returns a random element from the list" <|
            \( list, s ) ->
                Random.step (R.select list) (Random.initialSeed s)
                    |> Tuple.first
                    |> toSet
                    |> Expect.equal (Set.fromList list)
        , test "it returns a random element from the list [1, 2, 3, 4, 5]" <|
            \_ ->
                Random.step (R.select [ 1, 2, 3, 4, 5 ]) (Random.initialSeed 10)
                    |> Tuple.first
                    |> Expect.equal ( Just 4, [ 1, 2, 3, 5 ] )
        ]


selectAtMostNSuite : Test
selectAtMostNSuite =
    describe "selectAtMostN"
        [ let
            fuzzer =
                Fuzz.tuple ( Fuzz.int, Fuzz.int )
          in
          fuzz fuzzer "it returns [] for the empty list" <|
            \( n, s ) ->
                Random.step (R.selectAtMostN n []) (Random.initialSeed s)
                    |> Tuple.first
                    |> Expect.equal ( [], [] )
        , let
            fuzzer =
                Fuzz.tuple3 ( Fuzz.int, Fuzz.intRange 1 5, Fuzz.int )
          in
          fuzz fuzzer "it returns the only element from the singleton list" <|
            \( x, n, s ) ->
                Random.step (R.selectAtMostN n [ x ]) (Random.initialSeed s)
                    |> Tuple.first
                    |> Expect.equal ( [ x ], [] )
        , test "it returns two random elements from the list [1, 2, 3, 4, 5]" <|
            \_ ->
                Random.step (R.selectAtMostN 2 [ 1, 2, 3, 4, 5 ]) (Random.initialSeed 20)
                    |> Tuple.first
                    |> Expect.equal ( [ 5, 3 ], [ 1, 2, 4 ] )
        ]
