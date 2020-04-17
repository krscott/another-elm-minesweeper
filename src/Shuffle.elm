module Shuffle exposing (shuffle)

{-| Custom implementation of Random.List.shuffle. Possibly slower, but seems
to have a more uniform distribution.
-}

import List.Extra exposing (getAt, removeAt)
import Random exposing (Generator)


popAt : Int -> List a -> Maybe ( a, List a )
popAt i list =
    getAt i list
        |> Maybe.map (\x -> ( x, removeAt i list ))


shuffle : List a -> Generator (List a)
shuffle list =
    case list of
        [] ->
            Random.constant []

        [ x ] ->
            Random.constant [ x ]

        _ :: _ ->
            Random.int 0 (List.length list - 1)
                |> Random.andThen
                    (\i ->
                        case popAt i list of
                            Nothing ->
                                -- Guaranteed not possible from parent case block
                                Random.constant []

                            Just ( next, remaining ) ->
                                Random.map (\tail -> next :: tail) (shuffle remaining)
                    )
