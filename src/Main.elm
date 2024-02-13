module Main exposing (main)

import Array exposing (Array)
import Browser
import Browser.Events as Events
import Html exposing (Html, div, span, text)
import Html.Attributes exposing (attribute)
import Html.Events exposing (onClick)
import Json.Decode as Decode
import Random



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , subscriptions = subscriptions
        , update = update
        }



-- MODEL


type GuessedLetterResult
    = Incorrect
    | NearlyCorrect
    | Correct


type GuessedLetter
    = GuessedLetter Char GuessedLetterResult


type Round
    = Round (Array GuessedLetter)


type GameOutcome
    = Won
    | Lost
    | Playing


type alias Model =
    { lettersEntered : Array Char
    , rounds : Array Round
    , outcome : GameOutcome
    , lettersToGuess : List Char
    }


numRounds : Int
numRounds =
    6


initialState : Model
initialState =
    { lettersEntered = Array.empty
    , rounds = Array.empty
    , outcome = Playing
    , lettersToGuess = []
    }


init : flags -> ( Model, Cmd Msg )
init _ =
    ( initialState
    , Random.generate NewGame randomWord
    )



-- UPDATE


type Msg
    = NewGame String
    | Letter Char
    | Backspace
    | Enter
    | NoOp


possibleWords : Array String
possibleWords =
    Array.fromList
        [ "apple"
        , "arise"
        , "drink"
        , "field"
        , "gloss"
        , "grape"
        , "grape"
        , "kiwis"
        , "lemon"
        , "mango"
        , "melon"
        , "olive"
        , "peach"
        , "pears"
        , "peony"
        , "plums"
        , "quail"
        , "radar"
        , "salsa"
        , "swine"
        , "tacos"
        , "vodka"
        , "yield"
        ]


randomWord : Random.Generator String
randomWord =
    let
        index =
            Random.int 0 (Array.length possibleWords)

        mapper i =
            Array.get i possibleWords |> Maybe.withDefault "salsa"
    in
    Random.map mapper index


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ lettersEntered, rounds, outcome, lettersToGuess } as state) =
    let
        newState =
            case outcome of
                Won ->
                    state

                Lost ->
                    state

                Playing ->
                    case msg of
                        NewGame word ->
                            { state | lettersToGuess = String.toList word }

                        Letter l ->
                            let
                                newLettersEntered =
                                    if Array.length lettersEntered < 5 then
                                        Array.push l lettersEntered

                                    else
                                        lettersEntered
                            in
                            { state | lettersEntered = newLettersEntered }

                        Enter ->
                            if Array.length lettersEntered == 5 then
                                let
                                    roundLetters =
                                        List.map2
                                            (\entered toGuess ->
                                                let
                                                    result =
                                                        if entered == toGuess then
                                                            Correct

                                                        else if List.member entered lettersToGuess then
                                                            NearlyCorrect

                                                        else
                                                            Incorrect
                                                in
                                                GuessedLetter entered result
                                            )
                                            (Array.toList lettersEntered)
                                            lettersToGuess

                                    won =
                                        List.all
                                            (\(GuessedLetter _ r) ->
                                                case r of
                                                    Correct ->
                                                        True

                                                    _ ->
                                                        False
                                            )
                                            roundLetters

                                    newRounds =
                                        Array.push (Round (Array.fromList roundLetters)) rounds

                                    newOutcome =
                                        if won then
                                            Won

                                        else if Array.length newRounds == numRounds then
                                            Lost

                                        else
                                            Playing
                                in
                                { state | lettersEntered = Array.empty, rounds = newRounds, outcome = newOutcome }

                            else
                                state

                        Backspace ->
                            let
                                newLettersEntered =
                                    if Array.isEmpty lettersEntered then
                                        lettersEntered

                                    else
                                        Array.slice 0 -1 lettersEntered
                            in
                            { state | lettersEntered = newLettersEntered }

                        NoOp ->
                            state
    in
    ( newState
    , Cmd.none
    )



-- VIEW


viewLetterKey : Char -> Html Msg
viewLetterKey l =
    div
        [ attribute "class" "key"
        , onClick (Letter (Char.toLower l))
        ]
        [ text (String.fromChar l) ]


viewBackspaceKey : Html Msg
viewBackspaceKey =
    div
        [ attribute "class" "key backspace"
        , onClick Backspace
        ]
        [ text "⌫" ]


viewEnterKey : Html Msg
viewEnterKey =
    div
        [ attribute "class" "key enter"
        , onClick Enter
        ]
        [ text "↵" ]


viewTopRow : Html Msg
viewTopRow =
    div [ attribute "class" "keyboard-row top-row" ]
        [ viewLetterKey 'Q', viewLetterKey 'W', viewLetterKey 'E', viewLetterKey 'R', viewLetterKey 'T', viewLetterKey 'Y', viewLetterKey 'U', viewLetterKey 'I', viewLetterKey 'O', viewLetterKey 'P', viewBackspaceKey ]


viewMiddleRow : Html Msg
viewMiddleRow =
    div [ attribute "class" "keyboard-row middle-row" ]
        [ viewLetterKey 'A', viewLetterKey 'S', viewLetterKey 'D', viewLetterKey 'F', viewLetterKey 'G', viewLetterKey 'H', viewLetterKey 'J', viewLetterKey 'K', viewLetterKey 'L', viewEnterKey ]


viewBottomRow : Html Msg
viewBottomRow =
    div [ attribute "class" "keyboard-row bottom-row" ]
        [ viewLetterKey 'Z', viewLetterKey 'X', viewLetterKey 'C', viewLetterKey 'V', viewLetterKey 'B', viewLetterKey 'N', viewLetterKey 'M' ]


viewEmptyCell : Html msg
viewEmptyCell =
    div [ attribute "class" "cell empty" ] []


viewGuessedLetter : GuessedLetter -> Html msg
viewGuessedLetter (GuessedLetter l r) =
    let
        addClass =
            case r of
                Correct ->
                    "correct"

                NearlyCorrect ->
                    "nearly-correct"

                Incorrect ->
                    "incorrect"

        cls =
            String.append "cell " addClass
    in
    div [ attribute "class" cls ] [ span [] [ text (String.fromChar (Char.toUpper l)) ] ]


viewRoundRow : Round -> Html msg
viewRoundRow (Round guessedLetters) =
    let
        cells =
            Array.map viewGuessedLetter guessedLetters |> Array.toList
    in
    div [ attribute "class" "row" ] cells


viewEnteredLetter : Char -> Html msg
viewEnteredLetter l =
    div [ attribute "class" "cell" ] [ span [] [ text (String.fromChar (Char.toUpper l)) ] ]


viewLettersEnteredRow : Array Char -> Html msg
viewLettersEnteredRow lettersEntered =
    let
        cells =
            Array.map viewEnteredLetter lettersEntered

        paddingLength =
            5 - Array.length cells

        padding =
            if paddingLength > 0 then
                List.repeat paddingLength viewEmptyCell

            else
                []

        combined =
            List.concat [ Array.toList cells, padding ]
    in
    div [ attribute "class" "row" ] combined


viewEmptyRow : Html msg
viewEmptyRow =
    div [ attribute "class" "row" ] (List.repeat 5 viewEmptyCell)


viewRows : Array Round -> Array Char -> List (Html msg)
viewRows rounds lettersEntered =
    let
        viewableRounds =
            Array.map viewRoundRow rounds |> Array.toList

        viewableEntered =
            if not (Array.isEmpty lettersEntered) then
                [ viewLettersEnteredRow lettersEntered ]

            else
                []

        paddingLength =
            numRounds - List.length viewableRounds - List.length viewableEntered

        padding =
            if paddingLength > 0 then
                List.repeat paddingLength viewEmptyRow

            else
                []
    in
    List.concat [ viewableRounds, viewableEntered, padding ]


viewOutcome : List Char -> GameOutcome -> Html msg
viewOutcome lettersToGuess outcome =
    case outcome of
        Won ->
            div [ attribute "class" "outcome won" ] [ text "You won!" ]

        Lost ->
            let
                wordToGuess =
                    String.fromList lettersToGuess |> String.toUpper
            in
            div [ attribute "class" "outcome lost" ] [ text ("You lost! The word was " ++ wordToGuess) ]

        Playing ->
            div [] []


view : Model -> Html Msg
view { rounds, lettersEntered, lettersToGuess, outcome } =
    div [ attribute "class" "app-main" ]
        [ div [ attribute "class" "rows" ]
            (viewRows rounds lettersEntered)
        , viewOutcome lettersToGuess outcome
        , div
            [ attribute "class" "keyboard" ]
            [ viewTopRow
            , viewMiddleRow
            , viewBottomRow
            ]
        ]



-- SUBSCRIPTIONS


toKey : String -> Msg
toKey string =
    case String.uncons string of
        Just ( char, "" ) ->
            Letter (Char.toLower char)

        _ ->
            if string == "Backspace" then
                Backspace

            else if string == "Enter" then
                Enter

            else
                NoOp


keyDecoder : Decode.Decoder Msg
keyDecoder =
    Decode.map toKey (Decode.field "key" Decode.string)


subscriptions : Model -> Sub Msg
subscriptions _ =
    Events.onKeyDown keyDecoder
