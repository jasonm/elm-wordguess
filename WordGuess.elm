module WordGuess where

import Effects exposing (Effects, Never)
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Json
import Task
import String

import WordnikApi exposing (wordnikApiKey)


-- MODEL

maxGuesses = 10

type alias Model =
    { word : String
    , guesses : List String
    }


init : (Model, Effects Action)
init =
  ( Model "Loading..." []
  , getRandomWord
  )


-- UPDATE

type Action
    = NewGame
    | GuessLetter (String)
    | NewWord (Maybe String)


update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    NewGame -> init

    NewWord maybeWord ->
      ( Model
          (Maybe.withDefault model.word maybeWord)
          model.guesses
      , Effects.none
      )

    GuessLetter letter ->
      ( Model
          model.word
          (
          if (List.member letter model.guesses) then
            model.guesses
          else
            (model.guesses ++ [letter])
          )
      , Effects.none
      )



-- VIEW

(=>) = (,)

guessedLetterOrBlank : String -> List String -> String
guessedLetterOrBlank letter guesses =
  if List.member (String.toUpper letter) guesses then
    letter
  else
    "_"


partiallyRevealedWord : String -> List String -> Html
partiallyRevealedWord word guesses =
  let
     wordLetters = String.split "" word
  in
     pre [] (List.map (\letter -> (span [] [text (" " ++ (guessedLetterOrBlank letter guesses))])) wordLetters)


hangman : String -> List String -> Html
hangman word guesses =
  let
     wordLetters = String.split "" word
     wrongGuess = \guess -> not (List.member (String.toUpper guess) (List.map String.toUpper wordLetters))
     wrongGuesses = List.filter wrongGuess guesses
     numberOfWrongGuesses = List.length wrongGuesses
  in
     div [] [text (toString numberOfWrongGuesses)]

outOfGuesses : List String -> Bool
outOfGuesses guesses =
  List.length guesses >= maxGuesses

view : Signal.Address Action -> Model -> Html
view address model =
  div [ style
          [ "width" => "200px"
          , "margin" => "0 auto"
          ] ]
    [ h2 [headerStyle] [text "the old guessy-wordy game"]

    , h3 [] [text "Word:"]
    , partiallyRevealedWord model.word model.guesses


    , h3 [] [text "Wrong guesses"]
    , hangman model.word model.guesses

    , if (List.length model.guesses) >= maxGuesses then
        div []
          [
          (text "Better luck next time!")
          , h3 [] [text "Your word:"]
          , pre [] [text model.word]
          ]
      else
        div []
          [ h3 [] [text "Click a letter to guess it:"]
          , div []
            (List.map
              (\letter -> button [ onClick address (GuessLetter letter) ] [text letter])
              [ "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z", "-" ])

          , h3 [] [text "Guesses:"]
          , div [] (List.map (\guess -> span [] [text guess]) (List.sort model.guesses))
          , text ((toString (maxGuesses - (List.length model.guesses))) ++ " left")
          ]


    , button [ onClick address NewGame ] [ text "This is hard, do a different one!" ]
    ]


headerStyle : Attribute
headerStyle =
  style
    [ "width" => "200px"
    , "text-align" => "center"
    ]

subheaderStyle : Attribute
subheaderStyle =
  style
    [ "width" => "200px"
    , "text-align" => "center"
    ]

-- EFFECTS

-- From http://stackoverflow.com/questions/26622708/how-to-get-random-word-using-wordnik-api-in-javascript
wordnikUrl = "http://api.wordnik.com/v4/words.json/randomWords?hasDictionaryDef=true&minCorpusCount=0&minLength=5&maxLength=15&limit=1&api_key=" ++ wordnikApiKey

decodeWordnikResponse : Json.Decoder String
decodeWordnikResponse =
  -- API response is JSON in this structure:
  -- [
  --   {
  --     word: 'your-new-word'
  --   }
  -- ]
  Json.tuple1 identity (Json.at ["word"] Json.string)

getRandomWord : Effects Action
getRandomWord = Http.get decodeWordnikResponse wordnikUrl
    |> Task.toMaybe
    |> Task.map NewWord
    |> Effects.task
