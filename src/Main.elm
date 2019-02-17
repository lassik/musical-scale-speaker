module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Array
import Browser
import Debug
import Dict
import Html exposing (div, input, label, option, select, span, text)
import Html.Attributes exposing (selected, type_, value)
import Html.Events exposing (onClick, onInput)
import Midi
import Speech
import Util
import WebMidi


type alias Model =
    { tonic : Int, speak : Bool, message : Midi.Message }


type Msg
    = SetTonic String
    | ToggleSpeak
    | MidiMessage WebMidi.Message


type Scale
    = Scale (List Int) Int


noteNames =
    Array.fromList
        [ "C"
        , "Db"
        , "D"
        , "Eb"
        , "E"
        , "F"
        , "F#"
        , "G"
        , "Ab"
        , "A"
        , "Bb"
        , "B"
        ]


minorIntervals =
    [ 2, 1, 2, 2, 1, 2 ]


buildLookupX semitone scaleDegree oldIntervals oldLookup =
    let
        lookup =
            Dict.insert (modBy 12 semitone) scaleDegree oldLookup
    in
    case oldIntervals of
        [] ->
            lookup

        interval :: intervals ->
            buildLookupX (semitone + interval) (scaleDegree + 1) intervals lookup


buildLookup : Scale -> Dict.Dict Int Int
buildLookup (Scale intervals tonic) =
    buildLookupX tonic 0 intervals Dict.empty


semitoneToScaleDegree : Scale -> Int -> Maybe Int
semitoneToScaleDegree scale semitone =
    Dict.get semitone (buildLookup scale)


init : () -> ( Model, Cmd Msg )
init flags =
    ( { tonic = 0, speak = False, message = Midi.Unknown }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    WebMidi.onMidiMessage MidiMessage


speechCommand model =
    if not model.speak then
        Cmd.none

    else
        case model.message of
            Midi.NoteOn pitch ->
                let
                    scale =
                        Scale minorIntervals model.tonic
                in
                case semitoneToScaleDegree scale (modBy 12 pitch) of
                    Just scaleDegree ->
                        Speech.speak (String.fromInt (1 + scaleDegree))

                    Nothing ->
                        Cmd.none

            _ ->
                Cmd.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetTonic tonic ->
            ( { model | tonic = Util.indexDefault tonic model.tonic }
            , Cmd.none
            )

        ToggleSpeak ->
            ( { model | speak = not model.speak }
            , Cmd.none
            )

        MidiMessage webMidiMessage ->
            let
                message =
                    Midi.parseMessage webMidiMessage.midiEvent

                newModel =
                    { model | message = message }
            in
            ( newModel, speechCommand newModel )


noteOptions selectedIndex =
    Array.indexedMap
        (\index noteName ->
            option
                ([ value (String.fromInt index) ]
                    ++ (if index == selectedIndex then
                            [ selected True ]

                        else
                            []
                       )
                )
                [ text noteName ]
        )
        noteNames
        |> Array.toList


view : Model -> Html.Html Msg
view model =
    div []
        [ text "Key: "
        , select [ onInput SetTonic ] (noteOptions model.tonic)
        , label []
            [ input [ type_ "checkbox", onClick ToggleSpeak ] []
            , text "Speak"
            ]
        ]


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
