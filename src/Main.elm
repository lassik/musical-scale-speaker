module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Browser
import Debug
import Dict
import Html
import Midi
import Speech
import WebMidi


type alias Model =
    Midi.Message


type Msg
    = MidiMessage WebMidi.Message


type Scale
    = Scale (List Int) Int


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
    ( Midi.Unknown, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    WebMidi.onMidiMessage MidiMessage


fMinor =
    Scale minorIntervals 5


speechCommand message =
    case message of
        Midi.NoteOn pitch ->
            case semitoneToScaleDegree fMinor (modBy 12 pitch) of
                Just scaleDegree ->
                    Speech.speak (String.fromInt (1 + scaleDegree))

                Nothing ->
                    Cmd.none

        _ ->
            Cmd.none


update : Msg -> Model -> ( Model, Cmd Msg )
update (MidiMessage webMidiMessage) model =
    let
        message =
            Midi.parseMessage webMidiMessage.midiEvent
    in
    ( message, speechCommand message )


view : Model -> Html.Html Msg
view model =
    model |> Debug.toString |> Html.text


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
