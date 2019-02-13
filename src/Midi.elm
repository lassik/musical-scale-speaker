module Midi exposing (Message(..), parseMessage)

import Bitwise


type Message
    = NoteOn Int
    | NoteOff Int
    | Unknown


parseMessage : String -> Message
parseMessage byteString =
    case byteString |> String.toList |> List.map Char.toCode of
        firstByte :: secondByte :: thirdByte :: [] ->
            let
                message =
                    Bitwise.and 0xF0 firstByte
            in
            case message of
                0x80 ->
                    let
                        pitch =
                            secondByte
                    in
                    NoteOff pitch

                0x90 ->
                    let
                        pitch =
                            secondByte

                        velocity =
                            thirdByte
                    in
                    if velocity == 0 then
                        NoteOff pitch

                    else
                        NoteOn pitch

                _ ->
                    Unknown

        _ ->
            Unknown
