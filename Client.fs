namespace ReDice

open WebSharper
open WebSharper.UI
open WebSharper.UI.Html
open WebSharper.UI.Client

[<JavaScript>]
module Client =

    type DiceType =
        | D4
        | D6
        | D8
        | D10
        | D12
        | D20

    let diceTypeToText dice =
        match dice with
        | D4 -> "d4"
        | D6 -> "d6"
        | D8 -> "d8"
        | D10 -> "d10"
        | D12 -> "d12"
        | D20 -> "d20"

    let diceTypes =
        [ D20; D12; D10; D8; D6; D4 ]

    let diceCounts =
        [ 2; 1; 3; 4; 5; 6 ]

    let diceTypeOption dice =
        option [ attr.value (diceTypeToText dice) ] [
            text (diceTypeToText dice)
        ]

    let diceCountOption count =
        option [ attr.value (string count) ] [
            text (string count)
        ]

    [<SPAEntryPoint>]
    let Main () =
        div [ attr.``class`` "min-h-screen bg-gray-900 text-white flex flex-col items-center p-6" ] [

            h1 [ attr.``class`` "text-4xl font-bold mb-8" ] [
                text "ReDice 🎲"
            ]

            div [ attr.``class`` "bg-gray-800 p-6 rounded-2xl w-full max-w-md" ] [

                p [ attr.``class`` "text-sm text-gray-400 mb-2" ] [
                    text "Dice Type"
                ]

                select [ attr.``class`` "mb-4 w-full bg-gray-700 p-3 rounded-xl text-white" ] [
                    for dice in diceTypes do
                        diceTypeOption dice
                ]

                p [ attr.``class`` "text-sm text-gray-400 mb-2" ] [
                    text "Number of Dice"
                ]

                select [ attr.``class`` "mb-6 w-full bg-gray-700 p-3 rounded-xl text-white" ] [
                    for count in diceCounts do
                        diceCountOption count
                ]

                button [
                    attr.``class`` "w-full py-3 bg-purple-600 rounded-xl hover:bg-purple-700 font-semibold"
                ] [
                    text "ROLL DICE"
                ]
            ]

            div [ attr.``class`` "mt-10 text-center" ] [

                p [ attr.``class`` "text-gray-400" ] [
                    text "Last Roll"
                ]

                h2 [ attr.``class`` "text-5xl font-bold mt-2" ] [
                    text "30"
                ]

                p [ attr.``class`` "text-gray-500 mt-2" ] [
                    text "20 + 10"
                ]
            ]
        ]
        |> Doc.RunById "main"