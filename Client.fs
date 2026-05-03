namespace ReDice

open WebSharper
open WebSharper.JavaScript
open WebSharper.UI
open WebSharper.UI.Html
open WebSharper.UI.Client
open WebSharper.Charting

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

    let selectedDice = Var.Create "d20"
    let selectedCount = Var.Create "2"

    let lastTotal = Var.Create 0
    let lastRolls = Var.Create ""

    let allRolls = Var.Create []

    let diceSides dice =
        match dice with
        | "d4" -> 4
        | "d6" -> 6
        | "d8" -> 8
        | "d10" -> 10
        | "d12" -> 12
        | "d20" -> 20
        | _ -> 6

    let countToInt count =
        match count with
        | "1" -> 1
        | "2" -> 2
        | "3" -> 3
        | "4" -> 4
        | "5" -> 5
        | "6" -> 6
        | _ -> 1

    let rollOne sides =
        int (JavaScript.Math.Random() * float sides) + 1

    let rollDice () =
        let sides = diceSides selectedDice.Value
        let count = countToInt selectedCount.Value

        let rolls =
            [ for i in 1 .. count -> rollOne sides ]

        let total = List.sum rolls
        let rollsText = rolls |> List.map string |> String.concat ", "

        lastTotal.Set total
        lastRolls.Set rollsText

        let updated = allRolls.Value @ rolls
        allRolls.Set updated

    let resetStats () =
        allRolls.Set []
        lastTotal.Set 0
        lastRolls.Set ""

    let getChartData sides rolls =
        [ for value in 1 .. sides ->
            let count =
                rolls
                |> List.filter (fun r -> r = value)
                |> List.length

            (string value, float count)
        ]

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
        div [ attr.``class`` "flex flex-col items-center p-6" ] [

            h1 [ attr.``class`` "text-4xl font-bold mb-8" ] [
                text "ReDice 🎲"
            ]

            div [ attr.``class`` "bg-gray-800 p-6 rounded-2xl w-full max-w-md" ] [

                p [ attr.``class`` "text-sm text-gray-400 mb-2" ] [
                    text "Dice Type"
                ]

                select [
                    attr.``class`` "mb-4 w-full bg-gray-700 p-3 rounded-xl text-white"
                    on.change (fun el _ -> selectedDice.Set el?value)
                ] [
                    for dice in diceTypes do
                        diceTypeOption dice
                ]

                p [ attr.``class`` "text-sm text-gray-400 mb-2" ] [
                    text "Number of Dice"
                ]

                select [
                    attr.``class`` "mb-6 w-full bg-gray-700 p-3 rounded-xl text-white"
                    on.change (fun el _ -> selectedCount.Set el?value)
                ] [
                    for count in diceCounts do
                        diceCountOption count
                ]

                button [
                    attr.``class`` "w-full py-3 bg-purple-600 rounded-xl hover:bg-purple-700 font-semibold"
                    on.click (fun _ _ -> rollDice ())
                ] [
                    text "ROLL DICE"
                ]
            ]

            div [ attr.``class`` "mt-10 text-center" ] [

                p [ attr.``class`` "text-gray-400" ] [
                    text "Last Roll"
                ]

                h2 [ attr.``class`` "text-5xl font-bold mt-2" ] [
                    Doc.BindView (fun total ->
                        text (string total)
                    ) lastTotal.View
                ]

                div [ attr.``class`` "mt-4 text-lg text-white" ] [
                    Doc.BindView (fun dice ->
                        Doc.BindView (fun count ->
                            text ("Selected: " + count + " x " + dice)
                        ) selectedCount.View
                    ) selectedDice.View
                ]

                div [ attr.``class`` "mt-4 text-sm text-gray-400" ] [
                    Doc.BindView (fun rolls ->
                        if rolls = "" then
                            text "Rolls: -"
                        else
                            text ("Rolls: " + rolls)
                    ) lastRolls.View
                ]

                div [ attr.``class`` "mt-4 text-xs text-gray-500" ] [
                    Doc.BindView (fun rolls ->
                        text ("Total rolls stored: " + string (List.length rolls))
                    ) allRolls.View
                ]
            ]

            div [ attr.``class`` "mt-10 bg-gray-800/90 border border-purple-500/20 p-6 rounded-2xl w-full max-w-xl shadow-2xl" ] [

                h2 [ attr.``class`` "text-2xl font-bold mb-4 text-center" ] [
                    text "Roll Distribution"
                ]

                Doc.BindView (fun dice ->
                    Doc.BindView (fun rolls ->

                        if List.isEmpty rolls then
                            div [ attr.``class`` "text-gray-400 text-sm text-center" ] [
                                text "No rolls yet."
                            ]
                        else
                            let sides = diceSides dice
                            let data = getChartData sides rolls

                            Chart.Bar(data)
                                .WithTitle("")
                                .WithFillColor(Color.Rgba(168, 85, 247, 0.85))
                                .WithStrokeColor(Color.Rgba(236, 72, 153, 1.0))
                            |> fun chart ->
                                Renderers.ChartJs.Render(chart, Size = Size(520, 260))

                    ) allRolls.View
                ) selectedDice.View

                div [ attr.``class`` "mt-4 flex justify-center" ] [
                    button [
                        attr.``class`` "px-4 py-2 bg-red-600 hover:bg-red-700 rounded-xl text-sm font-semibold"
                        on.click (fun _ _ -> resetStats ())
                    ] [
                        text "Reset Stats"
                    ]
                ]
            ]
        ]
        |> Doc.RunById "main"