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

    type RollEntry = {
        Dice: string
        Value: int
    }

    type RollHistoryEntry = {
        Dice: string
        Count: int
        Values: int list
        Total: int
    }

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

    let allRolls = Var.Create<RollEntry list> []
    let rollHistory = Var.Create<RollHistoryEntry list> []

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
        let dice = selectedDice.Value
        let sides = diceSides dice
        let count = countToInt selectedCount.Value

        let rolls =
            [ for i in 1 .. count -> rollOne sides ]

        let total = List.sum rolls
        let rollsText = rolls |> List.map string |> String.concat ", "

        let entries =
            rolls
            |> List.map (fun roll -> { Dice = dice; Value = roll })

        let historyEntry = {
            Dice = dice
            Count = count
            Values = rolls
            Total = total
        }

        lastTotal.Set total
        lastRolls.Set rollsText

        allRolls.Set (allRolls.Value @ entries)
        rollHistory.Set (rollHistory.Value @ [ historyEntry ])

    let resetStats () =
        allRolls.Set []
        rollHistory.Set []
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

    let rec takeFirst count items =
        match count, items with
        | 0, _ -> []
        | _, [] -> []
        | n, head :: tail -> head :: takeFirst (n - 1) tail

    let recentFourHistory (history: RollHistoryEntry list) =
        history
        |> List.rev
        |> takeFirst 4

    let rollsForDice dice (rolls: RollEntry list) =
        rolls
        |> List.filter (fun roll -> roll.Dice = dice)
        |> List.map (fun roll -> roll.Value)

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
        div [ attr.``class`` "p-4 md:p-8 xl:p-10" ] [

            div [ attr.``class`` "mb-8" ] [
                h1 [ attr.``class`` "text-3xl md:text-4xl font-bold" ] [
                    text "Home"
                ]

                p [ attr.``class`` "text-gray-400 mt-2" ] [
                    text "Roll your dice and track the most common values."
                ]
            ]

            div [ attr.``class`` "mb-8 flex flex-col sm:flex-row gap-3" ] [
                div [ attr.``class`` "px-5 py-3 rounded-2xl bg-purple-600 text-white font-semibold shadow-lg shadow-purple-900/30" ] [
                    text "Single Player"
                ]

                div [ attr.``class`` "px-5 py-3 rounded-2xl bg-gray-900 border border-orange-500/20 text-gray-400" ] [
                    text "Multi Player"
                ]
            ]

            div [ attr.``class`` "grid grid-cols-1 xl:grid-cols-2 gap-6" ] [

                div [ attr.``class`` "space-y-6" ] [

                    div [ attr.``class`` "bg-gray-900/90 border border-purple-500/20 p-6 rounded-3xl shadow-2xl" ] [
                        div [ attr.``class`` "flex items-center justify-between mb-6" ] [
                            h2 [ attr.``class`` "text-2xl font-bold" ] [
                                text "Roll Dice"
                            ]

                            div [ attr.``class`` "text-sm text-purple-300 bg-purple-500/10 px-3 py-1 rounded-full" ] [
                                text "Single Mode"
                            ]
                        ]

                        p [ attr.``class`` "text-sm text-gray-400 mb-2" ] [
                            text "Dice Type"
                        ]

                        select [
                            attr.``class`` "mb-4 w-full bg-gray-800 border border-purple-500/20 p-3 rounded-xl text-white outline-none"
                            on.change (fun el _ -> selectedDice.Set el?value)
                        ] [
                            for dice in diceTypes do
                                diceTypeOption dice
                        ]

                        p [ attr.``class`` "text-sm text-gray-400 mb-2" ] [
                            text "Number of Dice"
                        ]

                        select [
                            attr.``class`` "mb-6 w-full bg-gray-800 border border-purple-500/20 p-3 rounded-xl text-white outline-none"
                            on.change (fun el _ -> selectedCount.Set el?value)
                        ] [
                            for count in diceCounts do
                                diceCountOption count
                        ]

                        button [
                            attr.``class`` "w-full py-4 bg-gradient-to-r from-purple-600 to-pink-600 rounded-2xl hover:from-purple-700 hover:to-pink-700 font-bold tracking-wide shadow-lg shadow-purple-900/30"
                            on.click (fun _ _ -> rollDice ())
                        ] [
                            text "ROLL DICE"
                        ]
                    ]

                    div [ attr.``class`` "bg-gray-900/90 border border-purple-500/20 p-6 rounded-3xl shadow-2xl text-center" ] [
                        p [ attr.``class`` "text-gray-400 mb-2" ] [
                            text "Last Roll"
                        ]

                        h2 [ attr.``class`` "text-6xl font-bold" ] [
                            Doc.BindView (fun total ->
                                text (string total)
                            ) lastTotal.View
                        ]

                        div [ attr.``class`` "mt-4 text-lg text-white" ] [
                            Doc.BindView (fun dice ->
                                Doc.BindView (fun count ->
                                    text (count + " x " + dice)
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
                            ) rollHistory.View
                        ]
                    ]
                ]

                div [ attr.``class`` "space-y-6" ] [

                    div [ attr.``class`` "bg-gray-900/90 border border-purple-500/20 p-6 rounded-3xl shadow-2xl overflow-hidden" ] [
                        h2 [ attr.``class`` "text-2xl font-bold mb-4" ] [
                            text "Roll Distribution"
                        ]

                        Doc.BindView (fun dice ->
                            Doc.BindView (fun rolls ->

                                let filteredRolls = rollsForDice dice rolls

                                if List.isEmpty filteredRolls then
                                    div [ attr.``class`` "text-gray-400 text-sm text-center py-20" ] [
                                        text ("No " + dice + " rolls yet.")
                                    ]
                                else
                                    let sides = diceSides dice
                                    let data = getChartData sides filteredRolls

                                    div [ attr.``class`` "w-full flex justify-center overflow-hidden" ] [
                                        Chart.Bar(data)
                                            .WithTitle("")
                                            .WithFillColor(Color.Rgba(168, 85, 247, 0.85))
                                            .WithStrokeColor(Color.Rgba(236, 72, 153, 1.0))
                                        |> fun chart ->
                                            Renderers.ChartJs.Render(chart, Size = Size(420, 220))
                                    ]

                            ) allRolls.View
                        ) selectedDice.View
                    ]

                    div [ attr.``class`` "bg-gray-900/90 border border-purple-500/20 p-6 rounded-3xl shadow-2xl" ] [
                        h2 [ attr.``class`` "text-2xl font-bold mb-4" ] [
                            text "Recent Rolls"
                        ]

                        Doc.BindView (fun history ->
                            if List.isEmpty history then
                                div [ attr.``class`` "text-gray-400 text-sm" ] [
                                    text "No recent rolls yet."
                                ]
                            else
                                div [ attr.``class`` "space-y-3" ] [
                                    yield!
                                        recentFourHistory history
                                        |> List.mapi (fun index roll ->
                                            div [ attr.``class`` "flex items-center justify-between bg-gray-800/80 border border-purple-500/10 px-4 py-3 rounded-2xl" ] [
                                                div [] [
                                                    div [ attr.``class`` "text-gray-300 text-sm font-semibold" ] [
                                                        text (string roll.Count + " x " + roll.Dice)
                                                    ]

                                                    div [ attr.``class`` "text-gray-500 text-xs mt-1" ] [
                                                        text ("Values: " + (roll.Values |> List.map string |> String.concat ", "))
                                                    ]
                                                ]

                                                span [ attr.``class`` "text-xl font-bold text-purple-300" ] [
                                                    text (string roll.Total)
                                                ]
                                            ]
                                        )
                                ]
                        ) rollHistory.View
                    ]
                ]
            ]
        ]
        |> Doc.RunById "main"