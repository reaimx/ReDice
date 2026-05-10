namespace ReDice

open WebSharper
open WebSharper.JavaScript
open WebSharper.UI
open WebSharper.UI.Html
open WebSharper.UI.Client

[<JavaScript>]
module Client =

    type DiceType =
        | D4 | D6 | D8 | D10 | D12 | D20 | D100

    type DiceExpression = {
        Count: int
        Dice: DiceType
    }

    type RollEntry = {
        Dice: DiceType
        Value: int
    }

    type RollHistoryEntry = {
        Roll: DiceExpression
        Values: int list
        Total: int
    }

    type ChartItem = {
        Label: string
        Count: int
        IsGap: bool
    }

    let diceTypeToText dice =
        match dice with
        | D4 -> "D4"
        | D6 -> "D6"
        | D8 -> "D8"
        | D10 -> "D10"
        | D12 -> "D12"
        | D20 -> "D20"
        | D100 -> "D100"

    let diceTextToTypeOption text =
        match text with
        | "D4" -> Some D4
        | "D6" -> Some D6
        | "D8" -> Some D8
        | "D10" -> Some D10
        | "D12" -> Some D12
        | "D20" -> Some D20
        | "D100" -> Some D100
        | _ -> None

    let countToIntOption count =
        match count with
        | "1" -> Some 1
        | "2" -> Some 2
        | "3" -> Some 3
        | "4" -> Some 4
        | "5" -> Some 5
        | "6" -> Some 6
        | _ -> None

    let diceTypes =
        [ D100; D20; D12; D10; D8; D6; D4 ]

    let diceCounts =
        [ 1; 2; 3; 4; 5; 6 ]

    let selectedDice = Var.Create<DiceType option> None
    let selectedCount = Var.Create<int option> None

    let lastTotal = Var.Create 0
    let lastRolls = Var.Create ""

    let allRolls = Var.Create<RollEntry list> []
    let rollHistory = Var.Create<RollHistoryEntry list> []

    let diceIconPath dice =
        "/icons/" + diceTypeToText dice + ".svg"

    let selectedDiceIconPath diceOption =
        match diceOption with
        | Some dice -> diceIconPath dice
        | None -> "/icons/dice-empty.svg"

    let diceIconSmall diceOption =
        img [
            attr.src (selectedDiceIconPath diceOption)
            attr.``class`` "w-7 h-7 object-contain"
        ] []

    let diceIconMedium diceOption =
        img [
            attr.src (selectedDiceIconPath diceOption)
            attr.``class`` "w-14 h-14 object-contain"
        ] []

    let rollButtonIcon () =
        img [
            attr.src "/icons/dice-empty.svg"
            attr.``class`` "w-7 h-7 object-contain"
        ] []

    let diceSides dice =
        match dice with
        | D4 -> 4
        | D6 -> 6
        | D8 -> 8
        | D10 -> 10
        | D12 -> 12
        | D20 -> 20
        | D100 -> 100

    let rollOne sides =
        int (JavaScript.Math.Random() * float sides) + 1

    let rollDice () =
        match selectedDice.Value, selectedCount.Value with
        | Some dice, Some count ->
            let sides = diceSides dice

            let expression = {
                Count = count
                Dice = dice
            }

            let rolls =
                [ for i in 1 .. count -> rollOne sides ]

            let total = List.sum rolls
            let rollsText = rolls |> List.map string |> String.concat ", "

            let entries =
                rolls |> List.map (fun roll -> { Dice = dice; Value = roll })

            let historyEntry = {
                Roll = expression
                Values = rolls
                Total = total
            }

            lastTotal.Set total
            lastRolls.Set rollsText
            allRolls.Set (allRolls.Value @ entries)
            rollHistory.Set (rollHistory.Value @ [ historyEntry ])

        | _ -> ()

    let chartItem label count =
        { Label = label; Count = count; IsGap = false }

    let chartGap =
        { Label = ""; Count = 0; IsGap = true }

    let getChartData sides rolls =
        [ for value in 1 .. sides ->
            let count =
                rolls
                |> List.filter (fun r -> r = value)
                |> List.length

            chartItem (string value) count
        ]

    let d100TensLabel value =
        if value = 100 then
            "00"
        else
            let tens = (value / 10) * 10
            if tens = 0 then "00" else string tens

    let d100OnesLabel value =
        if value = 100 then
            "0"
        else
            string (value % 10)

    let countByLabel labels labelFunction rolls =
        labels
        |> List.map (fun label ->
            let count =
                rolls
                |> List.filter (fun value -> labelFunction value = label)
                |> List.length

            chartItem label count
        )

    let getD100ChartData rolls =
        let tensLabels =
            [ "10"; "20"; "30"; "40"; "50"; "60"; "70"; "80"; "90"; "00" ]

        let onesLabels =
            [ "1"; "2"; "3"; "4"; "5"; "6"; "7"; "8"; "9"; "0" ]

        let tensData =
            countByLabel tensLabels d100TensLabel rolls

        let onesData =
            countByLabel onesLabels d100OnesLabel rolls

        tensData @ [ chartGap ] @ onesData

    let rec takeFirst count items =
        match count, items with
        | 0, _ -> []
        | _, [] -> []
        | n, head :: tail -> head :: takeFirst (n - 1) tail

    let recentFiveHistory (history: RollHistoryEntry list) =
        history |> List.rev |> takeFirst 5

    let rollsForDice (dice: DiceType) (rolls: RollEntry list) =
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

    let neonBarChart items =
        let realItems =
            items |> List.filter (fun item -> not item.IsGap)

        let maxCount =
            realItems
            |> List.map (fun item -> item.Count)
            |> List.max

        let chartMax =
            if maxCount < 5 then 5 else maxCount

        let yLabels =
            [
                ("0%", chartMax)
                ("25%", chartMax * 3 / 4)
                ("50%", chartMax / 2)
                ("75%", chartMax / 4)
                ("100%", 0)
            ]

        div [ attr.``class`` "h-[245px] w-full grid grid-cols-[42px_1fr] gap-2 pt-4 pb-2 pr-6" ] [

            div [ attr.``class`` "relative h-[190px]" ] [
                yield!
                    yLabels
                    |> List.map (fun (topValue, label) ->
                        div [
                            attr.``class`` "absolute right-1 -translate-y-1/2 text-[10px] text-gray-500 leading-none"
                            attr.style ("top: " + topValue + ";")
                        ] [
                            text (string label)
                        ]
                    )
            ]

            div [ attr.``class`` "flex flex-col" ] [
                div [ attr.``class`` "relative h-[190px]" ] [

                    yield!
                        yLabels
                        |> List.map (fun (topValue, label) ->
                            div [
                                attr.``class`` "absolute left-0 right-0 border-t border-purple-500/10"
                                attr.style ("top: " + topValue + ";")
                            ] []
                        )

                    div [ attr.``class`` "absolute inset-0 flex items-end justify-start gap-1.5 px-1" ] [
                        yield!
                            items
                            |> List.map (fun item ->
                                if item.IsGap then
                                    div [ attr.``class`` "w-[18px] shrink-0 flex items-end justify-center" ] [
                                        div [ attr.``class`` "w-px h-[170px] bg-purple-500/20" ] []
                                    ]
                                else
                                    let height =
                                        if item.Count = 0 then
                                            6
                                        else
                                            int ((float item.Count / float chartMax) * 170.0)

                                    div [ attr.``class`` "flex-1 max-w-[34px] flex items-end" ] [
                                        div [
                                            attr.``class`` "w-full rounded-t-xl bg-gradient-to-t from-purple-700 to-pink-500 border border-purple-300/40 shadow-[0_0_14px_rgba(168,85,247,0.35)]"
                                            attr.style ("height: " + string height + "px; opacity: " + (if item.Count = 0 then "0.22" else "1") + ";")
                                        ] []
                                    ]
                            )
                    ]
                ]

                div [ attr.``class`` "h-6 mt-2 flex items-start justify-start gap-1.5 px-1" ] [
                    yield!
                        items
                        |> List.map (fun item ->
                            if item.IsGap then
                                div [ attr.``class`` "w-[18px] shrink-0" ] []
                            else
                                let labelClass =
                                    if item.Label.Length > 2 then
                                        "flex-1 max-w-[34px] text-center text-[9px] text-gray-300 leading-none"
                                    else
                                        "flex-1 max-w-[34px] text-center text-[10px] text-gray-300 leading-none"

                                div [ attr.``class`` labelClass ] [
                                    text item.Label
                                ]
                        )
                ]
            ]
        ]

    let customDistributionChart data =
        neonBarChart data

    let customD100DistributionChart rolls =
        rolls
        |> getD100ChartData
        |> neonBarChart

    [<SPAEntryPoint>]
    let Main () =
        div [ attr.``class`` "w-full" ] [
            div [ attr.``class`` "max-w-[1420px] mx-auto p-4 md:p-6 xl:p-8" ] [

                div [ attr.``class`` "mb-7 flex flex-col lg:flex-row lg:items-center lg:justify-between gap-5" ] [
                    div [] [
                        h1 [ attr.``class`` "text-3xl md:text-4xl font-bold text-purple-300 uppercase tracking-wide" ] [
                            text "Single Player Mode"
                        ]

                        p [ attr.``class`` "text-gray-400 mt-2" ] [
                            text "Roll your dice and track your adventure."
                        ]
                    ]

                    div [ attr.``class`` "flex items-center gap-3" ] [
                        div [ attr.``class`` "flex bg-gray-900 border border-purple-500/20 rounded-2xl p-1 shadow-lg shadow-purple-950/20" ] [
                            button [ attr.``class`` "px-6 py-3 rounded-xl bg-purple-600 text-white font-semibold shadow-lg shadow-purple-900/30" ] [
                                text "Single Player"
                            ]

                            button [ attr.``class`` "px-6 py-3 rounded-xl text-gray-400 font-semibold" ] [
                                text "Multi Player"
                            ]
                        ]

                        button [ attr.``class`` "w-11 h-11 rounded-xl bg-gray-900 border border-purple-500/20 text-purple-300 text-xl" ] [
                            text "☼"
                        ]
                    ]
                ]

                div [ attr.``class`` "grid grid-cols-1 xl:grid-cols-2 gap-6 items-start" ] [

                    div [ attr.``class`` "space-y-6" ] [

                        div [ attr.``class`` "h-[774px] bg-gray-900/90 border border-purple-500/30 p-6 rounded-3xl shadow-2xl shadow-purple-950/30 overflow-hidden flex flex-col" ] [
                            div [ attr.``class`` "mb-6" ] [
                                h2 [ attr.``class`` "text-2xl font-bold uppercase" ] [
                                    text "Roll Dice"
                                ]

                                p [ attr.``class`` "text-sm text-gray-400 mt-1" ] [
                                    text "Choose your dice setup and start rolling."
                                ]
                            ]

                            div [ attr.``class`` "grid grid-cols-1 md:grid-cols-2 gap-4 mb-5" ] [
                                div [ attr.``class`` "bg-gray-800/80 border border-purple-500/20 rounded-2xl p-4" ] [
                                    p [ attr.``class`` "text-xs uppercase tracking-wide text-gray-500 mb-3" ] [
                                        text "Dice Type"
                                    ]

                                    div [ attr.``class`` "relative" ] [
                                        div [ attr.``class`` "absolute left-4 top-1/2 -translate-y-1/2 pointer-events-none z-10" ] [
                                            Doc.BindView (fun diceOption ->
                                                diceIconSmall diceOption
                                            ) selectedDice.View
                                        ]

                                        select [
                                            attr.``class`` "w-full h-12 bg-gray-950 border border-purple-500/20 pl-14 pr-4 rounded-xl text-white outline-none"
                                            on.change (fun el _ ->
                                                selectedDice.Set (diceTextToTypeOption el?value)
                                            )
                                        ] [
                                            option [
                                                attr.value ""
                                                attr.selected "selected"
                                            ] [
                                                text "Select dice"
                                            ]

                                            for dice in diceTypes do
                                                diceTypeOption dice
                                        ]
                                    ]
                                ]

                                div [ attr.``class`` "bg-gray-800/80 border border-purple-500/20 rounded-2xl p-4" ] [
                                    p [ attr.``class`` "text-xs uppercase tracking-wide text-gray-500 mb-3" ] [
                                        text "Number of Dice"
                                    ]

                                    select [
                                        attr.``class`` "w-full h-12 bg-gray-950 border border-purple-500/20 px-4 rounded-xl text-white outline-none"
                                        on.change (fun el _ ->
                                            selectedCount.Set (countToIntOption el?value)
                                        )
                                    ] [
                                        option [
                                            attr.value ""
                                            attr.selected "selected"
                                        ] [
                                            text "Select amount"
                                        ]

                                        for count in diceCounts do
                                            diceCountOption count
                                    ]
                                ]
                            ]

                            div [ attr.``class`` "mb-5 bg-purple-500/10 border border-purple-400/70 rounded-2xl p-5 flex items-center justify-between shadow-[0_0_25px_rgba(168,85,247,0.18)]" ] [
                                div [] [
                                    p [ attr.``class`` "text-xs uppercase tracking-wide text-gray-500" ] [
                                        text "Current setup"
                                    ]

                                    div [ attr.``class`` "text-2xl font-bold text-white mt-1" ] [
                                        Doc.BindView (fun diceOption ->
                                            Doc.BindView (fun countOption ->
                                                match diceOption, countOption with
                                                | Some dice, Some count ->
                                                    text (string count + " x " + diceTypeToText dice)
                                                | _ ->
                                                    text "Select dice and amount"
                                            ) selectedCount.View
                                        ) selectedDice.View
                                    ]
                                ]

                                Doc.BindView (fun diceOption ->
                                    diceIconMedium diceOption
                                ) selectedDice.View
                            ]

                            Doc.BindView (fun diceOption ->
                                Doc.BindView (fun countOption ->
                                    match diceOption, countOption with
                                    | Some _, Some _ ->
                                        button [
                                            attr.``class`` "w-full py-4 bg-gradient-to-r from-purple-600 to-pink-600 rounded-2xl hover:from-purple-700 hover:to-pink-700 font-bold tracking-wide shadow-lg shadow-purple-900/30 flex items-center justify-center gap-3"
                                            on.click (fun _ _ -> rollDice ())
                                        ] [
                                            rollButtonIcon ()
                                            text "ROLL DICE"
                                        ]

                                    | _ ->
                                        button [
                                            attr.``class`` "w-full py-4 bg-gray-700 text-gray-400 rounded-2xl font-bold tracking-wide cursor-not-allowed flex items-center justify-center gap-3"
                                        ] [
                                            rollButtonIcon ()
                                            text "SELECT DICE FIRST"
                                        ]
                                ) selectedCount.View
                            ) selectedDice.View

                            div [ attr.``class`` "mt-6 border border-purple-500/30 rounded-3xl p-6 flex-1 shadow-[0_0_30px_rgba(168,85,247,0.08)]" ] [
                                h2 [ attr.``class`` "text-2xl font-bold mb-5" ] [
                                    text "Last Roll"
                                ]

                                div [ attr.``class`` "grid grid-cols-1 md:grid-cols-[210px_1fr] gap-7 items-center" ] [
                                    div [ attr.``class`` "w-44 h-44 rounded-3xl border-2 border-purple-400/90 flex items-center justify-center shadow-[0_0_35px_rgba(217,70,239,0.38)]" ] [
                                        h2 [ attr.``class`` "text-7xl font-black text-white leading-none drop-shadow-[0_0_16px_rgba(255,255,255,0.35)] -mt-2" ] [
                                            Doc.BindView (fun total ->
                                                if total = 0 then
                                                    text "-"
                                                else
                                                    text (string total)
                                            ) lastTotal.View
                                        ]
                                    ]

                                    div [] [
                                        div [ attr.``class`` "text-2xl font-bold text-purple-300" ] [
                                            Doc.BindView (fun diceOption ->
                                                Doc.BindView (fun countOption ->
                                                    match diceOption, countOption with
                                                    | Some dice, Some count ->
                                                        text (string count + " x " + diceTypeToText dice)
                                                    | _ ->
                                                        text "No roll yet"
                                                ) selectedCount.View
                                            ) selectedDice.View
                                        ]

                                        div [ attr.``class`` "text-sm text-gray-400 mt-2" ] [
                                            Doc.BindView (fun rolls ->
                                                if rolls = "" then
                                                    text "Rolls: -"
                                                else
                                                    text ("Rolls: " + rolls)
                                            ) lastRolls.View
                                        ]
                                    ]
                                ]
                            ]
                        ]
                    ]

                    div [ attr.``class`` "space-y-6" ] [

                        div [ attr.``class`` "h-[330px] bg-gray-900/90 border border-purple-500/20 p-6 rounded-3xl shadow-2xl overflow-hidden" ] [
                            div [ attr.``class`` "flex items-start justify-between mb-4" ] [
                                div [] [
                                    h2 [ attr.``class`` "text-2xl font-bold" ] [
                                        text "Roll Distribution"
                                    ]

                                    p [ attr.``class`` "text-sm text-gray-400 mt-1" ] [
                                        text "Distribution of results for the selected dice."
                                    ]
                                ]
                            ]

                            Doc.BindView (fun diceOption ->
                                Doc.BindView (fun rolls ->

                                    match diceOption with
                                    | None ->
                                        div [ attr.``class`` "h-56 flex flex-col items-center justify-center text-center text-gray-400" ] [
                                            div [ attr.``class`` "text-4xl mb-3 text-purple-300" ] [
                                                text "▥"
                                            ]

                                            div [ attr.``class`` "text-lg text-white font-semibold" ] [
                                                text "No data to display"
                                            ]

                                            div [ attr.``class`` "text-sm text-gray-400 mt-1" ] [
                                                text "Roll some dice to see the distribution."
                                            ]
                                        ]

                                    | Some dice ->
                                        let filteredRolls = rollsForDice dice rolls

                                        if List.isEmpty filteredRolls then
                                            div [ attr.``class`` "h-56 flex flex-col items-center justify-center text-center text-gray-400" ] [
                                                div [ attr.``class`` "text-lg text-white font-semibold" ] [
                                                    text ("No " + diceTypeToText dice + " rolls yet")
                                                ]

                                                div [ attr.``class`` "text-sm text-gray-400 mt-1" ] [
                                                    text "Roll this dice to create chart data."
                                                ]
                                            ]
                                        else
                                            match dice with
                                            | D100 ->
                                                customD100DistributionChart filteredRolls
                                            | _ ->
                                                let sides = diceSides dice
                                                let data = getChartData sides filteredRolls

                                                customDistributionChart data

                                ) allRolls.View
                            ) selectedDice.View
                        ]

                        div [ attr.``class`` "h-[420px] bg-gray-900/90 border border-purple-500/20 p-6 rounded-3xl shadow-2xl overflow-hidden" ] [
                            h2 [ attr.``class`` "text-2xl font-bold mb-1" ] [
                                text "Recent Rolls"
                            ]

                            p [ attr.``class`` "text-sm text-gray-400 mb-5" ] [
                                text "Your latest dice rolls"
                            ]

                            Doc.BindView (fun history ->
                                if List.isEmpty history then
                                    div [ attr.``class`` "h-[300px] flex flex-col items-center justify-center text-center text-gray-400" ] [
                                        div [ attr.``class`` "w-14 h-14 rounded-2xl border border-purple-500/40 flex items-center justify-center text-2xl mb-4" ] [
                                            text "◷"
                                        ]

                                        div [ attr.``class`` "text-lg text-white font-semibold" ] [
                                            text "No recent rolls"
                                        ]

                                        div [ attr.``class`` "text-sm text-gray-400 mt-1" ] [
                                            text "Your roll history will show up here."
                                        ]
                                    ]
                                else
                                    div [ attr.``class`` "space-y-2 h-[315px] overflow-hidden pr-1" ] [
                                        yield!
                                            recentFiveHistory history
                                            |> List.map (fun roll ->
                                                div [ attr.``class`` "flex items-center justify-between bg-gray-800/70 border border-purple-500/10 px-3 py-2 rounded-2xl hover:border-purple-500/30 transition" ] [
                                                    div [ attr.``class`` "flex items-center gap-3 min-w-0" ] [
                                                        img [
                                                            attr.src (diceIconPath roll.Roll.Dice)
                                                            attr.``class`` "w-9 h-9 object-contain"
                                                        ] []

                                                        div [ attr.``class`` "min-w-0" ] [
                                                            div [ attr.``class`` "text-gray-200 text-xs font-bold" ] [
                                                                text (string roll.Roll.Count + " x " + diceTypeToText roll.Roll.Dice)
                                                            ]

                                                            div [ attr.``class`` "text-gray-400 text-[11px] mt-0.5 truncate max-w-[250px]" ] [
                                                                text (roll.Values |> List.map string |> String.concat ", ")
                                                            ]
                                                        ]
                                                    ]

                                                    span [ attr.``class`` "text-xl font-bold text-purple-300 shrink-0 pl-3" ] [
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
        ]
        |> Doc.RunById "main"