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

    type AppMode =
        | SingleMode
        | MultiMode

    type ThemeMode =
        | DarkTheme
        | LightTheme

    type AppPage =
        | HomePage
        | HistoryPage
        | ImportExportPage
        | AboutPage

    type DiceExpression = {
        Count: int
        Dice: DiceType
    }

    type RollEntry = {
        Dice: DiceType
        Value: int
        Mode: AppMode
    }

    type RollHistoryEntry = {
        Roll: DiceExpression
        Values: int list
        Total: int
        PlayerName: string
        PlayerId: int
        Mode: AppMode
    }

    type ChartItem = {
        Label: string
        Count: int
        IsGap: bool
    }

    type Player = {
        Id: int
        Name: string
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

    let selectedMode = Var.Create SingleMode
    let selectedTheme = Var.Create DarkTheme
    let selectedPage = Var.Create HomePage
    let selectedHistoryDice = Var.Create<DiceType option> None
    let showMobileStats = Var.Create false

    let selectedDice = Var.Create<DiceType option> None
    let selectedCount = Var.Create<int option> None

    let lastTotal = Var.Create 0
    let lastRolls = Var.Create ""

    let allRolls = Var.Create<RollEntry list> []
    let rollHistory = Var.Create<RollHistoryEntry list> []

    let players =
        Var.Create<Player list> [
            { Id = 1; Name = "Player 1" }
        ]

    let currentPlayerId = Var.Create 1
    let nextPlayerId = Var.Create 2

    let iconBasePath =
        "./icons/"

    let diceIconPathForMode mode dice =
        match mode with
        | SingleMode -> iconBasePath + diceTypeToText dice + ".svg"
        | MultiMode -> iconBasePath + diceTypeToText dice + "-orange.svg"

    let emptyDiceIconPathForMode mode =
        match mode with
        | SingleMode -> iconBasePath + "dice-empty.svg"
        | MultiMode -> iconBasePath + "dice-empty-orange.svg"

    let selectedDiceIconPath mode diceOption =
        match diceOption with
        | Some dice -> diceIconPathForMode mode dice
        | None -> emptyDiceIconPathForMode mode

    let diceIconSmall mode diceOption =
        img [
            attr.src (selectedDiceIconPath mode diceOption)
            attr.``class`` "w-7 h-7 object-contain"
        ] []

    let diceIconMedium mode diceOption =
        img [
            attr.src (selectedDiceIconPath mode diceOption)
            attr.``class`` "w-14 h-14 object-contain"
        ] []

    let rollButtonIcon mode =
        img [
            attr.src (emptyDiceIconPathForMode mode)
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

    let modeTitle mode =
        match mode with
        | SingleMode -> "Single Player Mode"
        | MultiMode -> "Multiplayer Mode"

    let modeAccentText mode =
        match mode with
        | SingleMode -> "text-purple-300"
        | MultiMode -> "text-orange-300"

    let modeAccentTextLight mode =
        match mode with
        | SingleMode -> "text-purple-700"
        | MultiMode -> "text-orange-700"

    let accentText theme mode =
        match theme with
        | DarkTheme -> modeAccentText mode
        | LightTheme -> modeAccentTextLight mode

    let modeButtonColor mode =
        match mode with
        | SingleMode -> "bg-purple-600"
        | MultiMode -> "bg-orange-600"

    let modeGradient mode =
        match mode with
        | SingleMode -> "bg-gradient-to-r from-purple-600 to-pink-600 hover:from-purple-700 hover:to-pink-700"
        | MultiMode -> "bg-gradient-to-r from-orange-600 to-amber-500 hover:from-orange-700 hover:to-amber-600"

    let modeBorder mode =
        match mode with
        | SingleMode -> "border-purple-500/30"
        | MultiMode -> "border-orange-500/30"

    let modeStrongBorder mode =
        match mode with
        | SingleMode -> "border-purple-400/70"
        | MultiMode -> "border-orange-400/70"

    let modeSoftBg mode =
        match mode with
        | SingleMode -> "bg-purple-500/10"
        | MultiMode -> "bg-orange-500/10"

    let modeGlow mode =
        match mode with
        | SingleMode -> "shadow-[0_0_35px_rgba(217,70,239,0.38)]"
        | MultiMode -> "shadow-[0_0_35px_rgba(249,115,22,0.38)]"

    let modeBarGradient mode =
        match mode with
        | SingleMode -> "from-purple-700 to-pink-500"
        | MultiMode -> "from-orange-700 to-amber-500"

    let appBg theme =
        match theme with
        | DarkTheme -> "bg-gray-950 text-white"
        | LightTheme -> "bg-slate-200 text-slate-900"

    let sidebarBg theme =
        match theme with
        | DarkTheme -> "bg-gray-900 border-white/10"
        | LightTheme -> "bg-slate-100 border-slate-300"

    let panelBg theme =
        match theme with
        | DarkTheme -> "bg-gray-900/90"
        | LightTheme -> "bg-slate-100/95"

    let inputBg theme =
        match theme with
        | DarkTheme -> "bg-gray-950 text-white border-white/10"
        | LightTheme -> "bg-slate-200 text-slate-900 border-slate-300"

    let innerBoxBg theme =
        match theme with
        | DarkTheme -> "bg-gray-800/80 border-white/10"
        | LightTheme -> "bg-slate-200/80 border-slate-300"

    let mutedText theme =
        match theme with
        | DarkTheme -> "text-gray-400"
        | LightTheme -> "text-slate-600"

    let titleText theme =
        match theme with
        | DarkTheme -> "text-white"
        | LightTheme -> "text-slate-900"

    let cardHoverBorder theme =
        match theme with
        | DarkTheme -> "hover:border-white/20"
        | LightTheme -> "hover:border-slate-400"

    let playerDotColor playerId =
        match playerId % 6 with
        | 1 -> "bg-orange-500"
        | 2 -> "bg-blue-500"
        | 3 -> "bg-green-500"
        | 4 -> "bg-red-500"
        | 5 -> "bg-purple-500"
        | _ -> "bg-yellow-500"

    let findCurrentPlayerName playerList currentId =
        playerList
        |> List.tryFind (fun player -> player.Id = currentId)
        |> function
            | Some player -> player.Name
            | None -> "Player 1"

    let currentPlayerName () =
        findCurrentPlayerName players.Value currentPlayerId.Value

    let toggleTheme () =
        match selectedTheme.Value with
        | DarkTheme -> selectedTheme.Set LightTheme
        | LightTheme -> selectedTheme.Set DarkTheme

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
                rolls |> List.map (fun roll -> { Dice = dice; Value = roll; Mode = selectedMode.Value })

            let playerName =
                match selectedMode.Value with
                | SingleMode -> "Single Player"
                | MultiMode -> currentPlayerName ()

            let historyEntry = {
                Roll = expression
                Values = rolls
                Total = total
                PlayerName = playerName
                PlayerId = currentPlayerId.Value
                Mode = selectedMode.Value
            }

            lastTotal.Set total
            lastRolls.Set rollsText
            allRolls.Set (allRolls.Value @ entries)
            rollHistory.Set (rollHistory.Value @ [ historyEntry ])

        | _ -> ()

    let addPlayer () =
        let id = nextPlayerId.Value

        players.Set (
            players.Value @ [
                { Id = id; Name = "Player " + string id }
            ]
        )

        nextPlayerId.Set (id + 1)

    let updatePlayerName playerId newName =
        let safeName =
            if newName = "" then
                "Player " + string playerId
            else
                newName

        let updated =
            players.Value
            |> List.map (fun player ->
                if player.Id = playerId then
                    { player with Name = safeName }
                else
                    player
            )

        players.Set updated

    let nextPlayer () =
        let currentPlayers = players.Value

        match currentPlayers with
        | [] -> ()
        | _ ->
            let currentIndex =
                currentPlayers
                |> List.tryFindIndex (fun player -> player.Id = currentPlayerId.Value)

            match currentIndex with
            | Some index ->
                let nextIndex =
                    if index + 1 >= List.length currentPlayers then
                        0
                    else
                        index + 1

                currentPlayerId.Set currentPlayers.[nextIndex].Id

            | None ->
                currentPlayerId.Set currentPlayers.Head.Id

    let removePlayer playerId =
        let remaining =
            players.Value
            |> List.filter (fun player -> player.Id <> playerId)

        let safePlayers =
            if List.isEmpty remaining then
                [ { Id = 1; Name = "Player 1" } ]
            else
                remaining

        players.Set safePlayers

        if currentPlayerId.Value = playerId then
            currentPlayerId.Set safePlayers.Head.Id

    let storageKey =
        "redice-save-v1"

    let appModeToText mode =
        match mode with
        | SingleMode -> "single"
        | MultiMode -> "multi"

    let textToAppMode text =
        match text with
        | "single" -> SingleMode
        | "multi" -> MultiMode
        | _ -> SingleMode

    let cleanSaveText (value: string) =
        value
            .Replace("|", " ")
            .Replace(";", " ")
            .Replace("~", " ")
            .Replace(",", " ")

    let serializePlayer player =
        string player.Id + "," + cleanSaveText player.Name

    let deserializePlayer (textValue: string) =
        let parts = textValue.Split([|','|])

        if parts.Length = 2 then
            Some {
                Id = int parts.[0]
                Name = parts.[1]
            }
        else
            None

    let serializeHistoryEntry entry =
        let valuesText =
            entry.Values
            |> List.map string
            |> String.concat ","

        String.concat "|" [
            appModeToText entry.Mode
            string entry.PlayerId
            cleanSaveText entry.PlayerName
            string entry.Roll.Count
            diceTypeToText entry.Roll.Dice
            valuesText
            string entry.Total
        ]

    let deserializeHistoryEntry (textValue: string) =
        let parts = textValue.Split([|'|'|])

        if parts.Length = 7 then
            match diceTextToTypeOption parts.[4] with
            | Some dice ->
                let values =
                    if parts.[5] = "" then
                        []
                    else
                        parts.[5].Split([|','|])
                        |> Array.toList
                        |> List.map int

                Some {
                    Mode = textToAppMode parts.[0]
                    PlayerId = int parts.[1]
                    PlayerName = parts.[2]
                    Roll = {
                        Count = int parts.[3]
                        Dice = dice
                    }
                    Values = values
                    Total = int parts.[6]
                }

            | None ->
                None
        else
            None

    let buildSaveText () =
        let playersText =
            players.Value
            |> List.map serializePlayer
            |> String.concat ";"

        let historyText =
            rollHistory.Value
            |> List.map serializeHistoryEntry
            |> String.concat "~"

        String.concat "\n" [
            "current|" + string currentPlayerId.Value
            "next|" + string nextPlayerId.Value
            "players|" + playersText
            "history|" + historyText
        ]

    let saveGame () =
        JS.Window.LocalStorage.SetItem(storageKey, buildSaveText ())

    let loadSaveText (savedText: string) =
        let lines =
            savedText.Split([|'\n'|])
            |> Array.toList

        let getLine (prefix: string) =
            lines
            |> List.tryFind (fun (line: string) -> line.StartsWith(prefix))
            |> function
                | Some (line: string) -> line.Substring(prefix.Length)
                | None -> ""

        let loadedCurrentId =
            let value = getLine "current|"
            if value = "" then 1 else int value

        let loadedNextId =
            let value = getLine "next|"
            if value = "" then 2 else int value

        let loadedPlayers =
            let value = getLine "players|"

            if value = "" then
                [ { Id = 1; Name = "Player 1" } ]
            else
                value.Split([|';'|])
                |> Array.toList
                |> List.choose deserializePlayer

        let loadedHistory =
            let value = getLine "history|"

            if value = "" then
                []
            else
                value.Split([|'~'|])
                |> Array.toList
                |> List.choose deserializeHistoryEntry

        let loadedRolls =
            loadedHistory
            |> List.collect (fun history ->
                history.Values
                |> List.map (fun value ->
                    {
                        Dice = history.Roll.Dice
                        Value = value
                        Mode = history.Mode
                    }
                )
            )

        players.Set loadedPlayers
        currentPlayerId.Set loadedCurrentId
        nextPlayerId.Set loadedNextId

        rollHistory.Set loadedHistory
        allRolls.Set loadedRolls

        selectedDice.Set None
        selectedCount.Set None
        selectedHistoryDice.Set None

        match loadedHistory |> List.rev with
        | last :: _ ->
            lastTotal.Set last.Total
            lastRolls.Set (last.Values |> List.map string |> String.concat ", ")
        | [] ->
            lastTotal.Set 0
            lastRolls.Set ""

        showMobileStats.Set false

    let loadGame () =
        let savedText =
            JS.Window.LocalStorage.GetItem(storageKey)

        if isNull savedText then
            ()
        else
            loadSaveText savedText

    let exportSaveFile () =
        let saveText =
            buildSaveText ()

        let encoded =
            JS.EncodeURIComponent saveText

        let link =
            JS.Document.CreateElement("a")

        link.SetAttribute("href", "data:text/plain;charset=utf-8," + encoded)
        link.SetAttribute("download", "redice-save.redice")
        link?click()

    let importSaveFile (el: Dom.Element) =
        let files =
            el?files

        if isNull files then
            ()
        else
            let file =
                files?item(0)

            if isNull file then
                ()
            else
                let reader =
                    JS.Eval("new FileReader()")

                reader?onload <- fun _ ->
                    let importedText =
                        string reader?result

                    loadSaveText importedText
                    JS.Window.LocalStorage.SetItem(storageKey, importedText)

                reader?readAsText(file)

    let newGame () =
        selectedMode.Set SingleMode
        selectedPage.Set HomePage
        selectedHistoryDice.Set None

        selectedDice.Set None
        selectedCount.Set None

        lastTotal.Set 0
        lastRolls.Set ""

        allRolls.Set []
        rollHistory.Set []

        players.Set [
            { Id = 1; Name = "Player 1" }
        ]

        currentPlayerId.Set 1
        nextPlayerId.Set 2
        JS.Window.LocalStorage.RemoveItem(storageKey)

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

    let rollsForDice mode dice (rolls: RollEntry list) =
        rolls
        |> List.filter (fun roll -> roll.Mode = mode && roll.Dice = dice)
        |> List.map (fun roll -> roll.Value)

    let diceTypeOption dice =
        option [ attr.value (diceTypeToText dice) ] [
            text (diceTypeToText dice)
        ]

    let diceCountOption count =
        option [ attr.value (string count) ] [
            text (string count)
        ]

    let modeButton buttonMode currentMode =
        let isActive = buttonMode = currentMode

        button [
            attr.``class`` (
                if isActive then
                    "px-6 py-3 rounded-xl " + modeButtonColor buttonMode + " text-white font-semibold shadow-lg"
                else
                    "px-6 py-3 rounded-xl text-gray-400 font-semibold hover:text-white"
            )
            on.click (fun _ _ ->
                selectedMode.Set buttonMode

                selectedDice.Set None
                selectedCount.Set None
                selectedHistoryDice.Set None

                lastTotal.Set 0
                lastRolls.Set ""
            )
           
        ] [
            text (
                match buttonMode with
                | SingleMode -> "Single Player"
                | MultiMode -> "Multi Player"
            )
        ]

    let pageToText page =
        match page with
        | HomePage -> "Home"
        | HistoryPage -> "History"
        | ImportExportPage -> "Import / Export"
        | AboutPage -> "About"

    let sideMenuItem theme mode page currentPage =
        let isActive = page = currentPage

        let activeClass =
            if isActive then
                modeButtonColor mode + " text-white font-semibold"
            else
                match theme with
                | DarkTheme -> "text-gray-400 hover:text-white"
                | LightTheme -> "text-slate-600 hover:text-slate-900"

        button [
            attr.``class`` ("w-full text-left px-4 py-3 rounded-xl text-sm " + activeClass)
            on.click (fun _ _ -> selectedPage.Set page)
        ] [
            text (pageToText page)
        ]

    let newGameButton theme mode =
        button [
            attr.``class`` (
                "w-full mt-6 px-4 py-3 rounded-2xl border text-sm font-semibold transition text-left " +

                match mode with
                | SingleMode ->
                    match theme with
                    | DarkTheme ->
                        "border-purple-500/30 text-purple-300 hover:bg-purple-500/10"
                    | LightTheme ->
                        "border-purple-500/60 text-purple-700 hover:bg-purple-500/10"

                | MultiMode ->
                    match theme with
                    | DarkTheme ->
                        "border-orange-500/30 text-orange-300 hover:bg-orange-500/10"
                    | LightTheme ->
                        "border-orange-500/60 text-orange-700 hover:bg-orange-500/10"
            )
            on.click (fun _ _ -> newGame ())
        ] [
            text "↻ New Game"
        ]

    let sidebar theme mode currentPage =
        aside [ attr.``class`` ("hidden md:flex w-64 border-r p-6 flex-col min-h-screen " + sidebarBg theme) ] [
            div [ attr.``class`` "flex items-center gap-3 mb-10" ] [
                img [
                    attr.src (diceIconPathForMode mode D20)
                    attr.``class`` "w-9 h-9 object-contain"
                ] []

                h1 [ attr.``class`` ("text-3xl font-bold " + titleText theme) ] [
                    text "ReDice"
                ]
            ]

            nav [ attr.``class`` "flex flex-col h-full" ] [

                div [ attr.``class`` "space-y-3" ] [
                    newGameButton theme mode

                    div [ attr.``class`` ("flex border rounded-2xl p-1 shadow-lg " + panelBg theme + " " + modeBorder mode) ] [
                        button [
                            attr.``class`` (
                                "flex-1 px-4 py-3 rounded-xl text-white font-semibold transition " +
                                modeButtonColor mode
                            )
                            on.click (fun _ _ -> saveGame ())
                        ] [
                            text "Save"
                        ]

                        button [
                            attr.``class`` (
                                "flex-1 px-4 py-3 rounded-xl font-semibold transition " +
                                accentText theme mode
                            )
                            on.click (fun _ _ -> loadGame ())
                        ] [
                            text "Load"
                        ]
                    ]

                    button [
                        attr.``class`` (
                            "w-full px-4 py-3 rounded-2xl border text-sm font-semibold text-left transition " +
                            modeBorder mode + " " + accentText theme mode + " hover:bg-white/5"
                        )
                        on.click (fun _ _ -> selectedPage.Set ImportExportPage)
                    ] [
                        text "Import / Export"
                    ]
                ]

                div [ attr.``class`` "mt-auto space-y-3 pt-10" ] [
                    sideMenuItem theme mode HomePage currentPage
                    sideMenuItem theme mode HistoryPage currentPage
                    sideMenuItem theme mode AboutPage currentPage
                ]
            ]
        ]

    let mobileNavbar theme mode currentPage =
        div [ attr.``class`` ("md:hidden border-b p-4 " + sidebarBg theme) ] [
            div [ attr.``class`` "flex items-center justify-between" ] [
                div [ attr.``class`` "flex items-center gap-3" ] [
                    img [
                        attr.src (diceIconPathForMode mode D20)
                        attr.``class`` "w-8 h-8 object-contain"
                    ] []

                    div [ attr.``class`` ("text-2xl font-bold " + titleText theme) ] [
                        text "ReDice"
                    ]
                ]

                details [ attr.``class`` "relative" ] [
                    summary [ attr.``class`` "cursor-pointer rounded-xl bg-gray-800 px-4 py-2 text-sm text-white" ] [
                        text "☰"
                    ]

                    div [ attr.``class`` ("absolute right-0 mt-3 w-56 rounded-2xl border p-3 shadow-xl z-50 " + sidebarBg theme) ] [

                        div [ attr.``class`` "space-y-3" ] [

                            newGameButton theme mode

                            div [ attr.``class`` ("flex border rounded-2xl p-1 shadow-lg " + panelBg theme + " " + modeBorder mode) ] [

                                button [
                                    attr.``class`` (
                                        "flex-1 px-3 py-2 rounded-xl text-white font-semibold transition text-sm " +
                                        modeButtonColor mode
                                    )
                                    on.click (fun _ _ -> saveGame ())
                                ] [
                                    text "Save"
                                ]

                                button [
                                    attr.``class`` (
                                        "flex-1 px-3 py-2 rounded-xl font-semibold transition text-sm " +
                                        accentText theme mode
                                    )
                                    on.click (fun _ _ -> loadGame ())
                                ] [
                                    text "Load"
                                ]
                            ]

                            button [
                                attr.``class`` (
                                    "w-full px-4 py-3 rounded-2xl border text-sm font-semibold text-left transition " +
                                    modeBorder mode + " " + accentText theme mode + " hover:bg-white/5"
                                )
                                on.click (fun _ _ -> selectedPage.Set ImportExportPage)
                            ] [
                                text "Import / Export"
                            ]

                            div [ attr.``class`` "border-t border-white/10 pt-3 mt-3 space-y-3" ] [
                                sideMenuItem theme mode HomePage currentPage
                                sideMenuItem theme mode HistoryPage currentPage
                                sideMenuItem theme mode AboutPage currentPage
                            ]
                        ]
                    ]
                ]
            ]
        ]

    let chartYAxisLabel topValue label =
        div [
            attr.``class`` "absolute right-1 -translate-y-1/2 text-[10px] text-gray-500 leading-none"
            attr.style ("top: " + topValue + ";")
        ] [
            text (string label)
        ]

    let chartGridLine topValue =
        div [
            attr.``class`` "absolute left-0 right-0 border-t border-white/10"
            attr.style ("top: " + topValue + ";")
        ] []

    let chartBar mode chartMax item =
        if item.IsGap then
            div [ attr.``class`` "w-[18px] shrink-0 flex items-end justify-center" ] [
                div [ attr.``class`` "w-px h-[170px] bg-white/20" ] []
            ]
        else
            let height =
                if item.Count = 0 then
                    6
                else
                    int ((float item.Count / float chartMax) * 170.0)

            div [ attr.``class`` "flex-1 max-w-[34px] flex items-end" ] [
                div [
                    attr.``class`` ("w-full rounded-t-xl bg-gradient-to-t " + modeBarGradient mode + " border border-white/30 shadow-[0_0_14px_rgba(168,85,247,0.35)]")
                    attr.style ("height: " + string height + "px; opacity: " + (if item.Count = 0 then "0.22" else "1") + ";")
                ] []
            ]

    let chartXLabel item =
        if item.IsGap then
            div [ attr.``class`` "w-[18px] shrink-0" ] []
        else
            div [ attr.``class`` "flex-1 max-w-[34px] text-center text-[10px] text-gray-300 leading-none" ] [
                text item.Label
            ]

    let neonBarChart mode items =
        let realItems =
            items |> List.filter (fun item -> not item.IsGap)

        let maxCount =
            if List.isEmpty realItems then
                5
            else
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

        let yLabelDocs =
            yLabels |> List.map (fun (topValue, label) -> chartYAxisLabel topValue label)

        let gridDocs =
            yLabels |> List.map (fun (topValue, _) -> chartGridLine topValue)

        let barDocs =
            items |> List.map (fun item -> chartBar mode chartMax item)

        let xLabelDocs =
            items |> List.map chartXLabel

        div [ attr.``class`` "h-[245px] w-full grid grid-cols-[42px_1fr] gap-2 pt-4 pb-2 pr-6" ] [
            div [ attr.``class`` "relative h-[190px]" ] yLabelDocs

            div [ attr.``class`` "flex flex-col" ] [
                div [ attr.``class`` "relative h-[190px]" ] [
                    div [ attr.``class`` "absolute inset-0" ] gridDocs
                    div [ attr.``class`` "absolute inset-0 flex items-end justify-start gap-1.5 px-1" ] barDocs
                ]

                div [ attr.``class`` "h-6 mt-2 flex items-start justify-start gap-1.5 px-1" ] xLabelDocs
            ]
        ]

    let customDistributionChart mode data =
        neonBarChart mode data

    let customD100DistributionChart mode rolls =
        rolls
        |> getD100ChartData
        |> neonBarChart mode

    let currentPlayerNameDoc () =
        Doc.BindView (fun currentId ->
            let name = findCurrentPlayerName players.Value currentId
            text name
        ) currentPlayerId.View

    let currentPlayerDotDoc () =
        Doc.BindView (fun currentId ->
            div [ attr.``class`` ("w-3 h-3 rounded-full " + playerDotColor currentId) ] []
        ) currentPlayerId.View

    let rollSetupDoc () =
        Doc.BindView (fun diceOption ->
            Doc.BindView (fun countOption ->
                match diceOption, countOption with
                | Some dice, Some count ->
                    text (string count + " x " + diceTypeToText dice)
                | _ ->
                    text "Select dice"
            ) selectedCount.View
        ) selectedDice.View

    let lastRollTitleDoc mode =
        match mode with
        | SingleMode ->
            Doc.BindView (fun diceOption ->
                Doc.BindView (fun countOption ->
                    match diceOption, countOption with
                    | Some dice, Some count ->
                        text (string count + " x " + diceTypeToText dice)
                    | _ ->
                        text "No roll yet"
                ) selectedCount.View
            ) selectedDice.View

        | MultiMode ->
            div [ attr.``class`` "flex items-center gap-2" ] [
                currentPlayerNameDoc ()
                currentPlayerDotDoc ()
            ]

    let recentRollTitle mode roll =
        match mode with
        | SingleMode ->
            string roll.Roll.Count + " x " + diceTypeToText roll.Roll.Dice
        | MultiMode ->
            roll.PlayerName

    let recentRollDetail mode roll =
        match mode with
        | SingleMode ->
            roll.Values |> List.map string |> String.concat ", "
        | MultiMode ->
            string roll.Roll.Count + " x " + diceTypeToText roll.Roll.Dice + " · " + (roll.Values |> List.map string |> String.concat ", ")

    let recentRollDoc theme mode roll =
        let titleText = recentRollTitle mode roll
        let detailText = recentRollDetail mode roll

        let titleChildren =
            match mode with
            | SingleMode ->
                [ text titleText ]
            | MultiMode ->
                [
                    div [ attr.``class`` ("w-2.5 h-2.5 rounded-full " + playerDotColor roll.PlayerId) ] []
                    text titleText
                ]

        div [ attr.``class`` ("flex items-center justify-between border px-3 py-2 rounded-2xl transition " + innerBoxBg theme + " " + cardHoverBorder theme) ] [
            div [ attr.``class`` "flex items-center gap-3 min-w-0" ] [
                img [
                    attr.src (diceIconPathForMode mode roll.Roll.Dice)
                    attr.``class`` "w-9 h-9 object-contain"
                ] []

                div [ attr.``class`` "min-w-0" ] [
                    div [ attr.``class`` ("flex items-center gap-2 text-xs font-bold " + accentText theme mode) ] titleChildren

                    div [ attr.``class`` ("text-[11px] mt-0.5 truncate max-w-[250px] " + mutedText theme) ] [
                        text detailText
                    ]
                ]
            ]

            span [ attr.``class`` ("text-xl font-bold shrink-0 pl-3 " + accentText theme mode) ] [
                text (string roll.Total)
            ]
        ]

    let historyRollDoc theme mode roll =
        let titleText =
            match mode with
            | SingleMode ->
                string roll.Roll.Count + " x " + diceTypeToText roll.Roll.Dice
            | MultiMode ->
                roll.PlayerName

        let detailText =
            match mode with
            | SingleMode ->
                roll.Values |> List.map string |> String.concat ", "
            | MultiMode ->
                string roll.Roll.Count + " x " + diceTypeToText roll.Roll.Dice + " · " + (roll.Values |> List.map string |> String.concat ", ")

        div [ attr.``class`` ("flex items-center justify-between border px-3 py-3 rounded-2xl transition " + innerBoxBg theme + " " + cardHoverBorder theme) ] [
            div [ attr.``class`` "flex items-center gap-3 min-w-0" ] [
                img [
                    attr.src (diceIconPathForMode mode roll.Roll.Dice)
                    attr.``class`` "w-10 h-10 object-contain"
                ] []

                div [ attr.``class`` "min-w-0" ] [
                    div [ attr.``class`` ("flex items-center gap-2 text-sm font-bold " + accentText theme mode) ] [
                        if mode = MultiMode then
                            div [ attr.``class`` ("w-2.5 h-2.5 rounded-full " + playerDotColor roll.PlayerId) ] []

                        text titleText
                    ]

                    div [ attr.``class`` ("text-xs mt-1 truncate max-w-[340px] " + mutedText theme) ] [
                        text detailText
                    ]
                ]
            ]

            span [ attr.``class`` ("text-2xl font-bold shrink-0 pl-3 " + accentText theme mode) ] [
                text (string roll.Total)
            ]
        ]

    let historyListPanel theme mode =
        div [ attr.``class`` ("h-[774px] border " + panelBg theme + " " + modeBorder mode + " p-6 rounded-3xl shadow-2xl overflow-hidden") ] [
            h2 [ attr.``class`` ("text-2xl font-bold mb-1 " + titleText theme) ] [
                text "Roll History"
            ]

            p [ attr.``class`` ("text-sm mb-5 " + mutedText theme) ] [
                text "All saved rolls for the selected mode."
            ]

            Doc.BindView (fun history ->
                let filtered =
                    history
                    |> List.filter (fun roll -> roll.Mode = mode)
                    |> List.rev

                if List.isEmpty filtered then
                    div [ attr.``class`` ("h-[650px] flex flex-col items-center justify-center text-center " + mutedText theme) ] [
                        div [ attr.``class`` ("text-lg font-semibold " + titleText theme) ] [
                            text "No history yet"
                        ]

                        div [ attr.``class`` ("text-sm mt-1 " + mutedText theme) ] [
                            text "Roll some dice and your full history will appear here."
                        ]
                    ]
                else
                    let items =
                        filtered
                        |> List.map (fun roll -> historyRollDoc theme mode roll)

                    div [
                        attr.``class`` (
                            "space-y-2 h-[650px] overflow-y-auto pr-2 " +
                            (match mode with
                            | SingleMode -> "redice-scroll-purple"
                            | MultiMode -> "redice-scroll-orange")
                        )
                    ] items
            ) rollHistory.View
        ]

    let historyDistributionPanel theme mode =
        div [ attr.``class`` ("h-[330px] border " + panelBg theme + " " + modeBorder mode + " p-6 rounded-3xl shadow-2xl overflow-hidden") ] [
            div [ attr.``class`` "flex items-start justify-between gap-4 mb-4" ] [
                div [] [
                    h2 [ attr.``class`` ("text-2xl font-bold " + titleText theme) ] [
                        text "History Distribution"
                    ]

                    p [ attr.``class`` ("text-sm mt-1 " + mutedText theme) ] [
                        text "Choose a dice type to inspect its full history."
                    ]
                ]

                select [
                    attr.``class`` ("w-32 h-11 px-3 rounded-xl outline-none " + inputBg theme)
                    on.change (fun el _ ->
                        selectedHistoryDice.Set (diceTextToTypeOption el?value)
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

            Doc.BindView (fun diceOption ->
                Doc.BindView (fun rolls ->
                    match diceOption with
                    | None ->
                        div [ attr.``class`` ("h-52 flex flex-col items-center justify-center text-center " + mutedText theme) ] [
                            div [ attr.``class`` ("text-lg font-semibold " + titleText theme) ] [
                                text "Select a dice type"
                            ]

                            div [ attr.``class`` ("text-sm mt-1 " + mutedText theme) ] [
                                text "The chart will use only rolls from this mode."
                            ]
                        ]

                    | Some dice ->
                        let filteredRolls =
                            rollsForDice mode dice rolls

                        if List.isEmpty filteredRolls then
                            div [ attr.``class`` ("h-52 flex flex-col items-center justify-center text-center " + mutedText theme) ] [
                                div [ attr.``class`` ("text-lg font-semibold " + titleText theme) ] [
                                    text ("No " + diceTypeToText dice + " history yet")
                                ]

                                div [ attr.``class`` ("text-sm mt-1 " + mutedText theme) ] [
                                    text "Roll this dice first to create history data."
                                ]
                            ]
                        else
                            match dice with
                            | D100 ->
                                customD100DistributionChart mode filteredRolls
                            | _ ->
                                let sides = diceSides dice
                                let data = getChartData sides filteredRolls
                                customDistributionChart mode data
                ) allRolls.View
            ) selectedHistoryDice.View
        ]

    let mostUsedDiceText entries =
        let diceCounts =
            diceTypes
            |> List.map (fun dice ->
                let count =
                    entries
                    |> List.filter (fun entry -> entry.Roll.Dice = dice)
                    |> List.length

                dice, count
            )

        let best =
            diceCounts
            |> List.sortByDescending snd
            |> List.tryHead

        match best with
        | Some (dice, count) when count > 0 ->
            diceTypeToText dice
        | _ ->
            "-"

    let historySummaryPanel theme mode =
        div [ attr.``class`` ("h-[420px] border " + panelBg theme + " " + modeBorder mode + " p-6 rounded-3xl shadow-2xl overflow-hidden") ] [
            h2 [ attr.``class`` ("text-2xl font-bold mb-1 " + titleText theme) ] [
                text "History Summary"
            ]

            p [ attr.``class`` ("text-sm mb-5 " + mutedText theme) ] [
                text "Overview for the selected mode."
            ]

            Doc.BindView (fun history ->
                let filtered =
                    history
                    |> List.filter (fun roll -> roll.Mode = mode)

                let totalRollActions =
                    filtered |> List.length

                let totalDiceThrown =
                    filtered
                    |> List.sumBy (fun roll -> List.length roll.Values)

                let highestTotal =
                    filtered
                    |> List.map (fun roll -> roll.Total)
                    |> function
                        | [] -> 0
                        | values -> List.max values

                let mostUsed =
                    mostUsedDiceText filtered

                div [ attr.``class`` "grid grid-cols-1 sm:grid-cols-2 gap-4" ] [
                    div [ attr.``class`` ("border rounded-2xl p-4 " + innerBoxBg theme) ] [
                        p [ attr.``class`` ("text-xs uppercase tracking-wide " + mutedText theme) ] [
                            text "Roll actions"
                        ]

                        div [ attr.``class`` ("text-3xl font-black mt-2 " + accentText theme mode) ] [
                            text (string totalRollActions)
                        ]
                    ]

                    div [ attr.``class`` ("border rounded-2xl p-4 " + innerBoxBg theme) ] [
                        p [ attr.``class`` ("text-xs uppercase tracking-wide " + mutedText theme) ] [
                            text "Dice thrown"
                        ]

                        div [ attr.``class`` ("text-3xl font-black mt-2 " + accentText theme mode) ] [
                            text (string totalDiceThrown)
                        ]
                    ]

                    div [ attr.``class`` ("border rounded-2xl p-4 " + innerBoxBg theme) ] [
                        p [ attr.``class`` ("text-xs uppercase tracking-wide " + mutedText theme) ] [
                            text "Most used dice"
                        ]

                        div [ attr.``class`` ("text-3xl font-black mt-2 " + accentText theme mode) ] [
                            text mostUsed
                        ]
                    ]

                    div [ attr.``class`` ("border rounded-2xl p-4 " + innerBoxBg theme) ] [
                        p [ attr.``class`` ("text-xs uppercase tracking-wide " + mutedText theme) ] [
                            text "Highest total"
                        ]

                        div [ attr.``class`` ("text-3xl font-black mt-2 " + accentText theme mode) ] [
                            if highestTotal = 0 then
                                text "-"
                            else
                                text (string highestTotal)
                        ]
                    ]
                ]
            ) rollHistory.View
        ]

    let historyPageLayout theme mode =
        div [ attr.``class`` "grid grid-cols-1 xl:grid-cols-2 gap-6 items-start" ] [
            div [ attr.``class`` "space-y-6" ] [
                historyListPanel theme mode
            ]

            div [ attr.``class`` "space-y-6" ] [
                historyDistributionPanel theme mode
                historySummaryPanel theme mode
            ]
        ]

    let aboutPageLayout theme mode =
        div [ attr.``class`` "grid grid-cols-1 xl:grid-cols-2 gap-6 items-start" ] [

            div [ attr.``class`` ("h-[774px] border " + panelBg theme + " " + modeBorder mode + " p-6 rounded-3xl shadow-2xl overflow-hidden") ] [
                h2 [ attr.``class`` ("text-3xl font-bold mb-2 " + accentText theme mode) ] [
                    text "About ReDice"
                ]

                p [ attr.``class`` ("text-sm mb-6 " + mutedText theme) ] [
                    text "ReDice is a responsive RPG dice roller web application built as a WebSharper single page application."
                ]

                div [ attr.``class`` "space-y-4" ] [
                    div [ attr.``class`` ("border rounded-2xl p-5 " + innerBoxBg theme) ] [
                        h3 [ attr.``class`` ("text-xl font-bold mb-2 " + titleText theme) ] [
                            text "Project goal"
                        ]

                        p [ attr.``class`` ("text-sm leading-relaxed " + mutedText theme) ] [
                            text "The goal of the project is to support tabletop RPG sessions with quick dice rolling, local multiplayer support, roll history, statistics, and portable save files."
                        ]
                    ]

                    div [ attr.``class`` ("border rounded-2xl p-5 " + innerBoxBg theme) ] [
                        h3 [ attr.``class`` ("text-xl font-bold mb-2 " + titleText theme) ] [
                            text "Main features"
                        ]

                        ul [ attr.``class`` ("text-sm leading-7 list-disc pl-5 " + mutedText theme) ] [
                            li [] [ text "Single Player and Multiplayer modes" ]
                            li [] [ text "Editable local player list" ]
                            li [] [ text "Dice types from D4 to D100" ]
                            li [] [ text "Roll distribution charts" ]
                            li [] [ text "Full roll history with summary statistics" ]
                            li [] [ text "Save / Load and .redice Import / Export" ]
                            li [] [ text "Responsive layout with dark and light themes" ]
                        ]
                    ]
                ]
            ]

            div [ attr.``class`` "space-y-6" ] [
                div [ attr.``class`` ("h-[330px] border " + panelBg theme + " " + modeBorder mode + " p-6 rounded-3xl shadow-2xl overflow-hidden") ] [
                    h2 [ attr.``class`` ("text-2xl font-bold mb-1 " + titleText theme) ] [
                        text "Technology"
                    ]

                    p [ attr.``class`` ("text-sm mb-5 " + mutedText theme) ] [
                        text "The application uses technologies from the DUE Functional F / WebSharper SPA course material."
                    ]

                    div [ attr.``class`` "grid grid-cols-1 sm:grid-cols-2 gap-4" ] [
                        div [ attr.``class`` ("border rounded-2xl p-4 " + innerBoxBg theme) ] [
                            p [ attr.``class`` ("text-xs uppercase tracking-wide " + mutedText theme) ] [
                                text "Language"
                            ]

                            div [ attr.``class`` ("text-2xl font-black mt-2 " + accentText theme mode) ] [
                                text "F#"
                            ]
                        ]

                        div [ attr.``class`` ("border rounded-2xl p-4 " + innerBoxBg theme) ] [
                            p [ attr.``class`` ("text-xs uppercase tracking-wide " + mutedText theme) ] [
                                text "Framework"
                            ]

                            div [ attr.``class`` ("text-2xl font-black mt-2 " + accentText theme mode) ] [
                                text "WebSharper"
                            ]
                        ]

                        div [ attr.``class`` ("border rounded-2xl p-4 " + innerBoxBg theme) ] [
                            p [ attr.``class`` ("text-xs uppercase tracking-wide " + mutedText theme) ] [
                                text "UI"
                            ]

                            div [ attr.``class`` ("text-2xl font-black mt-2 " + accentText theme mode) ] [
                                text "Tailwind"
                            ]
                        ]

                        div [ attr.``class`` ("border rounded-2xl p-4 " + innerBoxBg theme) ] [
                            p [ attr.``class`` ("text-xs uppercase tracking-wide " + mutedText theme) ] [
                                text "Storage"
                            ]

                            div [ attr.``class`` ("text-2xl font-black mt-2 " + accentText theme mode) ] [
                                text ".redice"
                            ]
                        ]
                    ]
                ]

                div [ attr.``class`` ("h-[420px] border " + panelBg theme + " " + modeBorder mode + " p-6 rounded-3xl shadow-2xl overflow-hidden") ] [
                    h2 [ attr.``class`` ("text-2xl font-bold mb-1 " + titleText theme) ] [
                        text "Course connection"
                    ]

                    p [ attr.``class`` ("text-sm leading-relaxed mt-4 " + mutedText theme) ] [
                        text "The application demonstrates a client-side SPA structure with state management, dynamic views, responsive UI design, user interaction, charts, and local browser-based save handling."
                    ]

                    div [ attr.``class`` ("mt-6 rounded-2xl border p-5 " + modeSoftBg mode + " " + modeStrongBorder mode) ] [
                        p [ attr.``class`` ("text-sm font-semibold " + accentText theme mode) ] [
                            text "ReDice is designed as a practical RPG dice roller, not just a demo page."
                        ]
                    ]
                ]
            ]
        ]

    let importExportPageLayout theme mode =
        div [ attr.``class`` "grid grid-cols-1 xl:grid-cols-2 gap-6 items-start" ] [

            div [ attr.``class`` ("h-[420px] border " + panelBg theme + " " + modeBorder mode + " p-6 rounded-3xl shadow-2xl overflow-hidden") ] [
                h2 [ attr.``class`` ("text-2xl font-bold mb-1 " + titleText theme) ] [
                    text "Export Save"
                ]

                p [ attr.``class`` ("text-sm mb-6 " + mutedText theme) ] [
                    text "Download your current ReDice save file and move it to another computer."
                ]

                button [
                    attr.``class`` ("w-full py-4 rounded-2xl text-white font-bold " + modeButtonColor mode)
                    on.click (fun _ _ -> exportSaveFile ())
                ] [
                    text "EXPORT SAVE FILE"
                ]
            ]

            div [ attr.``class`` ("h-[420px] border " + panelBg theme + " " + modeBorder mode + " p-6 rounded-3xl shadow-2xl overflow-hidden") ] [
                h2 [ attr.``class`` ("text-2xl font-bold mb-1 " + titleText theme) ] [
                    text "Import Save"
                ]

                p [ attr.``class`` ("text-sm mb-6 " + mutedText theme) ] [
                    text "Choose a previously exported ReDice save file."
                ]

                input [
                    attr.``type`` "file"
                    attr.accept ".redice"
                    attr.``class`` ("w-full rounded-2xl border p-4 " + inputBg theme)
                    on.change (fun el _ -> importSaveFile el)
                ] []
            ]
        ]

    let playerRow theme mode currentId player =
        let activeClass =
            if player.Id = currentId then
                match mode with
                | SingleMode -> "bg-purple-500/15 border-purple-400/40"
                | MultiMode -> "bg-orange-500/15 border-orange-400/40"
            else
                match theme with
                | DarkTheme -> "bg-gray-800/70 border-white/10"
                | LightTheme -> "bg-slate-200/80 border-slate-300"

        div [ attr.``class`` ("flex items-center gap-2 border rounded-xl p-2 " + activeClass) ] [

            div [ attr.``class`` ("w-3 h-3 rounded-full shrink-0 " + playerDotColor player.Id) ] []

            input [
                attr.value player.Name
                attr.``class`` ("flex-1 min-w-0 bg-transparent text-sm outline-none " + titleText theme)
                on.change (fun el _ -> updatePlayerName player.Id el?value)
            ] []

            if player.Id = 1 then
                div [
                    attr.``class`` "w-8 h-8 shrink-0 rounded-lg border border-transparent"
                ] []
            else
                button [
                    attr.``class`` ("w-8 h-8 shrink-0 rounded-lg flex items-center justify-center text-white font-bold " + modeButtonColor mode)
                    on.click (fun _ _ -> removePlayer player.Id)
                ] [
                    span [ attr.style "transform: translateY(-1px);" ] [
                        text "−"
                    ]
                ]
        ]

    let currentPlayerPanel theme mode =
        div [ attr.``class`` "mt-4 mb-3 flex items-center justify-between" ] [
            div [] [
                p [ attr.``class`` ("text-xs uppercase tracking-wide " + mutedText theme) ] [
                    text "Current player"
                ]

                div [ attr.``class`` ("flex items-center gap-2 text-2xl font-bold mt-1 " + accentText theme mode) ] [
                    currentPlayerNameDoc ()
                    currentPlayerDotDoc ()
                ]
            ]

            button [
                attr.``class`` ("px-5 py-3 rounded-xl border " + modeStrongBorder mode + " " + accentText theme mode + " font-bold hover:bg-white/5")
                on.click (fun _ _ -> nextPlayer ())
            ] [
                text "NEXT PLAYER →"
            ]
        ]

    let playersPanel theme mode =
        div [ attr.``class`` ("h-[420px] border " + panelBg theme + " " + modeBorder mode + " p-5 rounded-3xl shadow-2xl overflow-hidden") ] [
            div [ attr.``class`` "flex items-center justify-between mb-4" ] [
                h2 [ attr.``class`` ("text-xl font-bold " + titleText theme) ] [
                    text "Players"
                ]

                button [
                    attr.``class`` ("w-9 h-9 rounded-xl border flex items-center justify-center " + modeStrongBorder mode + " " + accentText theme mode + " text-xl font-bold hover:bg-white/5")
                    on.click (fun _ _ -> addPlayer ())
                ] [
                    span [ attr.style "transform: translateY(-2px);" ] [
                        text "+"
                    ]
                ]
            ]

            Doc.BindView (fun currentPlayers ->
                Doc.BindView (fun currentId ->
                    let playerDocs =
                        currentPlayers
                        |> List.map (fun player -> playerRow theme mode currentId player)

                    div [ attr.``class`` "space-y-2 h-[330px] overflow-y-auto pr-1 redice-scroll-orange" ] playerDocs
                ) currentPlayerId.View
            ) players.View
        ]

    let recentRollsPanel theme mode =
        div [ attr.``class`` ("h-[420px] border " + panelBg theme + " " + modeBorder mode + " p-6 rounded-3xl shadow-2xl overflow-hidden") ] [
            h2 [ attr.``class`` ("text-2xl font-bold mb-1 " + titleText theme) ] [
                text "Recent Rolls"
            ]

            p [ attr.``class`` ("text-sm mb-5 " + mutedText theme) ] [
                text "Your latest dice rolls"
            ]

            Doc.BindView (fun history ->
                if List.isEmpty history then
                    div [ attr.``class`` ("h-[300px] flex flex-col items-center justify-center text-center " + mutedText theme) ] [
                        div [ attr.``class`` "w-14 h-14 rounded-2xl border border-white/20 flex items-center justify-center text-2xl mb-4" ] [
                            text "◷"
                        ]

                        div [ attr.``class`` ("text-lg font-semibold " + titleText theme) ] [
                            text "No recent rolls"
                        ]

                        div [ attr.``class`` ("text-sm mt-1 " + mutedText theme) ] [
                            text "Your roll history will show up here."
                        ]
                    ]
                else
                    let recentDocs =
                        history
                        |> List.filter (fun roll -> roll.Mode = mode)
                        |> recentFiveHistory
                        |> List.map (fun roll -> recentRollDoc theme mode roll)

                    div [ attr.``class`` "space-y-2 h-[315px] overflow-hidden pr-1" ] recentDocs
            ) rollHistory.View
        ]

    let distributionPanel theme mode =
        div [ attr.``class`` ("h-[330px] border " + panelBg theme + " " + modeBorder mode + " p-6 rounded-3xl shadow-2xl overflow-hidden") ] [
            div [ attr.``class`` "flex items-start justify-between mb-4" ] [
                div [] [
                    h2 [ attr.``class`` ("text-2xl font-bold " + titleText theme) ] [
                        text "Roll Distribution"
                    ]

                    p [ attr.``class`` ("text-sm mt-1 " + mutedText theme) ] [
                        text "Distribution of results for the selected dice."
                    ]
                ]
            ]

            Doc.BindView (fun diceOption ->
                Doc.BindView (fun rolls ->
                    match diceOption with
                    | None ->
                        div [ attr.``class`` ("h-56 flex flex-col items-center justify-center text-center " + mutedText theme) ] [
                            div [ attr.``class`` ("text-4xl mb-3 " + accentText theme mode) ] [
                                text "▥"
                            ]

                            div [ attr.``class`` ("text-lg font-semibold " + titleText theme) ] [
                                text "No data to display"
                            ]

                            div [ attr.``class`` ("text-sm mt-1 " + mutedText theme) ] [
                                text "Roll some dice to see the distribution."
                            ]
                        ]

                    | Some dice ->
                        let filteredRolls = rollsForDice mode dice rolls

                        if List.isEmpty filteredRolls then
                            div [ attr.``class`` ("h-56 flex flex-col items-center justify-center text-center " + mutedText theme) ] [
                                div [ attr.``class`` ("text-lg font-semibold " + titleText theme) ] [
                                    text ("No " + diceTypeToText dice + " rolls yet")
                                ]

                                div [ attr.``class`` ("text-sm mt-1 " + mutedText theme) ] [
                                    text "Roll this dice to create chart data."
                                ]
                            ]
                        else
                            match dice with
                            | D100 ->
                                customD100DistributionChart mode filteredRolls
                            | _ ->
                                let sides = diceSides dice
                                let data = getChartData sides filteredRolls
                                customDistributionChart mode data
                ) allRolls.View
            ) selectedDice.View
        ]

    let rollButtonDoc mode =
        Doc.BindView (fun diceOption ->
            Doc.BindView (fun countOption ->
                match diceOption, countOption with
                | Some _, Some _ ->
                    button [
                        attr.``class`` ("w-full py-4 " + modeGradient mode + " rounded-2xl font-bold tracking-wide shadow-lg flex items-center justify-center gap-3")
                        on.click (fun _ _ -> rollDice ())
                    ] [
                        rollButtonIcon mode
                        text "ROLL DICE"
                    ]

                | _ ->
                    button [
                        attr.``class`` "w-full py-4 bg-gray-700 text-gray-400 rounded-2xl font-bold tracking-wide cursor-not-allowed flex items-center justify-center gap-3"
                    ] [
                        rollButtonIcon mode
                        text "SELECT DICE FIRST"
                    ]
            ) selectedCount.View
        ) selectedDice.View

    let lastRollDesktop theme mode =
        div [ attr.``class`` "hidden md:block" ] [
            div [ attr.``class`` "grid grid-cols-1 md:grid-cols-[190px_1fr] gap-7 items-center" ] [
                div [ attr.``class`` ("w-36 h-36 rounded-3xl border-2 " + modeStrongBorder mode + " flex items-center justify-center " + modeGlow mode) ] [
                    h2 [ attr.``class`` "text-6xl font-black text-white leading-none drop-shadow-[0_0_16px_rgba(255,255,255,0.35)] -mt-2" ] [
                        Doc.BindView (fun total ->
                            if total = 0 then
                                text "-"
                            else
                                text (string total)
                        ) lastTotal.View
                    ]
                ]

                div [] [
                    div [ attr.``class`` ("text-2xl font-bold " + accentText theme mode) ] [
                        lastRollTitleDoc mode
                    ]

                    div [ attr.``class`` ("text-sm mt-2 " + mutedText theme) ] [
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

    let lastRollMobile theme mode =
        div [ attr.``class`` "md:hidden" ] [
            div [ attr.``class`` "grid grid-cols-[110px_1fr] gap-4 items-center" ] [
                div [ attr.``class`` ("rounded-2xl " + modeSoftBg mode + " px-4 py-3") ] [
                    p [ attr.``class`` ("text-xs uppercase tracking-wide " + mutedText theme) ] [
                        text "Total"
                    ]

                    div [ attr.``class`` ("text-4xl font-black mt-1 " + accentText theme mode) ] [
                        Doc.BindView (fun total ->
                            if total = 0 then
                                text "-"
                            else
                                text (string total)
                        ) lastTotal.View
                    ]
                ]

                div [] [
                    div [ attr.``class`` ("text-xl font-bold " + accentText theme mode) ] [
                        lastRollTitleDoc mode
                    ]

                    div [ attr.``class`` ("text-sm mt-1 " + mutedText theme) ] [
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

    let rollDicePanel theme mode =
        let optionalCurrentPlayer =
            match mode with
            | SingleMode -> []
            | MultiMode -> [ currentPlayerPanel theme mode ]

        let firstPart =
            [
                div [ attr.``class`` "mb-6" ] [
                    h2 [ attr.``class`` ("text-2xl font-bold uppercase " + titleText theme) ] [
                        text "Roll Dice"
                    ]

                    p [ attr.``class`` ("text-sm mt-1 " + mutedText theme) ] [
                        text "Choose your dice setup and start rolling."
                    ]
                ]

                div [ attr.``class`` "grid grid-cols-1 md:grid-cols-2 gap-4 mb-5" ] [
                    div [ attr.``class`` ("border rounded-2xl p-4 " + innerBoxBg theme) ] [
                        p [ attr.``class`` ("text-xs uppercase tracking-wide mb-3 " + mutedText theme) ] [
                            text "Dice Type"
                        ]

                        div [ attr.``class`` "relative" ] [
                            div [ attr.``class`` "absolute left-4 top-1/2 -translate-y-1/2 pointer-events-none z-10" ] [
                                Doc.BindView (fun diceOption ->
                                    diceIconSmall mode diceOption
                                ) selectedDice.View
                            ]

                            select [
                                attr.``class`` ("w-full h-12 pl-14 pr-4 rounded-xl outline-none " + inputBg theme)
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

                    div [ attr.``class`` ("border rounded-2xl p-4 " + innerBoxBg theme) ] [
                        p [ attr.``class`` ("text-xs uppercase tracking-wide mb-3 " + mutedText theme) ] [
                            text "Number of Dice"
                        ]

                        select [
                            attr.``class`` ("w-full h-12 px-4 rounded-xl outline-none " + inputBg theme)
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

                div [ attr.``class`` ("mb-5 " + modeSoftBg mode + " border " + modeStrongBorder mode + " rounded-2xl p-5 flex items-center justify-between shadow-[0_0_25px_rgba(168,85,247,0.18)]") ] [
                    div [] [
                        p [ attr.``class`` ("text-xs uppercase tracking-wide " + mutedText theme) ] [
                            text "Current setup"
                        ]

                        div [ attr.``class`` ("text-2xl font-bold mt-1 " + titleText theme) ] [
                            rollSetupDoc ()
                        ]
                    ]

                    Doc.BindView (fun diceOption ->
                        diceIconMedium mode diceOption
                    ) selectedDice.View
                ]

                rollButtonDoc mode
            ]

        let lastPart =
            [
                div [ attr.``class`` ("mt-4 border " + modeBorder mode + " rounded-3xl px-5 py-4 md:px-6 md:py-5 md:flex-1 min-h-0 overflow-hidden shadow-[0_0_30px_rgba(168,85,247,0.08)] " + modeSoftBg mode) ] [
                    div [ attr.``class`` "md:hidden" ] [
                        h2 [ attr.``class`` ("text-lg font-bold mb-3 " + titleText theme) ] [
                            text "Last Roll"
                        ]

                        div [ attr.``class`` "grid grid-cols-[85px_1fr] gap-4 items-center" ] [
                            div [ attr.``class`` ("text-4xl font-black leading-none " + accentText theme mode) ] [
                                Doc.BindView (fun total ->
                                    if total = 0 then
                                        text "-"
                                    else
                                        text (string total)
                                ) lastTotal.View
                            ]

                            div [ attr.``class`` "min-w-0" ] [
                                div [ attr.``class`` ("text-base font-bold truncate " + accentText theme mode) ] [
                                    lastRollTitleDoc mode
                                ]

                                div [ attr.``class`` ("text-xs mt-1 truncate " + mutedText theme) ] [
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

                    div [ attr.``class`` "hidden md:flex md:flex-col md:justify-center h-full" ] [
                        h2 [ attr.``class`` ("text-2xl font-bold mb-4 " + titleText theme) ] [
                            text "Last Roll"
                        ]

                        div [ attr.``class`` "grid grid-cols-1 md:grid-cols-[190px_1fr] gap-7 items-center" ] [
                            div [ attr.``class`` ("w-36 h-36 rounded-3xl border-2 " + modeStrongBorder mode + " flex items-center justify-center " + modeGlow mode) ] [
                                h2 [ attr.``class`` ("text-6xl font-black leading-none -mt-2 " + accentText theme mode) ] [
                                    Doc.BindView (fun total ->
                                        if total = 0 then
                                            text "-"
                                        else
                                            text (string total)
                                    ) lastTotal.View
                                ]
                            ]

                            div [] [
                                div [ attr.``class`` ("text-2xl font-bold " + accentText theme mode) ] [
                                    lastRollTitleDoc mode
                                ]

                                div [ attr.``class`` ("text-sm mt-2 " + mutedText theme) ] [
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

        let children =
            firstPart @ optionalCurrentPlayer @ lastPart

        div [ attr.``class`` ("h-[774px] border " + panelBg theme + " " + modeBorder mode + " p-6 rounded-3xl shadow-2xl overflow-hidden flex flex-col") ] children

    let statsPanels theme mode =
        match mode with
        | SingleMode ->
            div [ attr.``class`` "space-y-6" ] [
                distributionPanel theme mode
                recentRollsPanel theme mode
            ]
        | MultiMode ->
            div [ attr.``class`` "space-y-6" ] [
                distributionPanel theme mode

                div [ attr.``class`` "grid grid-cols-1 lg:grid-cols-[0.8fr_1.2fr] gap-6" ] [
                    playersPanel theme mode
                    recentRollsPanel theme mode
                ]
            ]

    let mobileStatsToggle theme mode =
        div [ attr.``class`` "xl:hidden mt-6" ] [
            button [
                attr.``class`` ("w-full py-4 rounded-2xl border " + modeStrongBorder mode + " " + accentText theme mode + " font-bold")
                on.click (fun _ _ -> showMobileStats.Set (not showMobileStats.Value))
            ] [
                Doc.BindView (fun visible ->
                    if visible then
                        text "HIDE STATISTICS ↑"
                    else
                        text "SHOW STATISTICS ↓"
                ) showMobileStats.View
            ]

            Doc.BindView (fun visible ->
                if visible then
                    div [ attr.``class`` "mt-6" ] [
                        statsPanels theme mode
                    ]
                else
                    Doc.Empty
            ) showMobileStats.View
        ]

    let pageLayout theme mode =
        div [ attr.``class`` "grid grid-cols-1 xl:grid-cols-2 gap-6 items-start" ] [
            div [ attr.``class`` "space-y-6" ] [
                rollDicePanel theme mode
                mobileStatsToggle theme mode
            ]

            div [ attr.``class`` "hidden xl:block" ] [
                statsPanels theme mode
            ]
        ]

    let page theme mode currentPage =
        div [ attr.``class`` ("min-h-screen " + appBg theme) ] [
            mobileNavbar theme mode currentPage

            div [ attr.``class`` "flex" ] [
                sidebar theme mode currentPage

                div [ attr.id "main-content"; attr.``class`` "flex-1" ] [
                    div [ attr.``class`` "max-w-[1420px] mx-auto p-4 md:p-6 xl:p-8" ] [
                        div [ attr.``class`` "mb-7 flex flex-col lg:flex-row lg:items-center lg:justify-between gap-5" ] [
                            div [] [
                                h1 [ attr.``class`` ("text-3xl md:text-4xl font-bold uppercase tracking-wide " + accentText theme mode) ] [
                                    text (modeTitle mode)
                                ]

                                p [ attr.``class`` ("mt-2 " + mutedText theme) ] [
                                    text "Roll your dice and track your adventure."
                                ]
                            ]

                            div [ attr.``class`` "flex items-center gap-3" ] [
                                div [ attr.``class`` ("flex border rounded-2xl p-1 shadow-lg " + panelBg theme + " " + modeBorder mode) ] [
                                    modeButton SingleMode mode
                                    modeButton MultiMode mode
                                ]

                                button [
                                    attr.``class`` ("w-11 h-11 rounded-xl border flex items-center justify-center text-xl " + panelBg theme + " " + modeBorder mode + " " + titleText theme)
                                    on.click (fun _ _ -> toggleTheme ())
                                ] [
                                    span [ attr.style "transform: translateY(-1px);" ] [
                                        text (
                                            match theme with
                                            | DarkTheme -> "☼"
                                            | LightTheme -> "☾"
                                        )
                                    ]
                                ]
                            ]
                        ]

                        match currentPage with
                        | HomePage ->
                            pageLayout theme mode

                        | HistoryPage ->
                            historyPageLayout theme mode

                        | ImportExportPage ->
                            importExportPageLayout theme mode

                        | AboutPage ->
                            aboutPageLayout theme mode
                    ]
                ]
            ]
        ]

    [<SPAEntryPoint>]
    let Main () =
        Doc.BindView (fun mode ->
            Doc.BindView (fun theme ->
                Doc.BindView (fun currentPage ->
                    page theme mode currentPage
                ) selectedPage.View
            ) selectedTheme.View
        ) selectedMode.View
        |> Doc.RunById "main"