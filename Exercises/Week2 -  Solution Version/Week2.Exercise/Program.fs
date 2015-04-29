module Program

open System.Diagnostics

[<EntryPoint>]
let main args = 
    Exercise.drawAndSaveFractalTree 1920 1080
    //CornerHitterShortcutVector.drawAndSaveFractalTree 1920 1080

    printfn "DONE!"
    //System.Console.ReadLine() |> ignore
    0