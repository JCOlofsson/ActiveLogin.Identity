﻿module ConsoleSample.FSharp

open ActiveLogin.Identity.Swedish.FSharp
open System

let sampleStrings = [ "990913+9801"; "120211+9986"; "990807-2391"; "180101-2392"; "180101.2392"; "ABC" ]

let parseAndPrintPersonalIdentityNumber str =
    let printHeader str =
        str
        |> printfn "Input: %s\n----------------------"

    let print10DigitString pin =
        pin
        |> SwedishPersonalIdentityNumber.to10DigitString
        |> printfn "SwedishPersonalIdentityNumber.to10DigitString: %s"

    let print12DigitString pin =
        pin
        |> SwedishPersonalIdentityNumber.to12DigitString
        |> printfn "SwedishPersonalIdentityNumber.to12DigitString: %s"

    let printDateOfBirthHint pin =
        let date = pin |> SwedishPersonalIdentityNumber.Hints.getDateOfBirthHint
        date.ToShortDateString() |> printfn "SwedishPersonalIdentityNumber.Hints.getDateOfBirthHint: %s"

    let printAgeHint pin =
        pin
        |> SwedishPersonalIdentityNumber.Hints.getAgeHintOnDate DateTime.UtcNow
        |> Option.defaultValue 0
        |> printfn "SwedishPersonalIdentityNumber.Hints.getAgeHintOnDate: %i"

    let printGenderHint pin =
        let gender = pin |> SwedishPersonalIdentityNumber.Hints.getGenderHint
        gender.ToString() |> printfn "SwedishPersonalIdentityNumber.Hints.getGenderHint: %s"

    printHeader str
    match SwedishPersonalIdentityNumber.parse str with
    | Ok pin ->
        printfn "SwedishPersonalIdentityNumber:"
        printfn "%A" pin
        print10DigitString pin
        print12DigitString pin
        printDateOfBirthHint pin
        printAgeHint pin
        printGenderHint pin
    | Error e -> printfn "%A: Unable to parse the input as a SwedishPersonalIdentityNumber." e
    printf "\n\n"

[<EntryPoint>]
let main argv =
    printfn "Sample showing possible uses of SwedishPersonalIdentityNumber."
    printf "\n\n"
    sampleStrings |> List.iter parseAndPrintPersonalIdentityNumber
    printfn "What is your (Swedish) Personal Identity Number?"
    let userInput = Console.ReadLine()
    parseAndPrintPersonalIdentityNumber userInput
    Console.ReadLine() |> ignore
    0 // return an integer exit code
