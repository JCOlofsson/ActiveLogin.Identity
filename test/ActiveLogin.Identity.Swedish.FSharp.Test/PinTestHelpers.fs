module ActiveLogin.Identity.Swedish.FSharp.Test.PinTestHelpers
open ActiveLogin.Identity.Swedish.FSharp
open ActiveLogin.Identity.Swedish.FSharp.TestData
open System
open Swensen.Unquote
open Expecto.Flip


let quickParseR (str:string) = 
    let values = 
        { Year = str.[ 0..3 ] |> int
          Month = str.[ 4..5 ] |> int 
          Day = str.[ 6..7 ] |> int
          BirthNumber = str.[ 8..10 ] |> int
          Checksum = str.[ 11..11 ] |> int }
    SwedishPersonalIdentityNumber.create values

let quickParse str = 
    match quickParseR str with 
    | Ok p -> p 
    | Error e -> e.ToString() |> failwithf "Test setup error %s"

let private parseYear = 
    DateTime.Today.Year 
    |> Year.create 
    |> function 
    | Ok y -> y 
    | Error e -> e |> failwith "Test setup error %A"

let random10DigitWithPlusDelimiter =
    seq { 
        for pin in SwedishPersonalIdentityNumberTestData.allPinsShuffled() do
        let tenDigit = pin |> SwedishPersonalIdentityNumber.to10DigitStringInSpecificYear parseYear
        if tenDigit.Contains("+") then
            yield (tenDigit, pin)
    }

let random10DigitWithDashDelimiter =
    seq { 
        for pin in SwedishPersonalIdentityNumberTestData.allPinsShuffled() do
        let tenDigit = pin |> SwedishPersonalIdentityNumber.to10DigitStringInSpecificYear parseYear
        if tenDigit.Contains("+") |> not then
            yield (tenDigit, pin)
    }

module Expect =
    let equalPin (expected: SwedishPersonalIdentityNumber) (actual: Result<SwedishPersonalIdentityNumber,_>) =
        actual |> Expect.isOk "should be ok"
        match actual with
        | Error _ -> failwith "test error"
        | Ok pin ->
            pin.Year =! expected.Year
            pin.Month =! expected.Month
            pin.Day =! expected.Day
            pin.BirthNumber =! expected.BirthNumber
            pin.Checksum =! expected.Checksum

