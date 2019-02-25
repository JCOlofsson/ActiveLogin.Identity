/// <remarks>
/// Tested with offical test Personal Identity Numbers from Skatteverket:
/// https://skatteverket.entryscape.net/catalog/9/datasets/147
/// </remarks>
module ActiveLogin.Identity.Swedish.FSharp.Test.SwedishPersonalIdentityNumber_create
open Swensen.Unquote
open Expecto
open ActiveLogin.Identity.Swedish.FSharp
open System.Reflection
open Generators

let defaultValues = 
    { Year = 2018; Month = 01; Day = 01; BirthNumber = 239; Checksum = 2 }

[<Tests>]
let tests =
    testList "create" 
        [ testPropInvalidYear "Invalid year" <|
            fun year ->
                let result = 
                    { defaultValues with Year = year } 
                    |> SwedishPersonalIdentityNumber.create

                result =! Error (InvalidYear year) 
          testPropInvalidMonth "Invalid month" <|
            fun month ->
                let result = 
                    { defaultValues with Month = month } 
                    |> SwedishPersonalIdentityNumber.create

                result =! Error (InvalidMonth month)
          testPropInvalidDay "Invalid day" <|
            fun (year, month, day) ->
                let result =
                    { defaultValues with Year = year; Month = month; Day = day }
                    |> SwedishPersonalIdentityNumber.create

                result =! Error (InvalidDayAndCoordinationDay day)
          testPropValidDay "Possible coordination-number day" <|
            fun (year, month, day) ->
                let coordinationDay = day + 60
                let result =
                    { defaultValues with Year = year; Month = month; Day = coordinationDay }
                    |> SwedishPersonalIdentityNumber.create

                result =! Error (InvalidDay coordinationDay)
          testPropInvalidBirthNumber "Invalid birthnumber" <|
            fun (birthNumber) ->
                let result =
                    { defaultValues with BirthNumber = birthNumber }
                    |> SwedishPersonalIdentityNumber.create

                result =! Error (InvalidBirthNumber birthNumber)
          testPropValidWithoutChecksum "With valid values only 1 out of 10 checksums is valid" <|
            fun (year, month, day, birthNumber) ->
                let isValid x = match x with Ok _ -> true | Error _ -> false
                let results =
                    [ 0..9 ]
                    |> List.map (fun checksum -> 
                        { defaultValues with
                            Year = year 
                            Month = month
                            Day = day
                            BirthNumber = birthNumber
                            Checksum = checksum }
                        |> SwedishPersonalIdentityNumber.create) 
                let numValid = results |> List.filter isValid |> List.length 
                numValid =! 1
          testCase "FSharp should have no public constructor" <|
            fun () ->
                let typ = typeof<SwedishPersonalIdentityNumber>
                let numConstructors = typ.GetConstructors(BindingFlags.Public) |> Array.length
                numConstructors =! 0 ]