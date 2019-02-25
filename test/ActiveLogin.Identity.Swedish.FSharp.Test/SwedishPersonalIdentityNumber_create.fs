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


[<Tests>]
let tests =
    testList "create" 
        [ testPropertyWithConfig invalidYearConfig "Invalid year" <|
            fun values ->
                let result = 
                    values
                    |> SwedishPersonalIdentityNumber.create
                result =! Error (InvalidYear values.Year) 

          testPropertyWithConfig invalidMonthConfig "Invalid month" <|
            fun values ->
                let result = 
                    values
                    |> SwedishPersonalIdentityNumber.create
                result =! Error (InvalidMonth values.Month)

          testPropertyWithConfig invalidDayConfig "Invalid day" <|
            fun values ->
                let result =
                    values
                    |> SwedishPersonalIdentityNumber.create
                result =! Error (InvalidDayAndCoordinationDay values.Day)

          testPropertyWithConfig validValuesConfig "Possible coordination-number day" <|
            fun values ->
                let coordinationDay = values.Day + 60
                let result =
                    { values with Day = coordinationDay }
                    |> SwedishPersonalIdentityNumber.create
                result =! Error (InvalidDay coordinationDay)

          testPropertyWithConfig invalidBirthNumberConfig "Invalid birthnumber" <|
            fun values ->
                let result =
                    values
                    |> SwedishPersonalIdentityNumber.create
                result =! Error (InvalidBirthNumber values.BirthNumber)

          testPropertyWithConfig validValuesConfig "With all other values valid only 1 out of 10 checksums is valid" <|
            fun values ->
                let isValid x = match x with Ok _ -> true | Error _ -> false
                let results =
                    [ 0..9 ]
                    |> List.map (fun checksum -> 
                        { values with SwedishPersonalIdentityNumberValues.Checksum = checksum }
                        |> SwedishPersonalIdentityNumber.create) 
                let numValid = results |> List.filter isValid |> List.length 
                numValid =! 1

          testCase "FSharp should have no public constructor" <|
            fun () ->
                let typ = typeof<SwedishPersonalIdentityNumber>
                let numConstructors = typ.GetConstructors(BindingFlags.Public) |> Array.length
                numConstructors =! 0 ]