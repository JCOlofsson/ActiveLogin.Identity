/// <remarks>
/// Tested with offical test Personal Identity Numbers from Skatteverket:
/// https://skatteverket.entryscape.net/catalog/9/datasets/147
/// </remarks>
module ActiveLogin.Identity.Swedish.FSharp.Test.SwedishPersonalIdentityNumber_equality

open Swensen.Unquote
open Expecto
open ActiveLogin.Identity.Swedish.FSharp
open ActiveLogin.Identity.Swedish.FSharp.TestData
open Generators
open FsCheck


type TwoPins() =
    static member TwoPins() : Arbitrary<SwedishPersonalIdentityNumber * SwedishPersonalIdentityNumber> =
        gen {
            let pin1 = SwedishPersonalIdentityNumberTestData.getRandom()
            let pin2 = SwedishPersonalIdentityNumberTestData.getRandom()
            return (pin1, pin2)
        } |> Arb.fromGen
let twoPinsConfig = { FsCheckConfig.defaultConfig with arbitrary = [typeof<TwoPins>] }
let testPropDifferent : string -> (SwedishPersonalIdentityNumber * SwedishPersonalIdentityNumber -> Property) -> Test = 
    testPropertyWithConfig twoPinsConfig 

[<Tests>]
let tests =
    testList "equality" 
        [ testPropIdentical "Identical pins are equal when using operator" <|
            fun (pin1, pin2) ->
                pin1 =! pin2 
                pin2 =! pin1 
          
          testPropIdentical "Identical pins are equal when using .Equals()" <|
            fun (pin1, pin2) ->
                pin1.Equals(pin2) =! true
                pin2.Equals(pin1) =! true
          testPropIdentical "Identical pins are equal when using .Equals() and one pin is object" <|
            fun (pin1, pin2) ->
                let pin2 = pin2 :> obj

                pin1.Equals(pin2) =! true
                pin2.Equals(pin1) =! true
          testPropDifferent "Different pins are not equal" <|
            fun (pin1, pin2) ->
                pin1 <> pin2 ==> lazy 
                pin1 <>! pin2
                pin2 <>! pin1
          testPropDifferent "Different pins are not equal using .Equals()" <|
            fun (pin1, pin2) ->
                pin1 <> pin2 ==> lazy
                pin1.Equals(pin2) =! false
                pin2.Equals(pin1) =! false
          testPropValidPin "A pin is not equal to null using .Equals()" <|
            fun (pin) ->
                pin.Equals(null) =! false
          testPropValidPin "A pin is not equal to object null using .Equals()" <|
            fun pin ->
                let nullObject = null :> obj
                pin.Equals(nullObject) =! false ]
