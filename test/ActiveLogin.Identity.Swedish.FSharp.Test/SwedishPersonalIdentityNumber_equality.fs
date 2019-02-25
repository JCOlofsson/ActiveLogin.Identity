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

[<Tests>]
let tests =
    testList "equality" 
        [ testPropertyWithConfig twoEqualPinsConfig "Identical pins are equal when using operator" <|
            fun (pin1: SwedishPersonalIdentityNumber, pin2: SwedishPersonalIdentityNumber) ->
                pin1 = pin2 =! true
                pin2 = pin1 =! true
          
          testPropertyWithConfig twoEqualPinsConfig "Identical pins are equal when using .Equals()" <|
            fun (pin1: SwedishPersonalIdentityNumber, pin2: SwedishPersonalIdentityNumber) ->
                pin1.Equals(pin2) =! true
                pin2.Equals(pin1) =! true

          testPropertyWithConfig twoEqualPinsConfig "Identical pins are equal when using .Equals() and one pin is object" <|
            fun (pin1: SwedishPersonalIdentityNumber, pin2: SwedishPersonalIdentityNumber) ->
                let pin2 = pin2 :> obj
                pin1.Equals(pin2) =! true
                pin2.Equals(pin1) =! true

          testPropertyWithConfig twoPinsConfig "Different pins are not equal" <|
            fun (pin1: SwedishPersonalIdentityNumber, pin2: SwedishPersonalIdentityNumber) ->
                pin1 <> pin2 ==> lazy 
                pin1 <> pin2 =! true
                pin2 <> pin1 =! true

          testPropertyWithConfig twoPinsConfig "Different pins are not equal using .Equals()" <|
            fun (pin1: SwedishPersonalIdentityNumber, pin2: SwedishPersonalIdentityNumber) ->
                pin1 <> pin2 ==> lazy
                pin1.Equals(pin2) =! false
                pin2.Equals(pin1) =! false

          testPropertyWithConfig validPinConfig "A pin is not equal to null using .Equals()" <|
            fun (pin:SwedishPersonalIdentityNumber) ->
                pin.Equals(null) =! false

          testPropertyWithConfig validPinConfig "A pin is not equal to object null using .Equals()" <|
            fun (pin: SwedishPersonalIdentityNumber) ->
                let nullObject = null :> obj
                pin.Equals(nullObject) =! false ]
