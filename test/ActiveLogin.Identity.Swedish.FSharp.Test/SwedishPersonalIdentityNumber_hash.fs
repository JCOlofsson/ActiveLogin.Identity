/// <remarks>
/// Tested with offical test Personal Identity Numbers from Skatteverket:
/// https://skatteverket.entryscape.net/catalog/9/datasets/147
/// </remarks>
module ActiveLogin.Identity.Swedish.FSharp.Test.SwedishPersonalIdentityNumber_hash
open Expecto
open Arbitraries
open Swensen.Unquote
open ActiveLogin.Identity.Swedish.FSharp


[<Tests>]
let tests =
    testList "hash" 
        [ testPropertyWithConfig twoEqualPinsConfig "Identical pins have the same hash code" <|
          fun (pin1: SwedishPersonalIdentityNumber, pin2: SwedishPersonalIdentityNumber) ->
            hash pin1 =! hash pin2 ]
