/// <remarks>
/// Tested with offical test Personal Identity Numbers from Skatteverket:
/// https://skatteverket.entryscape.net/catalog/9/datasets/147
/// </remarks>
module ActiveLogin.Identity.Swedish.FSharp.Test.SwedishPersonalIdentityNumber_hash
open Expecto
open Generators
open Swensen.Unquote


[<Tests>]
let tests =
    testList "SwedishPersonalIdentityNumber hash" 
        [ testPropIdentical "Identical pins have the same hash code" <|
          fun (pin1, pin2) ->
            hash pin1 =! hash pin2
        ]
