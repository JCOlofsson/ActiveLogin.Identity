module ActiveLogin.Identity.Swedish.FSharp.Test.PinTestHelpers
open ActiveLogin.Identity.Swedish.FSharp


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