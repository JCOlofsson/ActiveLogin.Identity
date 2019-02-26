module ActiveLogin.Identity.Swedish.FSharp.Test.Generators
open ActiveLogin.Identity.Swedish.FSharp
open Expecto
open FsCheck
open System
open ActiveLogin.Identity.Swedish.FSharp.TestData
open ActiveLogin.Identity.Swedish.FSharp.Test.PinTestHelpers

let chooseFromArray xs =
    gen {
        let! index = Gen.choose(0, (Array.length xs) - 1)
        return xs.[index]
    }

let valid12Digit = chooseFromArray SwedishPersonalIdentityNumberTestData.raw12DigitStrings

let private stringToValues (pin:string) =
    { Year = pin.[ 0..3 ] |> int
      Month = pin.[ 4..5 ] |> int
      Day = pin.[ 6..7 ] |> int
      BirthNumber = pin.[ 8..10 ] |> int
      Checksum = pin.[ 11..11 ] |> int }

let validValues =
    gen {
        let! str = valid12Digit
        return stringToValues str
    }

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
            yield (tenDigit, pin |> pinToValues)
    }

let random10DigitWithDashDelimiter =
    seq { 
        for pin in SwedishPersonalIdentityNumberTestData.allPinsShuffled() do
        let tenDigit = pin |> SwedishPersonalIdentityNumber.to10DigitStringInSpecificYear parseYear
        if tenDigit.Contains("+") |> not then
            yield (tenDigit, pin |> pinToValues)
    }

let invalidYear =
    gen {
        let! year =
            Gen.oneof 
                [ (Gen.choose(Int32.MinValue, DateTime.MinValue.Year - 1)) 
                  (Gen.choose(DateTime.MaxValue.Year + 1, Int32.MaxValue)) ]
        let! values = validValues
        return { values with Year = year }
    }
type InvalidYearGen() =
    static member Year() : Arbitrary<SwedishPersonalIdentityNumberValues> = Arb.fromGen invalidYear
let invalidYearConfig = { FsCheckConfig.defaultConfig with arbitrary = [typeof<InvalidYearGen> ] }

let invalidMonth = 
    gen {
        let! month =
            Gen.oneof 
                [ (Gen.choose(Int32.MinValue, 0))
                  (Gen.choose(13, Int32.MaxValue)) ]
        let! values = validValues
        return { values with Month = month }
    }
type InvalidMonthGen() = 
    static member Month() : Arbitrary<SwedishPersonalIdentityNumberValues> = Arb.fromGen invalidMonth
let invalidMonthConfig = { FsCheckConfig.defaultConfig with arbitrary = [typeof<InvalidMonthGen> ] }

let invalidDay = 
    gen {
        let! values = validValues
        let daysInMonth = DateTime.DaysInMonth(values.Year, values.Month)
        let! day = Gen.oneof [ Gen.choose(Int32.MinValue, 0)
                               Gen.choose(daysInMonth + 1, Int32.MaxValue) ]
        return { values with Day = day }
    }
type InvalidDayGen() =
    static member Day() : Arbitrary<SwedishPersonalIdentityNumberValues> = Arb.fromGen invalidDay
let invalidDayConfig = { FsCheckConfig.defaultConfig with arbitrary = [typeof<InvalidDayGen> ] }

let invalidBirthNumber = 
    gen {
        let! birthNumber =
            Gen.oneof [ Gen.choose(Int32.MinValue, 0); Gen.choose(1000, Int32.MaxValue) ]
        let! values = validValues
        return { values with BirthNumber = birthNumber }
    }
type InvalidBirthNumberGen() =
    static member BirthNumber() : Arbitrary<SwedishPersonalIdentityNumberValues> = Arb.fromGen invalidBirthNumber
let invalidBirthNumberConfig = { FsCheckConfig.defaultConfig with arbitrary = [typeof<InvalidBirthNumberGen> ] }

type ValidValues() =
    static member ValidValues() : Arbitrary<SwedishPersonalIdentityNumberValues> = 
        gen { return! validValues } 
        |> Arb.fromGen 
let validValuesConfig = { FsCheckConfig.defaultConfig with arbitrary = [ typeof<ValidValues> ] }

type Valid12Digit() =
    static member Valid12Digit() : Arbitrary<string * SwedishPersonalIdentityNumberValues> =
        gen {
            let! str = valid12Digit
            return (str, stringToValues str)
        } |> Arb.fromGen
let valid12DigitConfig = 
    { FsCheckConfig.defaultConfig with arbitrary = [ typeof<Valid12Digit> ] }

let validPin =
    gen {
        return SwedishPersonalIdentityNumberTestData.getRandom()
    }

type ValidPin() =
    static member ValidPin() : Arbitrary<SwedishPersonalIdentityNumber> =
        validPin |> Arb.fromGen
let validPinConfig = { FsCheckConfig.defaultConfig with arbitrary = [typeof<ValidPin>]}

type TwoEqualPins() =
    static member TwoEqualPins() : Arbitrary<SwedishPersonalIdentityNumber * SwedishPersonalIdentityNumber> =
        gen {
            let! pin = validPin
            return (pin, pin)
        } |> Arb.fromGen
let twoEqualPinsConfig = { FsCheckConfig.defaultConfig with arbitrary = [typeof<TwoEqualPins>] }