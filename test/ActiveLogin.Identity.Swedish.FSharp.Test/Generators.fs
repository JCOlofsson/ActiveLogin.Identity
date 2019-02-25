module ActiveLogin.Identity.Swedish.FSharp.Test.Generators
open ActiveLogin.Identity.Swedish.FSharp
open Expecto
open FsCheck
open System
open ActiveLogin.Identity.Swedish.FSharp.TestData


let validValues() =
    let toValues (pin:SwedishPersonalIdentityNumber) =
        { Year = pin.Year |> Year.value 
          Month = pin.Month |> Month.value
          Day = pin.Day |> Day.value
          BirthNumber = pin.BirthNumber |> BirthNumber.value
          Checksum = pin.Checksum |> Checksum.value }
    SwedishPersonalIdentityNumberTestData.getRandom() |> toValues

let invalidYear =
    gen {
        let! year =
            Gen.oneof 
                [ (Gen.choose(Int32.MinValue, DateTime.MinValue.Year - 1)) 
                  (Gen.choose(DateTime.MaxValue.Year + 1, Int32.MaxValue)) ]
        return { validValues() with Year = year }
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
        return { validValues() with Month = month }
    }
type InvalidMonthGen() = 
    static member Month() : Arbitrary<SwedishPersonalIdentityNumberValues> = Arb.fromGen invalidMonth
let invalidMonthConfig = { FsCheckConfig.defaultConfig with arbitrary = [typeof<InvalidMonthGen> ] }

let invalidDay = gen {
    let values = validValues()
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
        return { validValues() with BirthNumber = birthNumber }
    }
type InvalidBirthNumberGen() =
    static member BirthNumber() : Arbitrary<SwedishPersonalIdentityNumberValues> = Arb.fromGen invalidBirthNumber
let invalidBirthNumberConfig = { FsCheckConfig.defaultConfig with arbitrary = [typeof<InvalidBirthNumberGen> ] }

type ValidValues() =
    static member ValidValues() : Arbitrary<SwedishPersonalIdentityNumberValues> = 
        gen { return validValues() } 
        |> Arb.fromGen 
let validValuesConfig = { FsCheckConfig.defaultConfig with arbitrary = [ typeof<ValidValues> ] }

type Pin12DigitString() =
    static member Pin12DigitString() : Arbitrary<string * SwedishPersonalIdentityNumber> =
        gen {
            let pin = SwedishPersonalIdentityNumberTestData.getRandom()
            return (SwedishPersonalIdentityNumber.to12DigitString pin, pin)
        } |> Arb.fromGen
let pin12DigitStringConfig = 
    { FsCheckConfig.defaultConfig with arbitrary = [ typeof<Pin12DigitString> ] }

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