module ActiveLogin.Identity.Swedish.FSharp.Test.Generators
open ActiveLogin.Identity.Swedish.FSharp
open Expecto
open FsCheck
open System
open ActiveLogin.Identity.Swedish.FSharp.TestData
open FsCheck

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

let invalidMonth = 
    gen {
        let! month =
            Gen.oneof 
                [ (Gen.choose(Int32.MinValue, 0))
                  (Gen.choose(13, Int32.MaxValue)) ]
        return { validValues() with Month = month }
    }
let invalidDay = gen {
    let values = validValues()
    let daysInMonth = DateTime.DaysInMonth(values.Year, values.Month)
    let! day = Gen.oneof [ Gen.choose(Int32.MinValue, 0)
                           Gen.choose(daysInMonth + 1, Int32.MaxValue) ]
    return { values with Day = day }
}
let invalidBirthNumber = 
    gen {
        let! birthNumber =
            Gen.oneof [ Gen.choose(Int32.MinValue, 0); Gen.choose(1000, Int32.MaxValue) ]
        return { validValues() with BirthNumber = birthNumber }
    }

type InvalidYearGen() =
    static member Year() : Arbitrary<SwedishPersonalIdentityNumberValues> = Arb.fromGen invalidYear
let invalidYearConfig = { FsCheckConfig.defaultConfig with arbitrary = [typeof<InvalidYearGen> ] }
let testPropInvalidYear : string -> (SwedishPersonalIdentityNumberValues -> unit) -> Test = 
    testPropertyWithConfig invalidYearConfig

type InvalidMonthGen() = 
    static member Month() : Arbitrary<SwedishPersonalIdentityNumberValues> = Arb.fromGen invalidMonth
let invalidMonthConfig = { FsCheckConfig.defaultConfig with arbitrary = [typeof<InvalidMonthGen> ] }
let testPropInvalidMonth : string -> (SwedishPersonalIdentityNumberValues -> unit) -> Test = 
    testPropertyWithConfig invalidMonthConfig

type InvalidDayGen() =
    static member Day() : Arbitrary<SwedishPersonalIdentityNumberValues> = Arb.fromGen invalidDay
let invalidDayConfig = { FsCheckConfig.defaultConfig with arbitrary = [typeof<InvalidDayGen> ] }
let testPropInvalidDay : string -> (SwedishPersonalIdentityNumberValues -> unit) -> Test = 
    testPropertyWithConfig invalidDayConfig

type InvalidBirthNumberGen() =
    static member BirthNumber() : Arbitrary<SwedishPersonalIdentityNumberValues> = Arb.fromGen invalidBirthNumber
let invalidBirthNumberConfig = { FsCheckConfig.defaultConfig with arbitrary = [typeof<InvalidBirthNumberGen> ] }
let testPropInvalidBirthNumber : string -> (SwedishPersonalIdentityNumberValues -> unit) -> Test = testPropertyWithConfig invalidBirthNumberConfig

type ValidValues() =
    static member ValidValues() : Arbitrary<SwedishPersonalIdentityNumberValues> = 
        gen { return validValues() } 
        |> Arb.fromGen 
let validValuesConfig = { FsCheckConfig.defaultConfig with arbitrary = [ typeof<ValidValues> ] }
let testPropValidValues : string -> (SwedishPersonalIdentityNumberValues -> unit) -> Test = 
    testPropertyWithConfig validValuesConfig

type Pin12DigitString() =
    static member Pin12DigitString() : Arbitrary<string * SwedishPersonalIdentityNumber> =
        gen {
            let pin = SwedishPersonalIdentityNumberTestData.getRandom()
            return (SwedishPersonalIdentityNumber.to12DigitString pin, pin)
        } |> Arb.fromGen
let pin12DigitStringConfig = 
    { FsCheckConfig.defaultConfig with arbitrary = [ typeof<Pin12DigitString> ] }
let testProp12DigitString : string -> (string * SwedishPersonalIdentityNumber -> unit) -> Test = testPropertyWithConfig pin12DigitStringConfig

let validPin =
    gen {
        return SwedishPersonalIdentityNumberTestData.getRandom()
    }

type ValidPin() =
    static member ValidPin() : Arbitrary<SwedishPersonalIdentityNumber> =
        validPin |> Arb.fromGen
let validPinConfig = { FsCheckConfig.defaultConfig with arbitrary = [typeof<ValidPin>]}
let testPropValidPin : string -> (SwedishPersonalIdentityNumber -> unit) -> Test = testPropertyWithConfig validPinConfig

type TwoEqualPins() =
    static member TwoEqualPins() : Arbitrary<SwedishPersonalIdentityNumber * SwedishPersonalIdentityNumber> =
        gen {
            let! pin = validPin
            return (pin, pin)
        } |> Arb.fromGen
let twoEqualPinsConfig = { FsCheckConfig.defaultConfig with arbitrary = [typeof<TwoEqualPins>] }
let testPropIdentical : string -> (SwedishPersonalIdentityNumber * SwedishPersonalIdentityNumber -> unit) -> Test = 
    testPropertyWithConfig twoEqualPinsConfig