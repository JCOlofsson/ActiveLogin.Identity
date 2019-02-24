module ActiveLogin.Identity.Swedish.FSharp.Test.Generators
open ActiveLogin.Identity.Swedish.FSharp
open Expecto
open FsCheck
open System
open ActiveLogin.Identity.Swedish.FSharp.TestData

let validYear = Gen.choose(DateTime.MinValue.Year, DateTime.MaxValue.Year)
let invalidYear =
    Gen.oneof 
        [ (Gen.choose(Int32.MinValue, DateTime.MinValue.Year - 1)) 
          (Gen.choose(DateTime.MaxValue.Year + 1, Int32.MaxValue)) ]

let validMonth = Gen.choose(1, 12)
let invalidMonth = 
    Gen.oneof 
        [ (Gen.choose(Int32.MinValue, 0))
          (Gen.choose(13, Int32.MaxValue)) ]
let validDay = gen {
    let! year = validYear
    let! month = validMonth
    let daysInMonth = DateTime.DaysInMonth(year, month)
    let! day = Gen.choose(1, daysInMonth)
    return (year, month, day)
}
let invalidDay = gen {
    let! year = validYear
    let! month = validMonth
    let daysInMonth = DateTime.DaysInMonth(year, month)
    let! day = Gen.oneof [ Gen.choose(Int32.MinValue, 0)
                           Gen.choose(daysInMonth + 1, Int32.MaxValue) ]
    return (year, month, day)
}
let validBirthNumber = Gen.choose(1, 999)
let invalidBirthNumber = 
    Gen.oneof [ Gen.choose(Int32.MinValue, 0); Gen.choose(1000, Int32.MaxValue) ]

let validWithoutChecksum =
    gen {
        let! (year, month, day) = validDay
        let! birthNumber = validBirthNumber
        return (year, month, day, birthNumber)
    }

type InvalidYearGen() =
    static member Year() : Arbitrary<int> = Arb.fromGen invalidYear
let invalidYearConfig = { FsCheckConfig.defaultConfig with arbitrary = [typeof<InvalidYearGen>]}
let testPropInvalidYear : string -> (int -> unit) -> Test = testPropertyWithConfig invalidYearConfig

type InvalidMonthGen() = static member Month() : Arbitrary<int> = Arb.fromGen invalidMonth
let invalidMonthConfig = { FsCheckConfig.defaultConfig with arbitrary = [typeof<InvalidMonthGen>]}
let testPropInvalidMonth : string -> (int -> unit) -> Test = testPropertyWithConfig invalidMonthConfig

type InvalidDayGen() =
    static member Day() : Arbitrary<int * int * int> = Arb.fromGen invalidDay
let invalidDayConfig = { FsCheckConfig.defaultConfig with arbitrary = [typeof<InvalidDayGen>]}
let testPropInvalidDay : string -> (int * int * int -> unit) -> Test = testPropertyWithConfig invalidDayConfig

type ValidDayGen() =
    static member Day() : Arbitrary<int * int * int> = Arb.fromGen validDay
let validDayConfig = { FsCheckConfig.defaultConfig with arbitrary = [typeof<ValidDayGen>]}
let testPropValidDay : string -> (int * int * int -> unit) -> Test = testPropertyWithConfig validDayConfig

type InvalidBirthNumberGen() =
    static member BirthNumber() : Arbitrary<int> = Arb.fromGen invalidBirthNumber
let invalidBirthNumberConfig = { FsCheckConfig.defaultConfig with arbitrary = [typeof<InvalidBirthNumberGen>]}
let testPropInvalidBirthNumber : string -> (int -> unit) -> Test = testPropertyWithConfig invalidBirthNumberConfig

type ValidWithoutChecksum() =
    static member WithoutChecksum() : Arbitrary<int * int * int * int> = Arb.fromGen validWithoutChecksum
let validWithoutChecksumConfig = { FsCheckConfig.defaultConfig with arbitrary = [typeof<ValidWithoutChecksum>]}
let testPropValidWithoutChecksum : string -> (int * int * int * int -> unit) -> Test = testPropertyWithConfig validWithoutChecksumConfig

let pin12DigitString =
    gen { 
        let! index = Gen.choose(0, (Array.length SwedishPersonalIdentityNumberTestData.raw12DigitStrings - 1))
        return SwedishPersonalIdentityNumberTestData.raw12DigitStrings.[index]
    }
type Pin12DigitString() =
    static member Pin12DigitString() : Arbitrary<string> =
        pin12DigitString |> Arb.fromGen
let pin12DigitStringConfig = 
    { FsCheckConfig.defaultConfig with arbitrary = [ typeof<Pin12DigitString> ] }
let testProp12DigitString : string -> (string -> unit) -> Test = testPropertyWithConfig pin12DigitStringConfig

let validPin =
    gen {
        let! str = pin12DigitString 
        return PinTestHelpers.quickParse str
    }

type ValidPin() =
    static member ValidPin() : Arbitrary<SwedishPersonalIdentityNumber> =
        validPin |> Arb.fromGen
let validPinConfig = { FsCheckConfig.defaultConfig with arbitrary = [typeof<ValidPin>]}
let testPropValidPin : string -> (SwedishPersonalIdentityNumber -> unit) -> Test = testPropertyWithConfig validPinConfig
