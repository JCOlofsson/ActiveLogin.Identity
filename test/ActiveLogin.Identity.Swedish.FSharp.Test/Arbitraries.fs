module ActiveLogin.Identity.Swedish.FSharp.Test.Arbitraries
open Expecto
open Generators
open PinTestHelpers
open FsCheck
open ActiveLogin.Identity.Swedish.FSharp
open ActiveLogin.Identity.Swedish.FSharp.TestData
open FsCheck

type InvalidYearGen() =
    static member Year() : Arbitrary<SwedishPersonalIdentityNumberValues> = Arb.fromGen invalidYear
let invalidYearConfig = { FsCheckConfig.defaultConfig with arbitrary = [typeof<InvalidYearGen> ] }

type InvalidMonthGen() = 
    static member Month() : Arbitrary<SwedishPersonalIdentityNumberValues> = Arb.fromGen invalidMonth
let invalidMonthConfig = { FsCheckConfig.defaultConfig with arbitrary = [typeof<InvalidMonthGen> ] }

type InvalidDayGen() =
    static member Day() : Arbitrary<SwedishPersonalIdentityNumberValues> = Arb.fromGen invalidDay
let invalidDayConfig = { FsCheckConfig.defaultConfig with arbitrary = [typeof<InvalidDayGen> ] }

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
        random12Digit |> Arb.fromGen
let valid12DigitConfig = 
    { FsCheckConfig.defaultConfig with arbitrary = [ typeof<Valid12Digit> ] }

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

type Valid10Digit() =
    static member Valid10Digit : Arbitrary<string * SwedishPersonalIdentityNumberValues> =
        random10Digit |> Arb.fromGen
let valid10DigitConfig = { FsCheckConfig.defaultConfig with arbitrary = [ typeof<Valid10Digit> ] }

type Valid10DigitWithPlusDelimiter() =
    static member ValidWithPlusDelimiter : Arbitrary<string * SwedishPersonalIdentityNumberValues> =
        gen {
            return random10DigitWithPlusDelimiter |> Seq.head
        } |> Arb.fromGen
let valid10DigitWithPlusConfig = 
    { FsCheckConfig.defaultConfig with arbitrary = [ typeof<Valid10DigitWithPlusDelimiter> ] }

type Valid10DigitWithHyphenDelimiter() =
    static member ValidWithHyphenDelimiter : Arbitrary<string * SwedishPersonalIdentityNumberValues> =
        gen {
            return random10DigitWithHyphenDelimiter |> Seq.head
        } |> Arb.fromGen
let valid10DigitWithHyphenConfig = 
    { FsCheckConfig.defaultConfig with arbitrary = [ typeof<Valid10DigitWithHyphenDelimiter> ] }

type Valid12DigitWithLeadingAndTrailingCharacters() =
    static member Valid12DigitWithLeadingAndTrailingCharacters : Arbitrary<string * SwedishPersonalIdentityNumberValues> =
        gen {
            let! leading = printableAscii
            let! trailing = printableAscii
            let! (str, values) = random12Digit
            return (sprintf "%s%s%s" leading str trailing, values)
        } |> Arb.fromGen
let valid12DigitWithLeadingAndTrailingCharactersConfig =
    { FsCheckConfig.defaultConfig with arbitrary = [ typeof<Valid12DigitWithLeadingAndTrailingCharacters> ] }

type Valid10DigitStringWithAnyDelimiterExceptPlus() =
    static member Valid10DigitStringWithAnyDelimiterExceptPlus : Arbitrary<string * SwedishPersonalIdentityNumberValues> =
        gen {
            let (str, expected) = random10DigitWithHyphenDelimiter |> Seq.head
            let! delimiter = singlePrintableAsciiString
            let result = 
                match delimiter with
                | "+" -> str.[ 0..5 ] + str.[ 7..10 ] 
                | _ -> str.[ 0..5 ] + delimiter + str.[ 7..10 ] 
            return (result, expected)
        } |> Arb.fromGen
let valid10DigitStringWithAnyDelimiterExceptPlusConfig = { FsCheckConfig.defaultConfig with arbitrary = [ typeof<Valid10DigitStringWithAnyDelimiterExceptPlus> ] }

type Valid12DigitStringMixedWithCharacters =
    static member Valid12DigitStringMixedWithCharacters : Arbitrary<string * SwedishPersonalIdentityNumberValues> =
        gen {
            let! cs1 = printableAscii
            let! cs2 = printableAscii
            let! cs3 = printableAscii
            let! cs4 = printableAscii
            let! cs5 = printableAscii
            let! cs6 = printableAscii
            let! cs7 = printableAscii
            let! cs8 = printableAscii
            let! cs9 = printableAscii
            let! cs10 = printableAscii
            let! cs11 = printableAscii
            let! cs12 = printableAscii
            let! cs13 = printableAscii
            let! (str, expected) = random12Digit
            return (cs1 + str.[0..0] + cs2 + str.[1..1] + cs3 + str.[2..2] + cs4 + str.[3..3] + cs5 + str.[4..4] + cs6 + str.[5..5] + cs7 + str.[6..6] + cs8 + str.[7..7] + cs9 + str.[8..8] + cs10 + str.[9..9] + cs11 + str.[10..10] + cs12 + str.[11..11] + cs13, expected)
        } |> Arb.fromGen
let valid12DigitStringMixedWithCharactersConfig = { FsCheckConfig.defaultConfig with arbitrary = [ typeof<Valid12DigitStringMixedWithCharacters> ] }

type Valid10DigitStringMixedWithCharacters =
    static member Valid12DigitStringMixedWithCharacters : Arbitrary<string * SwedishPersonalIdentityNumberValues> =
        gen {
            let! cs1 = printableAsciiExceptPlus
            let! cs2 = printableAsciiExceptPlus
            let! cs3 = printableAsciiExceptPlus
            let! cs4 = printableAsciiExceptPlus
            let! cs5 = printableAsciiExceptPlus
            let! cs6 = printableAsciiExceptPlus
            let! cs7 = printableAsciiExceptPlus
            let! cs8 = printableAsciiExceptPlus
            let! cs9 = printableAsciiExceptPlus
            let! cs10 = printableAsciiExceptPlus
            let! cs11 = printableAsciiExceptPlus
            let! cs12 = printableAsciiExceptPlus
            let! (str, expected) = random10Digit
            return (cs1 + str.[0..0] + cs2 + str.[1..1] + cs3 + str.[2..2] + cs4 + str.[3..3] + cs5 + str.[4..4] + cs6 + str.[5..5] + cs7 + str.[6..6] + cs8 + str.[7..7] + cs9 + str.[8..8] + cs10 + str.[9..9] + cs11 + str.[10..10] + cs12, expected)
        } |> Arb.fromGen
let valid10DigitStringMixedWithCharactersConfig = { FsCheckConfig.defaultConfig with arbitrary = [ typeof<Valid10DigitStringMixedWithCharacters> ] }