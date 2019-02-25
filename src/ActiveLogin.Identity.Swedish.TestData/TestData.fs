namespace ActiveLogin.Identity.Swedish.FSharp.TestData
open ActiveLogin.Identity.Swedish.FSharp
open ActiveLogin.Identity.Swedish.TestData.AllPins
open System
open System.Threading


module SwedishPersonalIdentityNumberTestData =
    let private rng = 
        // this thread-safe implementation is required to handle running lots of invocations of getRandom in parallel
        let seedGenerator = Random()
        let localGenerator = new ThreadLocal<Random>(fun _ ->
            lock seedGenerator (fun _ ->
                let seed = seedGenerator.Next()
                Random()))
        fun (min, max) -> localGenerator.Value.Next(min, max)
    let private random _ = rng(Int32.MinValue, Int32.MaxValue)
    let internal shuffledPins() = allPins |> Array.sortBy random
    let raw12DigitStrings = 
        allPins
        |> Array.map (fun (year, month, day, birthNumber, checksum) -> sprintf "%04i%02i%02i%03i%i" year month day birthNumber checksum)

    let internal create (year, month, day, birthNumber, checksum) =
        let values =
            { Year = year
              Month = month
              Day = day
              BirthNumber = birthNumber
              Checksum = checksum }
        match SwedishPersonalIdentityNumber.create values with
        | Ok p -> p
        | Error _ -> failwith "broken test data" 

    let allPinsByDateDesc() = seq { for pin in allPins do yield create pin }
    let allPinsShuffled() = seq { for pin in shuffledPins() do yield create pin }
    let getRandom() = 
        let index = rng(0, Array.length allPins - 1)
        allPins.[index]
        |> create
    let getRandomWithCount(count) = allPinsShuffled() |> Seq.take count

    let internal isTestNumberTuple (year, month, day, birthNumber, checksum) =
        allPins |> Array.contains (year, month, day, birthNumber, checksum)

    let isTestNumber pin =
        let asTuple 
            { SwedishPersonalIdentityNumber.Year = year
              Month = month
              Day = day
              BirthNumber = birthNumber
              Checksum = checksum } =
            (Year.value year, Month.value month, Day.value day, BirthNumber.value birthNumber, Checksum.value checksum)
        pin |> asTuple |> isTestNumberTuple

module SwedishPersonalIdentityNumber =
    let isTestNumber = SwedishPersonalIdentityNumberTestData.isTestNumber

