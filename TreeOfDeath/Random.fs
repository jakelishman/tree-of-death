namespace TreeOfDeath

open System

[<RequireQualifiedAccess>]
module Random =
    /// Random number class for use throughout the module.
    let private randomGenerator = System.Random()

    /// Get a random number between -1 and 1.
    let fraction () =
        (2.0 * float (randomGenerator.Next ()) / float Int32.MaxValue) - 1.0

    /// Get a random non-negative fraction between 0 and 1.
    let nonNegativeFraction () =
        float (randomGenerator.Next()) / float Int32.MaxValue

    /// Get a non-negative integer between 0 and Int32.MaxValue.
    let nonNegativeInteger () = randomGenerator.Next ()

    /// Get an integer between Int32.MinValue and Int32.MaxValue.
    let integer () = randomGenerator.Next (Int32.MinValue, Int32.MaxValue)

    /// Get an integer between the two specified values.
    let integerBetween a b = randomGenerator.Next (a, b)