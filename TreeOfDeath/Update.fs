namespace TreeOfDeath

open Geometry

module Update =
    module private Quantities =
        /// The maximum fraction of the growth rate that each branch length may vary by.
        let growthVariation = 1.0 / 4.0

        /// The approximate fraction of the distance between the start and end points that the tree
        /// will grow by each step.
        let growthDistanceFraction = 1.0 / 40.0

        /// The maximum angle variation that each branch may grow towards.
        let angleVariation = System.Math.PI / 4.0 * 1.0<rad>

        /// The probability that a leaf will create a new branch at each step.
        let branchProbability = 1.0 / 25.0

        /// The approximate distance in from the top left that the tree will begin at.
        let startDistance = 50.0

        /// The variation in the start distance.
        let startVariation = 1.0 / 2.0

    /// Given a length and variation, generate a new distance to grow.
    let private varyParameter (parameter : float<'T>) (variation : float<'T>) =
        parameter + Random.fraction () * variation

    /// Choose a location to start the tree at.  Always picks somewhere close to the point (0, 0).
    let private chooseStartLocation () =
        let x = int <| varyParameter Quantities.startDistance Quantities.startVariation
        let y = int <| varyParameter Quantities.startDistance Quantities.startVariation
        Vertex.create x y

    /// Make a leaf branched out from a staring location, using the passed parameters.
    let private makeLeaf start parameters =
        let distance = varyParameter parameters.GrowthRate parameters.GrowthVariation
        let angle    = float <| varyParameter parameters.BranchAngle parameters.AngleVariation
        { X = int <| distance * cos angle
          Y = int <| distance * sin angle }
        |> Leaf

    /// Create an initial tree given a start vertex, and a direction and distance to grow in.
    let private createTree target =
        let start = chooseStartLocation ()
        let growthRate =
            distanceBetween start target.TargetCentre
            |> float
            |> (*) Quantities.growthDistanceFraction
        let angle = angleBetween start target.TargetCentre
        let parameters =
            { GrowthRate        = growthRate
              GrowthVariation   = growthRate * Quantities.growthVariation
              BranchAngle       = angle
              AngleVariation    = Quantities.angleVariation
              BranchProbability = Quantities.branchProbability }
        { TreeStart      = start
          TreeFirstNode  = makeLeaf start parameters
          TreeParameters = parameters }