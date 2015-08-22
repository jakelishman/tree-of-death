namespace TreeOfDeath

open Geometry

[<AutoOpen>]
module Logic =
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

        /// The approximate distance in from the bottom right that the target will be at.
        let targetDistance = 50.0
        /// The variation in the target location.
        let targetVariation = 1.0 / 2.0

        /// The approximate radius of the target.
        let targetRadius = 10.0
        /// The variation in the radius of the target.
        let targetRadiusVariation = 1.0 / 2.0

    /// Given a length and variation, generate a new distance to grow.
    let private varyParameter (parameter : float<'T>) (variation : float<'T>) =
        parameter + Random.fraction () * variation

    /// Make a leaf branched out from a staring location, using the passed parameters.
    let private makeLeaf start parameters =
        let distance = varyParameter parameters.GrowthRate parameters.GrowthVariation
        let angle    = float <| varyParameter parameters.BranchAngle parameters.AngleVariation
        Leaf <| Vertex.create (int <| distance * cos angle) (int <| distance * sin angle)

    module Init =
        /// Choose a location to start the tree at.  Always picks somewhere close to the point (0, 0).
        let private chooseStartLocation () =
            let x = int <| varyParameter Quantities.startDistance Quantities.startVariation
            let y = int <| varyParameter Quantities.startDistance Quantities.startVariation
            Vertex.create x y

        /// Create an initial tree given a start vertex, and a direction and distance to grow in.
        let private tree target =
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
            let node = makeLeaf start parameters
            Tree.create start node parameters

        /// Choose a location for the target, probably close to the bottom right of the screen.
        let private chooseTargetLocation windowSize =
            let x =
                varyParameter Quantities.targetDistance Quantities.targetVariation
                |> int
                |> (-) windowSize.X
            let y =
                varyParameter Quantities.targetDistance Quantities.targetVariation
                |> int
                |> (-) windowSize.Y
            Vertex.create x y

        /// Choose the radius of the target object.
        let private chooseTargetRadius () =
            int <| varyParameter Quantities.targetRadius Quantities.targetRadiusVariation

        /// Create a new target object close to the bottom right of the window.
        let private target windowSize =
            let location = chooseTargetLocation windowSize
            let radius = chooseTargetRadius ()
            Target.create location radius

        /// Initialise a new scene.
        let scene windowSize =
            let target = target windowSize
            let tree   = tree target
            Scene.create tree List.empty target