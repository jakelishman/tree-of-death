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

        /// X offset of the path to make the upper wall.
        let wallXOffset = 600
        /// Y offset of the path to make the lower wall.
        let wallYOffset = 600

        /// Number of sections of path to have for the wall obstacles.
        let pathSegments = 4

        /// Maximum allowed variation in the angle the path takes from optimum.
        let pathAngleVariation = 1.0<rad> * System.Math.PI / 12.0

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

        /// Cons an item onto the front of a list.
        let private cons item list = item :: list

        /// Get the coordinates of the point projected onto the left edge.
        let private leftEdge windowSize =
            { windowSize with X = 0 }

        /// Get the coordinates of the point projected onto the top edge.
        let private topEdge windowSize =
            { windowSize with Y = 0 }

        /// Create the lower limiting wall of the level.
        let private obstacleLowerWall windowSize path =
            path
            |> List.map (fun vert -> Vertex.create (Vertex.x vert) (Vertex.y vert + Quantities.wallYOffset))
            |> cons (leftEdge <| List.head path)
            |> cons (leftEdge windowSize)
            |> List.rev
            |> cons windowSize
            |> Obstacle.create

        /// Create the upper limiting wall of the level.
        let private obstacleUpperWall windowSize path =
            path
            |> List.map (fun vert -> Vertex.create (Vertex.x vert + Quantities.wallXOffset) (Vertex.y vert))
            |> cons (topEdge <| List.head path)
            |> cons (topEdge windowSize)
            |> List.rev
            |> cons windowSize
            |> Obstacle.create

        /// Create a vertex list of an approximate path for the tree.
        let private makePath start target =
            let distance = (Geometry.distanceBetween start target) / float Quantities.pathSegments
            let rec loop acc last = function
                | 0 -> List.rev acc
                | 1 -> loop (target :: acc) target 0
                | i ->
                    let angle = float <| varyParameter (Geometry.angleBetween last target) Quantities.pathAngleVariation
                    let x = int <| distance * cos angle
                    let y = int <| distance * sin angle
                    let nextNode = Vertex.create x y
                    loop (nextNode :: acc) nextNode (i - 1)
            loop [start] start Quantities.pathSegments

        /// Create the obstacles for the level.
        let private obstacles windowSize start target =
            let path = makePath start (Target.centre target)
            [ obstacleLowerWall windowSize path ; obstacleUpperWall windowSize path ]

        /// Initialise a new scene.
        let scene windowSize =
            let target    = target windowSize
            let tree      = tree target
            let obstacles = obstacles windowSize (Tree.start tree) target
            Scene.create tree obstacles target