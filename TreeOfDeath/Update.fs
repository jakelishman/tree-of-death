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

        /// The angle added to the left branch of the tree.
        let angleLeft = System.Math.PI / 18.0 * 1.0<rad>
        /// The angle added to the right branch of the tree.
        let angleRight = System.Math.PI / 18.0 * -1.0<rad>
        /// The maximum angle variation that each branch may grow towards.
        let angleVariation = System.Math.PI / 4.0 * 1.0<rad>

        /// The probability that a leaf will create a new branch at each step.
        let branchProbability = 1.0 / 5.0

        /// The approximate distance in from the top left that the tree will begin at.
        let startDistance = 50.0
        /// The variation in the start distance.
        let startVariation = 1.0 / 2.0

        /// The approximate distance in from the bottom right that the target will be at.
        let targetDistance = 50.0
        /// The variation in the target location.
        let targetVariation = 1.0 / 2.0

        /// The approximate radius of the target.
        let targetRadius = 25.0
        /// The variation in the radius of the target.
        let targetRadiusVariation = 1.0 / 2.0

        /// X offset of the path to make the upper wall.
        let wallXOffset = 300
        /// Y offset of the path to make the lower wall.
        let wallYOffset = 300

        /// Number of sections of path to have for the wall obstacles.
        let pathSegments = 4

        /// Maximum allowed variation in the angle the path takes from optimum.
        let pathAngleVariation = 1.0<rad> * System.Math.PI / 12.0

    /// Given a length and variation, generate a new distance to grow.
    let private varyParameter (parameter : float<'T>) (variation : float<'T>) =
        parameter + Random.fraction () * variation

    module Init =
        /// Make a leaf branched out from a staring location, using the passed parameters.
        let private makeStartLeaf start parameters =
            // let distance = varyParameter parameters.GrowthRate parameters.GrowthVariation
            // let angle    = float <| varyParameter parameters.BranchAngle parameters.AngleVariation
            Leaf <| Vertex.create (int <| 100) (int <| 100)

        /// Choose a location to start the tree at.  Always picks somewhere close to the point (0, 0).
        let private chooseStartLocation () =
            let x = 75 // int <| varyParameter Quantities.startDistance Quantities.startVariation
            let y = 75 // int <| varyParameter Quantities.startDistance Quantities.startVariation
            Vertex.create x y

        /// Create an initial tree given a start vertex, and a direction and distance to grow in.
        let private tree target =
            let start = chooseStartLocation ()
            let growthRate = 3.0
                // distanceBetween start target.TargetCentre
                // |> float
                // |> (*) Quantities.growthDistanceFraction
            let angle = angleBetween start target.TargetCentre
            let parameters =
                { GrowthRate        = growthRate
                  GrowthVariation   = growthRate * Quantities.growthVariation
                  BranchAngle       = angle
                  AngleVariation    = Quantities.angleVariation
                  BranchProbability = Quantities.branchProbability }
            let node = makeStartLeaf start parameters
            Tree.create start node parameters

        /// Choose a location for the target, probably close to the bottom right of the screen.
        let private chooseTargetLocation bottomRightCorner =
            let x = 900
            let y = 550
            Vertex.create x y

        /// Choose the radius of the target object.
        let private chooseTargetRadius () =
            int <| varyParameter Quantities.targetRadius Quantities.targetRadiusVariation

        /// Create a new target object close to the bottom right of the window.
        let private target bottomRightCorner =
            let location = chooseTargetLocation bottomRightCorner
            let radius = chooseTargetRadius ()
            Target.create location radius

        /// Cons an item onto the front of a list.
        let private cons item list = item :: list

        /// Get the coordinates of the point projected onto the left edge.
        let private leftEdge bottomRightCorner =
            { bottomRightCorner with X = 0 }

        /// Get the coordinates of the point projected onto the top edge.
        let private topEdge bottomRightCorner =
            { bottomRightCorner with Y = 0 }

        /// Create the lower limiting wall of the level.
        let private obstacleLowerLeft =
            [ Vertex.create 0 768
              Vertex.create 0 100
              Vertex.create 100 350
              Vertex.create 50 768 ]
            |> Obstacle.create

        let private obstacleTopRight =
            [ Vertex.create 1024 0
              Vertex.create 1024 150
              Vertex.create 800 150
              Vertex.create 500 300
              Vertex.create 450 150
              Vertex.create 450 0 ]
            |> Obstacle.create

        let private obstacleBottom =
            [ Vertex.create 500 768 
              Vertex.create 650 680
              Vertex.create 800 768 ]
            |> Obstacle.create

        /// Create a vertex list of an approximate path for the tree.
        let makePath start target =
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
        let private obstacles bottomRightCorner start target =
            let path = makePath start (Target.centre target)
            [ obstacleLowerLeft ; obstacleTopRight ; obstacleBottom ]

        /// Initialise a new scene.
        let scene bottomRightCorner =
            let target    = target bottomRightCorner
            let tree      = tree target
            let obstacles = obstacles bottomRightCorner (Tree.start tree) target
            Scene.create tree obstacles target None

    module Update =
        /// Create a (left, right) pair of leaf nodes from the current point.
        let private branchLeaf start parameters =
            let leftDistance  = varyParameter parameters.GrowthRate parameters.GrowthVariation
            let rightDistance = varyParameter parameters.GrowthRate parameters.GrowthVariation
            let angle = varyParameter parameters.BranchAngle parameters.AngleVariation
            let leftAngle  = float <| angle + Quantities.angleLeft
            let rightAngle = float <| angle + Quantities.angleRight
            let left  = polarToRectangular start leftDistance leftAngle
            let right = polarToRectangular start rightDistance rightAngle
            (Leaf left, Leaf right)

        /// Grow the tree through one step.
        let private grow tree =
            let parameters = Tree.parameters tree
            let rec loop branchProbability root  = function
                | Leaf vertex ->
                    if Random.nonNegativeFraction () < branchProbability then
                        let (left, right) = branchLeaf vertex (Tree.parameters tree)
                        Branch (vertex, left, right)
                    else
                        let (dx, dy) = (float <| Vertex.x vertex - Vertex.x root, float <| Vertex.y vertex - Vertex.y root)
                        let length = sqrt(dx * dx + dy * dy)
                        let newLength = length + parameters.GrowthRate
                        let (newdx, newdy) = (int <| (dx * (newLength / length)), int <| (dy * (newLength / length)))
                        Leaf (Vertex.create (Vertex.x root + newdx) (Vertex.y root + newdy))
                | Branch (barnchPoint, left, right) -> 
                    let newBranchProbability = branchProbability * (sqrt parameters.BranchProbability)
                    Branch (barnchPoint, loop newBranchProbability barnchPoint left, loop newBranchProbability barnchPoint  right)
            let newNode = loop (parameters.BranchProbability) (Tree.start tree) (Tree.firstNode tree)
            { tree with TreeFirstNode = newNode }

        /// Check if the tree has reached the target, returning a boolean result.
        let private reachedTarget tree target =
            let rec loop = function
                | Leaf location ->
                    Geometry.distanceBetween location (Target.centre target) < (float <| Target.radius target)
                | Branch (_, left, right) -> loop left || loop right
            loop (Tree.firstNode tree)

        let private between0And1 a = (0.0 <= a && a <= 1.0)

        /// Check a float is between 0 and 1.
        let private intersect0And1 a b =
            between0And1 a || between0And1 a
            || (a < 0.0 && b > 1.0)
            || (b < 0.0 && a > 1.0)

        /// Check if two lines intersect.
        let private intersect (startA, endA) (startB, endB) =
            let branch = Geometry.lineDifference endA startA
            let cut = Geometry.lineDifference endB startB
            let join = Geometry.lineDifference startB startA
            if (Geometry.cross branch cut) = 0 && (Geometry.cross join branch) <> 0 then
                None
            elif (Geometry.cross branch cut) <> 0 then
                let t = float (Geometry.cross join cut) / float (Geometry.cross branch cut)
                let u = float (Geometry.cross join branch) / float (Geometry.cross branch cut)
                if between0And1 t && between0And1 u then
                    Some (Vertex.create (Vertex.x startA + (int t) * Line.x branch) (Vertex.y startA + (int t) * Line.y branch))
                else None
            else None

        let private doesIntersect pointA pointB =
            match intersect pointA pointB with
            | Some s -> true
            | None -> false

        /// Check if a branch intersects with an obstacle.
        let private obstacleIntersect branch obstacle =
            let folder state line = state || doesIntersect branch line
            Obstacle.vertices obstacle
            |> List.pairwise
            |> List.fold folder false

        /// Check if the tree has collided with an object.
        let private collidedWith tree obstacle =
            let rec loop prev = function
                | Leaf cur -> obstacleIntersect (prev, cur) obstacle
                | Branch (cur, left, right) ->
                    loop cur left || loop cur right
            loop (Vertex.create 0 0) (Tree.firstNode tree)

        /// Check if a collision has occurred.
        let private collisionExists tree obstacles =
            let folder state obstacle =
                obstacle
                |> collidedWith tree
                |> (||) state
            List.fold folder false obstacles

        /// Apply a cut to a tree.
        let prune cut tree =
            let rec loop prev node =
                match node with
                | Leaf cur ->
                    let intersection = intersect (prev, cur) (Cut.start cut, Cut.finish cut)
                    match intersection with
                    | Some s -> Leaf s
                    | None -> Leaf cur
                | Branch (cur, left, right) ->
                    let intersection = intersect (prev, cur) (Cut.start cut, Cut.finish cut)
                    match intersection with
                    | Some s -> Leaf s
                    | None ->
                        let node = Branch (cur, left, right)
                        Branch (cur, loop cur left, loop cur right)
            let node = loop (Tree.start tree) (Tree.firstNode tree)
            { tree with TreeFirstNode = node }

        /// Update the state of the game by one tick.
        let scene scene cut =
            let (cutter, cut') =
                match cut with
                | Some c when not <| Cut.isInProgress c -> (prune c, None)
                | chop                                  -> ((fun x -> x), chop)
            let tree =
                scene
                |> Scene.tree
                |> grow
                |> cutter
            if collisionExists tree (Scene.obstacles scene) then GameLost
            elif reachedTarget tree (Scene.target scene) then GameWon
            else
                scene
                |> Scene.withTree tree
                |> Scene.withCut cut'
                |> GameInProgress
