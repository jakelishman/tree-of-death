namespace TreeOfDeath

open Library

[<AutoOpen>]
module Model =
    /// Radian unit of measure.
    [<Measure>] type rad

    /// A point in 2 dimensions.
    type Vertex = { X : int ; Y : int }

    /// Boundaries of a polygon obstacle.
    type Obstacle = { ObstaclePolygon : Vertex list }

    /// A tree node.
    type Node =
        | Leaf   of location : Vertex
        | Branch of location : Vertex * left : Node * right : Node

    /// The parameters of the tree.
    type TreeParameters =
        { GrowthRate        : float
          GrowthVariation   : float
          BranchAngle       : float<rad>
          AngleVariation    : float<rad>
          BranchProbability : float }

    /// A tree.
    type Tree =
        { TreeStart      : Vertex
          TreeFirstNode  : Node
          TreeParameters : TreeParameters }

    /// The target which the tree needs to reach
    type Target =
        { TargetCentre : Vertex
          TargetRadius : int }

    /// Defines a cut performed by the player which prunes the tree.
    type Cut =
        { CutStart      : Vertex 
          CutEnd        : Vertex
          CutInProgress : bool }
    
    /// Defines the game scene.
    type Scene =
        { SceneTree      : Tree
          SceneObstacles : Obstacle list
          SceneTarget    : Target }

    [<AutoOpen>]
    /// Defines types which represent shapes added to the scene for later reference.
    module Shapes = 
        /// Contains all the shapes identifiers in the scene which belong to a tree.
        type TreeShape =
            | ShapeLeaf   of shapeId : string
            | ShapeBranch of shapeId : string * left : TreeShape * right : TreeShape

        /// Contains all the shape identifies in the scene which belong to an obstacle.
        type ObstacleShape = { ObstacleParts : string list }

        /// Contains the shape identifier for the target in the scene.
        type TargetShape = { TargetShape : string }

        /// Contains the shape identifier for a cut in the scene.
        type CutShape = { CutShape : string }

        /// Contains all the shapes in the scene.
        type SceneShape =
            { SceneTreeShape     : TreeShape
              SceneObstacleShape : ObstacleShape
              SceneTargetShape   : TargetShape
              SceneCutShape      : CutShape option }

    /// API functiosn for advancing updating the scene (Jake).
    type LogicApi =
        { Grow           : Tree -> Tree
          CheckCollision : Tree -> Obstacle -> bool
          ReachedTarget  : Tree -> Target -> bool
          UpdateScene    : Scene -> Cut option -> Scene }

    /// API functions for rendering the scene (Anton).
    type RenderApi =
        { RenderTree      : Tree -> TreeShape
          RenderObstacle  : Obstacle -> ObstacleShape
          RenderTarget    : Target -> TargetShape
          RenderCut       : Cut -> CutShape
          InitialiseScene : Scene -> SceneShape
          UpdateScene     : Scene -> SceneShape -> SceneShape }

module Vertex =
    /// Creates a vertex with the specified x and y coordinates.
    let create x y = { X = x ; Y = y }

    /// Gets the x coordinate of a vertex.
    let x vertex = vertex.X

    /// Gets the y coordinate of a vertex.
    let y vertex = vertex.Y

module Obstacle = 
    /// Creates an obstacle defined by a polygon consisting of the specified vertex list.
    let create vertices =
        if List.length vertices < 3 then failwith "Cannot create an obstacle with fewer than 3 vertices."
        { ObstaclePolygon = vertices }

module Target = 
    /// Creates a target with the specified centre and radius.
    let create centre radius =
        { TargetCentre = centre
          TargetRadius = radius }

    /// Get the centre of a target.
    let centre target = target.TargetCentre

    /// Get the radius of a target.
    let radius target = target.TargetRadius

module Tree =
    /// Gets the starting vertex of a tree.
    let start tree = tree.TreeStart

    /// Gets the first node of a tree.
    let firstNode tree = tree.TreeFirstNode

    /// Get the parameters of a tree.
    let parameters tree = tree.TreeParameters

    /// Create a new tree with the given quantities.
    let create start node parameters =
        { TreeStart      = start
          TreeFirstNode  = node
          TreeParameters = parameters }

module Scene =
    /// Get the tree from a scene.
    let tree scene = scene.SceneTree

    /// Get the target from a scene.
    let target scene = scene.SceneTarget

    /// Get the obstacle list from a scene.
    let obstacles scene = scene.SceneObstacles

    /// Create a scene with the given quantities.
    let create tree obstacles target =
        { SceneTree      = tree
          SceneObstacles = obstacles
          SceneTarget    = target }