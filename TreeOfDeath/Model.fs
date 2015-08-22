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
          CutFinish     : Vertex
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
        type ObstacleShape = { Triangles : string list }

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

    /// Returns the vertices of all the triangles contained in an obstacle.
    let triangles obstacle = 
        let v1 = List.head obstacle.ObstaclePolygon // take the first vertex
        List.tail obstacle.ObstaclePolygon // and create a triangle for every subsequent pair of vertices
        |> List.pairwise
        |> List.map (fun (v2, v3) -> (v1, v2, v3))

module Target = 
    /// Creates a target with the specified centre and radius.
    let create centre radius =
        { TargetCentre = centre
          TargetRadius = radius }

<<<<<<< HEAD
    /// Gets the vertex representing the centre of the target.
    let centre target = target.TargetCentre

    /// Gets the raidus of the target.
=======
    /// Get the centre of a target.
    let centre target = target.TargetCentre

    /// Get the radius of a target.
>>>>>>> 7930b34636a44ac959c2beb448efd11a3529a4ab
    let radius target = target.TargetRadius

module Tree =
    /// Gets the starting vertex of a tree.
    let start tree = tree.TreeStart

    /// Gets the first node of a tree.
    let firstNode tree = tree.TreeFirstNode

<<<<<<< HEAD
module Cut =
    /// Creates a cut with the specified start and finish point and a flag indicating whether it is in
    /// progress or finished (i.e. when the player releases the mouse).
    let create start finish inProgress =
        { CutStart      = start
          CutFinish     = finish
          CutInProgress = inProgress }

    /// Gets the starting vertex of a cut.
    let start cut = cut.CutStart

    /// Gets the end vertex of a cut.
    let finish cut = cut.CutFinish

    /// Checks whether the cut is in progress or finished.
    let isInProgress cut = cut.CutInProgress
    
module TreeShape =
    /// Creates a tree shape containing a single leaf.
    let createLeaf shapeId = ShapeLeaf shapeId

    /// Creates a tree shape containing a shape leading up to a branch point and the shapes for
    /// the left and right trees after the branch.
    let createBranch shapeId leftTree rightTree = ShapeBranch (shapeId, leftTree, rightTree)

module ObstacleShape =
    /// Creates an obstacle shape consisting of the given list of triangle shapes.
    let create triangleIds = { Triangles = triangleIds }

module TargetShape =
    /// Creates a target shape with the specified shape identifier.
    let create shapeId = { TargetShape = shapeId }

module CutShape =
    /// Creates a cut shape with the specified shape identifier.
    let create shapeId = { CutShape = shapeId }
=======
    /// Get the parameters of a tree.
    let parameters tree = tree.TreeParameters
>>>>>>> 7930b34636a44ac959c2beb448efd11a3529a4ab
