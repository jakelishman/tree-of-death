namespace TreeOfDeath

open Library

[<AutoOpen>]
module Model =
    /// A point in 2 dimensions.
    type Vertex = { X : int ; Y : int }

    /// Boundaries of a polygon obstacle.
    type Obstacle = { ObstaclePolygon : Vertex list }

    /// A tree node.
    type Node =
        | Branch of location : Vertex * left : Node * right : Node
        | Leaf   of location : Vertex

    /// A tree.
    type Tree =
       { // TODO: Add growth parameters (rate, branch angle coefficient, branching probability, etc.)
         TreeStart     : Vertex
         TreeFirstNode : Node }

    /// The target which the tree needs to reach
    type Target =
        { TargetCentre : Vertex
          TargetRadius : int }

    /// Defines a cut performed by the player which prunes the tree.
    type Cut =
        { CutStart : Vertex 
          CutEnd   : Vertex }
    
    /// Defines the game scene.
    type Scene =
        { SceneTree      : Tree
          SceneObstacles : Obstacle list
          SceneTarget    : Target }

    /// API functiosn for advancing updating the scene (Jake).
    type LogicApi =
        { Grow           : Tree -> Tree
          CheckCollision : Tree -> Obstacle -> bool
          ReachedTarget  : Tree -> Target -> bool
          UpdateScene    : Scene -> Cut option -> Scene }

    /// API functions for rendering the scene (Anton).
    type RenderApi =
        { RenderTree     : Tree -> GraphicsWindow -> unit
          RenderObstacle : Obstacle -> GraphicsWindow -> unit
          RenderTarget   : Target -> GraphicsWindow -> unit
          RenderScene    : Scene -> GraphicsWindow -> unit }

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

module Tree =
    /// Gets the starting vertex of a tree.
    let start tree = tree.TreeStart

    /// Gets the first node of a tree.
    let firstNode tree = tree.TreeFirstNode
