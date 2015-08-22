namespace TreeOfDeath

open Library

[<AutoOpen>]
module Model =
    /// A point in 2 dimensions.
    type Vertex = { X : int ; Y : int }

    /// Boundaries of a polygon obstacle.
    type Obstacle = { Polygon : Vertex list }

    /// A tree node.
    type Node =
        | Branch of location : Vertex * left : Node * right : Node
        | Leaf   of location : Vertex

    /// A tree.
    type Tree =
       { // TODO: Add growth parameters (rate, branch angle coefficient, branching probability, etc.)
         StartLocation : Vertex
         FirstNode     : Node }

    /// The target which the tree needs to reach
    type Target =
        { Centre : Vertex
          Radius : int }

    /// Defines a cut performed by the player which prunes the tree.
    type Cut =
        { Start : Vertex 
          End   : Vertex }
    
    /// Defines the game scene.
    type Scene =
        { Tree      : Tree
          Obstacles : Obstacle list
          Target    : Target }

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
