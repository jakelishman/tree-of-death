namespace TreeOfDeath

[<AutoOpen>]
module Model =
    /// One vertex on some polygon, or a point in space.
    type Vertex =
        { X : int
          Y : int }

    /// Part of the bounding wall of the game board.
    type Obstacle = Vertex list

    type Node =
        | Branch of location : Vertex * left : Node * right : Node
        | Leaf   of location : Vertex

    type Tree =
       { StartLocation : Vertex
         FirstNode     : Node }

    type Target =
        { TargetLocation : Vertex
          TargetRadius   : int }

    type Cut =
        { CutStart : Vertex 
          CutEnd   : Vertex }

    type Scene =
        { Tree      : Tree
          Obstacles : Obstacle list
          Target    : Target }

    type Api = {
        Grow           : Tree -> Tree
        Render         : Tree -> GraphicsWindow -> unit
        CheckCollision : Tree -> Obstacle -> bool
        ReachedTarget  : Tree -> Target -> bool
        UpdateScene    : Scene -> Cut option -> Scene }