﻿namespace TreeOfDeath

open Library

[<AutoOpen>]
module Render =
    /// Renders a tree node from the given starting position.
    let rec private renderNode start = function
        // if this is a leaf node, then add a like and create a leaf tree shape
        | Leaf finish -> 
            TreeShape.createLeaf 
            <| Shapes.AddLine (Vertex.x start, Vertex.y start, Vertex.x finish, Vertex.y finish)

        // if this is a branch node then create a branch shape containing the line from the start to
        // the branch point and shapes for left and right branches
        | Branch (branchPoint, leftBranch, rightBranch) -> 
            TreeShape.createBranch
            <| Shapes.AddLine (Vertex.x start, Vertex.y start, Vertex.x branchPoint, Vertex.y branchPoint)
            <| renderNode branchPoint leftBranch
            <| renderNode branchPoint rightBranch

    /// Renders a tree, returning the generated tree shape which contains identifiers for all the
    /// drawn shapes.
    let private renderTree tree =
        renderNode (Tree.start tree) (Tree.firstNode tree)
    
    /// Renders an obstacle, returning the generated obstacle shape which contains identifiers for
    /// all the drawn shapes. 
    let private renderObstacle obstacle =
        Obstacle.triangles obstacle
        |> List.map (fun (v1, v2, v3) -> Shapes.AddTriangle (float <| Vertex.x v1, float <| Vertex.y v1,
                                                             float <| Vertex.x v2, float <| Vertex.y v2,
                                                             float <| Vertex.x v3, float <| Vertex.y v3))
        |> ObstacleShape.create

    /// Renders a target, returning the generated obstacle shape which contains an identifier for the
    /// drawn shape.
    let private renderTarget target =
        let centre = Target.centre target
        let shape = Shapes.AddEllipse(Target.radius target, Target.radius target)
        Shapes.Move(shape, Vertex.x centre, Vertex.y centre)
        shape |> TargetShape.create

    /// Renders a cut, returning the generated cut shape which contains an identifier for the drawn shape.
    let private renderCut cut =
        if not <| Cut.isInProgress cut then failwith "Cannot draw a cut which has been performed."
        Shapes.AddLine(Vertex.x <| Cut.start  cut, Vertex.y <| Cut.start  cut,
                       Vertex.x <| Cut.finish cut, Vertex.y <| Cut.finish cut)
        |> CutShape.create