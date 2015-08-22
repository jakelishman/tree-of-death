namespace TreeOfDeath

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

    /// Renders a tree in the graphics window, returning the generated tree shape which contains
    /// identifiers for all the drawn shapes.
    let private renderTree tree =
        renderNode (Tree.start tree) (Tree.firstNode tree)
    
    /// Removes the tree shape fromm the graphics window.
    let private removeTreeShape treeShape =
        TreeShape.childShapeIds treeShape 
        |> List.iter Shapes.Remove

    /// Renders an obstacle in the graphics window, returning the generated obstacle shape which contains
    /// identifiers for all the drawn shapes. 
    let private renderObstacle obstacle =
        Obstacle.triangles obstacle
        |> List.map (fun (v1, v2, v3) -> Shapes.AddTriangle (float <| Vertex.x v1, float <| Vertex.y v1,
                                                             float <| Vertex.x v2, float <| Vertex.y v2,
                                                             float <| Vertex.x v3, float <| Vertex.y v3))
        |> ObstacleShape.create

    /// Removes the obstacle shape from the graphics window.
    let private removeObstacleShape obstacleShape =
        ObstacleShape.childShapeIds obstacleShape
        |> List.iter Shapes.Remove

    /// Renders a target in the graphics window, returning the generated obstacle shape which contains an
    /// identifier for the drawn shape.
    let private renderTarget target =
        let centre = Target.centre target
        let shape = Shapes.AddEllipse(2 * Target.radius target, 2 * Target.radius target)
        Shapes.Move(shape, Vertex.x centre - Target.radius target, Vertex.y centre - Target.radius target)
        shape |> TargetShape.create

    /// Removes a target shape from the graphics window.
    let private removeTargetShape targetShape =
        TargetShape.shapeId targetShape |> Shapes.Remove

    /// Renders a cut in the graphics window, returning the generated cut shape which contains an identifier
    /// for the drawn shape.
    let private renderCut = function 
        | Some cut -> 
            Shapes.AddLine(Vertex.x <| Cut.start  cut, Vertex.y <| Cut.start  cut,
                           Vertex.x <| Cut.finish cut, Vertex.y <| Cut.finish cut)
            |> CutShape.create |> Some
        | None -> None

            
    /// Removes a cut shape from the graphics window.
    let private removeCutShape cutShape =
        CutShape.shapeId cutShape |> Shapes.Remove

    /// Initialises the scene, rendering all objects in it.
    let initialiseScene scene =
        SceneShape.create
        <| renderTree (Scene.tree scene)
        <| List.map renderObstacle (Scene.obstacles scene)
        <| renderTarget (Scene.target scene)
        <| renderCut (Scene.cut scene)

    /// Updates the scene shape, given the previous scene shape and the new scene state.
    let updateScene scene prevShape = 
        match SceneShape.cut prevShape with
        | Some cut -> removeCutShape cut
        | None     -> ()
        removeTreeShape (SceneShape.tree prevShape)

        prevShape
        |> SceneShape.updateCut  (renderCut  <| Scene.cut scene)
        |> SceneShape.updateTree (renderTree <| Scene.tree scene)