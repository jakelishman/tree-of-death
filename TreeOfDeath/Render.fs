namespace TreeOfDeath

open Library
open Library.Colors

[<AutoOpen>]
module Render =

    /// Renders a tree node from the given starting position.
    let rec private renderNode start = function
        // if this is a leaf node, then add a like and create a leaf tree shape
        | Leaf finish -> 
            TreeShape.createLeaf 
            <| Shapes.AddLine (Vertex.x start, Vertex.y start, Vertex.x finish, Vertex.y finish)
        | Bend (point, next) ->
            TreeShape.createBend
            <| Shapes.AddLine (Vertex.x start, Vertex.y start, Vertex.x point, Vertex.y point)
            <| renderNode point next
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
        GraphicsWindow.PenColor <- SaddleBrown
        GraphicsWindow.PenWidth <- 4.0
        renderNode (Tree.start tree) (Tree.firstNode tree)
    
    /// Removes the tree shape fromm the graphics window.
    let private removeTreeShape treeShape =
        TreeShape.childShapeIds treeShape 
        |> List.iter Shapes.Remove

    /// Renders an obstacle in the graphics window, returning the generated obstacle shape which contains
    /// identifiers for all the drawn shapes. 
    let private renderObstacle obstacle =
        GraphicsWindow.PenWidth   <- 0.0
        GraphicsWindow.BrushColor <- DarkGreen
        Obstacle.triangles obstacle
        |> List.map (fun (v1, v2, v3) -> Shapes.AddTriangle (Vertex.x v1, Vertex.y v1,
                                                             Vertex.x v2, Vertex.y v2,
                                                             Vertex.x v3, Vertex.y v3))
        |> ObstacleShape.create

    /// Removes the obstacle shape from the graphics window.
    let private removeObstacleShape obstacleShape =
        ObstacleShape.childShapeIds obstacleShape
        |> List.iter Shapes.Remove

    /// Renders a target in the graphics window, returning the generated obstacle shape which contains an
    /// identifier for the drawn shape.
    let private renderTarget target =
        GraphicsWindow.PenWidth <- 0.0
        GraphicsWindow.BrushColor <- Red
        let centre = Target.centre target
        let shape = Shapes.AddEllipse(2.0 * Target.radius target, 2.0 * Target.radius target)
        Shapes.Move(shape, Vertex.x centre - Target.radius target, Vertex.y centre - Target.radius target)
        shape |> TargetShape.create

    /// Removes a target shape from the graphics window.
    let private removeTargetShape targetShape =
        TargetShape.shapeId targetShape |> Shapes.Remove

    /// Renders a cut in the graphics window, returning the generated cut shape which contains an identifier
    /// for the drawn shape.
    let private renderCut = function 
        | Some cut -> 
            GraphicsWindow.PenColor <- Black
            GraphicsWindow.PenWidth <- 1.0
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