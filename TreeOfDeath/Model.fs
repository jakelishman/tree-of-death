namespace TreeOfDeath

open Library

[<AutoOpen>]
module Model =
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
          SceneTarget    : Target
          SceneCut       : Cut option }

    [<AutoOpen>]
    /// Defines types which represent shapes added to the scene for later reference.
    module Shapes = 
        /// Contains all the shapes identifiers in the scene which belong to a tree.
        type TreeShape =
            | ShapeLeaf   of shapeId : string
            | ShapeBranch of shapeId : string * left : TreeShape * right : TreeShape

        /// Contains all the shape identifies in the scene which belong to an obstacle.
        type ObstaclesShape = { ObstacleTriangleIds : string list }

        /// Contains the shape identifier for the target in the scene.
        type TargetShape = { TargetEllipseId : string }

        /// Contains the shape identifier for a cut in the scene, if there is one.
        type CutShape = { CutLineId : string }

        /// Contains all the shapes in the scene.
        type SceneShape =
            { SceneTreeShape      : TreeShape
              SceneObstacleShapes : ObstaclesShape list
              SceneTargetShape    : TargetShape
              SceneCutShape       : CutShape option }

    /// API functiosn for advancing updating the scene (Jake).
    type LogicApi =
        { Grow           : Tree -> Tree
          CheckCollision : Tree -> Obstacle -> bool
          ReachedTarget  : Tree -> Target -> bool
          UpdateScene    : Scene -> Cut option -> Scene }

    /// API functions for rendering the scene (Anton).
    type RenderApi =
        { RenderTree      : Tree -> TreeShape
          RenderObstacle  : Obstacle -> ObstaclesShape
          RenderTarget    : Target -> TargetShape
          RenderCut       : Cut -> CutShape
          InitialiseScene : Scene -> SceneShape
          UpdateScene     : Scene -> SceneShape -> SceneShape }

module Obstacle = 
    /// Creates an obstacle defined by a polygon consisting of the specified vertex list.
    let create vertices =
        if List.length vertices < 3 then failwith "Cannot create an obstacle with fewer than 3 vertices."
        { ObstaclePolygon = vertices }

    /// Returns the vertices of all the triangles contained in an obstacle.
    let triangles obstacle = 
        Geometry.trianglesInPolygon obstacle.ObstaclePolygon

module Target = 
    /// Creates a target with the specified centre and radius.
    let create centre radius =
        { TargetCentre = centre
          TargetRadius = radius }

    /// Gets the vertex representing the centre of the target.
    let centre target = target.TargetCentre

    /// Gets the raidus of the target.
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
    /// Gets the tree from a scene.
    let tree scene = scene.SceneTree

    /// Gets the target from a scene.
    let target scene = scene.SceneTarget

    /// Gets the obstacle list from a scene.
    let obstacles scene = scene.SceneObstacles

    /// Gets the cut for a scene.
    let cut scene = scene.SceneCut

    /// Create a scene with the given quantities.
    let create tree obstacles target cut =
        { SceneTree      = tree
          SceneObstacles = obstacles
          SceneTarget    = target
          SceneCut       = cut }

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

    /// Gets the list of child shape identifiers for the tree.
    let rec childShapeIds = function
        | ShapeLeaf shapeId -> [ shapeId ]
        | ShapeBranch (shapeId, leftShape, rightShape) -> shapeId :: (List.append (childShapeIds leftShape) (childShapeIds rightShape))

module ObstacleShape =
    /// Creates an obstacle shape consisting of the given list of triangle shapes.
    let create triangleIds = { ObstacleTriangleIds = triangleIds }

    /// Gets the list of child shape identifiers for the tree.
    let childShapeIds obstacleShape = obstacleShape.ObstacleTriangleIds

module TargetShape =
    /// Creates a target shape with the specified shape identifier.
    let create shapeId = { TargetEllipseId = shapeId }

    /// Gets the shape identifier for the target.
    let shapeId targetShape = targetShape.TargetEllipseId

module CutShape =
    /// Creates a cut shape with the specified shape identifier.
    let create shapeId = { CutLineId = shapeId }

    /// Gets the shape identifier for the cut.
    let shapeId cutShape = cutShape.CutLineId

module SceneShape =
    /// Creates a scene shape containing identifiers for all objects in the scene.
    let create treeShape obstacleShapes targetShape cutShape =
        { SceneTreeShape      = treeShape
          SceneObstacleShapes = obstacleShapes
          SceneTargetShape    = targetShape
          SceneCutShape       = cutShape }

    /// Gets the tree shape for a scene shape.
    let tree sceneShape = sceneShape.SceneTreeShape

    /// Gets the obstacle shapes for a scene shape.
    let obstacles sceneShape = sceneShape.SceneObstacleShapes

    /// Gets the target shapes for a scene shape.
    let target sceneShape = sceneShape.SceneTargetShape

    /// Gets the cut shape for a scene shape.
    let cut sceneShape = sceneShape.SceneCutShape

    /// Returns a new scene shape with the updated tree shape.
    let updateTree treeShape sceneShape = { sceneShape with SceneTreeShape = treeShape }

    /// Returns a new scene shape with the updated cut shape.
    let updateCut cutShape sceneShape = { sceneShape with SceneCutShape = cutShape }
