namespace TreeOfDeath

[<AutoOpen>]
module GeometryModel =    
    /// Radian unit of measure.
    [<Measure>] type rad

    /// A point in 2 dimensions.
    type Vertex = { X : int ; Y : int }
    
module Vertex =
    /// Creates a vertex with the specified x and y coordinates.
    let create x y = { X = x ; Y = y }

    /// Gets the x coordinate of a vertex.
    let x vertex = vertex.X

    /// Gets the y coordinate of a vertex.
    let y vertex = vertex.Y

module Geometry =
    /// Calculate the absolute distance between two points.
    let distanceBetween a b =
        sqrt <| float (Vertex.x b - Vertex.x a) ** 2.0 + float (Vertex.y b - Vertex.y a) ** 2.0

    /// Calculate the bearing needed to go from point a to point b, where 0.0<rad> points
    /// directly upwards.
    let angleBetween src dest =
        let dy = float <| Vertex.y dest - Vertex.y src
        let dx = float <| Vertex.x dest - Vertex.x src
        1.0<rad> * atan2 dy dx

    /// Convert relative polar co-ordinates into absolute rectangular co-ordinates, given a start
    /// location.
    let polarToRectangular start (distance : float) (angle : float) =
        let x =
            distance
            |> (*) (cos angle)
            |> int
            |> (+) (Vertex.x start)
        let y =
            distance
            |> (*) (sin angle)
            |> int
            |> (+) (Vertex.y start)
        Vertex.create x y

    /// Indicates whether a vertex falls in the left or right half-plane of a line formed by two others.
    let sign v1 v2 v3 = 
            let (x1, y1) = (float <| Vertex.x v1, float <| Vertex.y v1)
            let (x2, y2) = (float <| Vertex.x v2, float <| Vertex.y v2)
            let (x3, y3) = (float <| Vertex.x v3, float <| Vertex.y v3)
            (x1 - x3) * (y2 - y3) - (x2 - x3) * (y1 - y3)
    
    /// Checks whether a point is inside a triangle formed by the three specified vertices.
    let pointInTriangle v1 v2 v3 pt =
        let b1 = sign pt v1 v2 < 0.0
        let b2 = sign pt v2 v3 < 0.0
        let b3 = sign pt v3 v1 < 0.0
        (b1 = b2) && (b2 = b3)

    /// Finds all triangles in a polygon by the ear-clipping method. (Inefficient implementation).
    let trianglesInPolygon polygon =
        let rec loop acc polygon =
            match polygon with
            | [] | [ _ ] | [ _ ; _ ] -> failwith "A polygon must have at least three vertices."
            | [ v1 ; v2 ; v3 ]       -> (v1, v2, v3) :: acc // if the polgyon only has three vertices, it is a triangle
            | v1 :: v2 :: v3 :: tail ->
                // if none of the other points in the polygon lie within the triangle formed by the first three vertices
                // then this triangle is an ear, so add it to the accumulator and remove the middle vertex
                // otherwise, skip this vertex
                match List.tryFind (pointInTriangle v1 v2 v3) tail with
                | None -> 
                    let ear = (v1, v2, v3)
                    loop (ear :: acc) (v1 :: v3 :: tail) 
                | _ ->
                    loop acc (List.append (v2 :: v3 :: tail) [ v1 ]) 
        
        loop [] polygon
