namespace TreeOfDeath

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