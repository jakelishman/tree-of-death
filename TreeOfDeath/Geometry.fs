namespace TreeOfDeath

module Geometry =
    /// Calculate the absolute distance between two points.
    let distanceBetween a b =
        sqrt <| (float b.X - float a.X) ** 2.0 + (float b.Y - float a.Y) ** 2.0

    /// Calculate the bearing needed to go from point a to point b, where 0.0<rad> points
    /// directly upwards.
    let angleBetween src dest =
        1.0<rad> * atan2 (float dest.Y - float src.Y) (float dest.X - float src.X)