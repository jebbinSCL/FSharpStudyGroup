module Helpers

open System

[<Measure>] type radian
[<Measure>] type degree

let convertDegreeToRadian (angle : float<degree>) : float<radian> = 
    let radiansPerDegree  = (Math.PI * 1.0<radian>) / 180.0<degree>
    radiansPerDegree * angle

type Point = {X : float; Y : float} with 
    member p.PointF = System.Drawing.PointF(float32 p.X, float32 p.Y)
type Vector = Point
type PointLine = {StartPoint : Point; EndPoint : Point} 
type VectorLine = {StartPoint : Point; UnitVector : Vector; Magnitude : float}

let vectorMagnitude (vector : Vector) = 
    sqrt ((vector.X ** 2.0) + (vector.Y ** 2.0))

let lineLength (line : PointLine) = 
    sqrt ( ((line.EndPoint.X - line.StartPoint.X) ** 2.0) + ((line.EndPoint.Y - line.StartPoint.Y) ** 2.0) )

let addPoints (p1 : Point) (p2 : Point) = 
    {X = p1.X + p2.X; Y = p1.Y + p2.Y }

//<summary>Returns a new point = p1 - p2</summary>
let subtractPoints (p1 : Point) (p2 : Point) = 
    {X =p1.X - p2.X; Y =  p1.Y - p2.Y}

let convertLineToVector (line : PointLine) = 
    let vector = (subtractPoints line.EndPoint (line.StartPoint))
    let magnitude = vectorMagnitude vector
    let unitVector = {X = vector.X/magnitude; Y = vector.Y/magnitude}
    {StartPoint = line.StartPoint; UnitVector = unitVector; Magnitude = magnitude}

let convertVectorToLine (vector : VectorLine) = 
    let vec = {X= vector.UnitVector.X * vector.Magnitude; Y=vector.UnitVector.Y * vector.Magnitude}
    {StartPoint = vector.StartPoint; EndPoint = addPoints vector.StartPoint vec}

let rotateWrtOrigin (p : Point) (angle : float<degree>) = 
    let x = p.X
    let y = p.Y
    let radianAngle = float <| convertDegreeToRadian angle
    let cosAngle = Math.Cos(radianAngle)
    let sinAngle = Math.Sin(radianAngle)
    let x' = x * cosAngle - y * sinAngle
    let y' = x * sinAngle + y * cosAngle
    {X= x'; Y= y'}

let rotateWrtPoint (p : Point) (aroundPoint : Point) (angle : float<degree>) = 
    addPoints aroundPoint <| rotateWrtOrigin (subtractPoints p aroundPoint) angle

let rotateVectorOld (vector : VectorLine) (degAngle : float<degree>) = 
    let x = float vector.UnitVector.X
    let y = float vector.UnitVector.Y
    let angle = float <| convertDegreeToRadian degAngle
    let cosAngle = Math.Cos angle
    let sinAngle = Math.Sin angle
    let x' = x * cosAngle - y * sinAngle
    let y' = x * sinAngle - y * cosAngle
    {vector with UnitVector = {X = x' ; Y = y'}}

let rotateVector2 (vector : VectorLine) (degAngle : float<degree>) = 
    let x = vector.UnitVector.X * vector.Magnitude
    let y = vector.UnitVector.Y * vector.Magnitude
    let angle = float <| convertDegreeToRadian degAngle
    let cosAngle = Math.Cos angle
    let sinAngle = Math.Sin angle
    let x' = x * cosAngle - y * sinAngle
    let y' = x * sinAngle - y * cosAngle
    let xFinal = x' + vector.StartPoint.X
    let yFinal = x' + vector.StartPoint.Y
    {vector with UnitVector = {X = xFinal/ vector.Magnitude ; Y = yFinal /vector.Magnitude}}

let rotateVector (vector : VectorLine) (degAngle : float<degree>) =
    let vectorWithMagnitude = {X=vector.UnitVector.X * vector.Magnitude;Y=vector.UnitVector.Y * vector.Magnitude}
    let rotatedVec = rotateWrtOrigin vectorWithMagnitude degAngle
    let rotUnitVec = {X = rotatedVec.X / vector.Magnitude; Y = rotatedVec.Y / vector.Magnitude}
    {vector with UnitVector = rotUnitVec}







