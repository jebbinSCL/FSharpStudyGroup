module CornerHitterShortcutVector

open System.Drawing
open System
open Helpers  

type GenerationData = {
    CurrentGen : VectorLine seq
    MagnitudeChangeFactor : float
    Angle : float<degree>
    AngleChangeByValue : float<degree>
}

let getChildren angle magnitudeChangeFactor (branch : VectorLine)= 
    let baseChildVector = {branch with StartPoint = (convertVectorToLine branch).EndPoint; UnitVector={X=0.0;Y= -1.0;}}
    let childOne = rotateVector baseChildVector angle 
    let childTwo = rotateVector baseChildVector -angle
    seq {
        yield {childOne with Magnitude = childOne.Magnitude * magnitudeChangeFactor}
        yield {childTwo with Magnitude = childTwo.Magnitude * magnitudeChangeFactor}
    }

let getChildGeneration (genData : GenerationData) = genData.CurrentGen |> Seq.collect (getChildren genData.Angle genData.MagnitudeChangeFactor)

let generateTreeFromTrunk startState = 
    seq {
        yield startState.CurrentGen
        let unfolder (state : GenerationData) =
            let nextGen = getChildGeneration state
            Some(nextGen,{state with CurrentGen=nextGen; Angle = state.Angle + state.AngleChangeByValue})
        let nextGenerationBranches = startState |> Seq.unfold (unfolder)
        yield! nextGenerationBranches 
    }

let drawVector (graphics : Graphics) pen shaodowPen (vector : VectorLine) =
    let line = convertVectorToLine vector
    let shadowStart = PointF(line.StartPoint.PointF.X - 1.0f, line.StartPoint.PointF.Y - 1.0f)
    let shadowEnd = PointF(line.EndPoint.PointF.X - 1.0f, line.EndPoint.PointF.Y - 1.0f)
    
    graphics.DrawLine(shaodowPen,shadowStart,shadowEnd)
    graphics.DrawLine(pen,line.StartPoint.PointF,line.EndPoint.PointF)

let drawAndSaveFractalTree (width : int) (height : int) = 

    let branchLength = 100
    let depth = 15
    let angle = 44.4<degree>

    let trunk : PointLine = { StartPoint= {X= (float width) / 2.0; Y = float height} ; EndPoint = {X=(float width)/2.0; Y=float (height - branchLength)} }
    let startState = {CurrentGen = seq {yield convertLineToVector trunk}; MagnitudeChangeFactor=1.0; Angle = angle; AngleChangeByValue = 0.0<degree> / float depth}

    let bmp = new Bitmap(width,height)
    let pen = new Pen(Color.LimeGreen, 3.0f)
    let shaodowPen = new Pen(Color.DarkGreen, 3.0f)
    use graphics = Graphics.FromImage(bmp)
    graphics.SmoothingMode <- System.Drawing.Drawing2D.SmoothingMode.HighQuality
    graphics.Clear(Color.Black)
    let drawVector' = drawVector graphics pen shaodowPen


    generateTreeFromTrunk startState |> Seq.take depth |> Seq.concat |> Seq.iter drawVector'

    bmp.Save("..\\..\\FractalTree.jpeg")

