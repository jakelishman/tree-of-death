namespace TreeOfDeath

module Main =
    let mutable scene      = Init.scene (Vertex.create 1024 768)
    let mutable sceneShape = Render.initialiseScene scene
    let mutable cutStart   = None 

    open Library

    GraphicsWindow.Width  <- 1024
    GraphicsWindow.Height <- 768

    [<EntryPoint>]
    let main argv = 
        

        Timer.Interval <- 50
        Timer.Tick <- (fun () -> 
            let cut = 
                match cutStart with
                | Some start ->
                    let isInPorgress = Mouse.IsLeftButtonDown
                    let cut = Cut.create start (Vertex.create (int Mouse.X) (int Mouse.Y)) isInPorgress
                    if not isInPorgress then cutStart <- None
                    Some cut

                | None ->
                    if Mouse.IsLeftButtonDown then cutStart <- Some <| Vertex.create (int Mouse.X) (int Mouse.Y)
                    None

            let result = Update.scene scene cut
            match result with
            | GameInProgress newScene ->
                scene <- newScene
                sceneShape <- Render.updateScene scene sceneShape
            | GameWon ->
                printfn "Game won!"
            | GameLost ->
                printfn "Game lost!")


        printfn "%A" argv
        0 // return an integer exit code