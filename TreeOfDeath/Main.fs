namespace TreeOfDeath

module Main =
    let mutable scene      = Init.scene (Vertex.create 1024 768)
    let mutable sceneShape = Render.initialiseScene scene

    open Library

    GraphicsWindow.Width  <- 1024
    GraphicsWindow.Height <- 768

    [<EntryPoint>]
    let main argv = 
        

        Timer.Interval <- 50
        Timer.Tick <- (fun () -> 
            let result = Update.scene scene None
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