namespace TreeOfDeath

module Main =

    open Library

    GraphicsWindow.Width  <- 1024
    GraphicsWindow.Height <- 768

    [<EntryPoint>]
    let main argv = 
        let scene = Init.scene (Vertex.create 1024 768)
        let shape = Render.initialiseScene scene

        printfn "%A" argv
        0 // return an integer exit code