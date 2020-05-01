// Credit to Boris (https://github.com/fierval) for the original code to interop with GraphViz
module GraphVisualisation.DrawGraph

open System
open System.Diagnostics
open System.IO

//let fileDirectory = Directory.GetCurrentDirectory()
let fileDirectory = @"D:\Documents\NTU\Year2\Sem2\CY2001Research\F#Implementation\GraphVisualiser\img"

/// Create a graph from a string representation
let createGraph (graph : string) (processName : string) (graphVizPath : string option) (filename: string option)=
    let workingDir =
        match graphVizPath with
        | Some p -> p
        | None -> String.Empty

    let graphFile =
        match filename with
        | Some p -> Path.Combine(fileDirectory, p)
        | None -> Path.GetTempFileName()
    File.WriteAllText(graphFile, graph)

    match processName with
    | procName when procName = "sfdp.exe" || procName = "dot.exe" ->

        let pi = ProcessStartInfo(Path.Combine(workingDir, processName))
        pi.CreateNoWindow <- true
        pi.ErrorDialog <- false;
        pi.UseShellExecute <- false;
        pi.Arguments <- String.Format("-Teps -O -Goverlap=prism {0}", graphFile)
        pi.WorkingDirectory <- workingDir
        try
            try
                let proc = new Process();
                proc.StartInfo <- pi
                proc.Start() |> ignore

                proc.WaitForExit()
                if proc.ExitCode = 0 then
                    printf "Image %s generated\n" graphFile
                else failwith "could not create image file\n"
            with
                | e -> printf "Exception occurred. %s\n" e.Message
        finally
            if File.Exists graphFile then File.Delete graphFile
    | _ -> failwith "Unknown graphing process"

let visualizeDot graph = createGraph graph "dot.exe" (Some @"C:\Graphviz2.38\bin\") None
let visualizeSfdp graph = createGraph graph "sfdp.exe" (Some @"C:\Graphviz2.38\bin\") None

let MakeGraph graph filename = createGraph graph "dot.exe" (Some @"C:\Graphviz2.38\bin\") filename
let createVisual = visualizeSfdp
let createVisualClusters = visualizeDot