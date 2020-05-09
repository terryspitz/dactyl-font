namespace websharper2

open WebSharper
open WebSharper.JavaScript
open WebSharper.JQuery
open WebSharper.UI
open WebSharper.UI.Client
open WebSharper.UI.Templating

open Generator

[<JavaScript>]
module Client =
    // The templates are loaded from the DOM, so you just can edit index.html
    // and refresh your browser, no need to recompile unless you add or remove holes.
    type IndexTemplate = Template<"wwwroot/index.html", ClientLoad.FromDocument>

    let People =
        ListModel.FromSeq [
            "John"
            "Paul"
        ]


    [<SPAEntryPoint>]
    let Main () =
        let newName = Var.Create ""
        let font = Font({Axes.DefaultAxes with thickness=10;})
    

        IndexTemplate.Main()
            .ListContainer(
                People.View.DocSeqCached(fun (name: string) ->
                    IndexTemplate.ListItem().Name(name).Doc()
                )
            )
            .Name(newName)
            .Add(fun _ ->
                People.Add(newName.Value)
                newName.Value <- ""
            )
            .Font((font.stringToSvg "THE QUICK BROWN FOX JUMPS over the lazy dog 0123456789" 0 0 false))
            .Doc()
        |> Doc.RunById "main"
