namespace wsoffline

open WebSharper
open WebSharper.Sitelets
open WebSharper.UI
open WebSharper.UI.Server

open Generator

type EndPoint =
    | [<EndPoint "GET /">] Home
    | [<EndPoint "GET /about">] About

module Templating =
    open WebSharper.UI.Html

    type MainTemplate = Templating.Template<"Main.html">

    // Compute a menubar where the menu item for the given endpoint is active
    let MenuBar (ctx: Context<EndPoint>) endpoint : Doc list =
        let ( => ) txt act =
             li [if endpoint = act then yield attr.``class`` "active"] [
                a [attr.href (ctx.Link act)] [text txt]
             ]
        [
            "Home" => EndPoint.Home
            "About" => EndPoint.About
        ]

    let Main ctx action (title: string) (body: Doc list) =
        Content.Page(
            MainTemplate()
                .Title(title)
                .MenuBar(MenuBar ctx action)
                .Body(body)
                .Doc()
        )


module Site =
    open WebSharper.UI.Html

    let HomePage ctx =
        let font = Font({Axes.DefaultAxes with thickness=10;})
        Templating.Main ctx EndPoint.Home "Home" [
            h1 [] [text "Say Hi to JavaScript!"]
            div [] [client <@ Client.Main() @>]
            Doc.Verbatim (font.stringToSvg "THE QUICK BROWN FOX JUMPS over the lazy dog 0123456789" 0 0 false)
            //SvgElements.svg [] [(font.stringToSvg "THE QUICK BROWN FOX JUMPS over the lazy dog 0123456789" 0 0 false)]
        ]

    let AboutPage ctx =
        Templating.Main ctx EndPoint.About "About" [
            h1 [] [text "About"]
            p [] [text "This is a template WebSharper generated html application."]
        ]

    [<Website>]
    let Main =
        Application.MultiPage (fun ctx action ->
            match action with
            | Home -> HomePage ctx
            | About -> AboutPage ctx
        )

[<Sealed>]
type Website() =
    interface IWebsite<EndPoint> with
        member this.Sitelet = Site.Main
        member this.Actions = [Home; About]

[<assembly: Website(typeof<Website>)>]
do ()
