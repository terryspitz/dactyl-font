module Explorer

open Browser.Dom
open Generator

// Get a reference to our button and cast the Element to an HTMLButtonElement
let textbox = document.getElementById("text") :?> Browser.Types.HTMLInputElement
let thicknessInput = document.getElementById("thickness") :?> Browser.Types.HTMLInputElement
//let myButton = document.getElementById("generate") :?> Browser.Types.HTMLButtonElement
let output = document.getElementById("output")

let generate _ = 
    let text = textbox.value
    let thickness = int thicknessInput.value
    let font = Font({Axes.DefaultAxes with thickness=thickness;})
    let rowHeight = font.Axes.height + 400
    let svg = font.stringToSvg text 0 rowHeight false |> toSvgDocument (rowHeight*2) (text.Length * (font.stringWidth text))
    output.innerHTML <- svg

textbox.oninput <- generate
thicknessInput.oninput <- generate
generate()
