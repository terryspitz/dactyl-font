module explorer

open Browser.Dom
open Dactyl

// Get a reference to our button and cast the Element to an HTMLButtonElement
let textbox = document.getElementById("text") :?> Browser.Types.HTMLInputElement
let thicknessInput = document.getElementById("thickness") :?> Browser.Types.HTMLInputElement
let myButton = document.getElementById("generate") :?> Browser.Types.HTMLButtonElement
let output = document.getElementById("output")


// Register our listener
myButton.onclick <- fun _ ->
    let text = textbox.value
    let thickness = int thicknessInput.value
    let font = Dactyl.Font({Dactyl.Axes.DefaultAxes with thickness=thickness;})
    output.innerHTML <- font.stringToSvg "THE QUICK BROWN FOX JUMPS over the lazy dog 0123456789" 0 0 false
