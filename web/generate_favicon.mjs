import { generateTweenSvg, defaultAxes } from './src/lib/fable/Api.js';
import fs from 'fs';

// Configure axes for Bold D
const axes = { ...defaultAxes, thickness: 35 };

// Generate SVG
const svgContent = generateTweenSvg("D", axes);

// Convert to White on Black
let finalSvg = svgContent.replace(/black/g, "white");

// Parse ViewBox to create background rect
const viewBoxMatch = finalSvg.match(/viewBox='([^']+)'/);
if (viewBoxMatch) {
    let [minX, minY, width, height] = viewBoxMatch[1].split(' ').map(Number);

    // Add extra top margin to push the D down
    const extraMargin = 60;
    minY -= extraMargin;
    height += extraMargin;

    // Update the viewBox in the SVG string
    finalSvg = finalSvg.replace(viewBoxMatch[1], `${minX} ${minY} ${width} ${height}`);

    // Insert black background rect before the content group
    const rect = `<rect x='${minX}' y='${minY}' width='${width}' height='${height}' fill='black' />`;
    // Insert after <svg ...> and before <g ...>
    // The typical structure is <svg ...> <g id='1'> ...
    finalSvg = finalSvg.replace(/(<svg[^>]+>)/, `$1${rect}`);
}

fs.writeFileSync('public/dactyl-icon.svg', finalSvg);
console.log("Generated public/dactyl-icon.svg");
