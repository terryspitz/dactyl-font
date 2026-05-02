import { generateSvg, generateSplineDebugSvgFromDefs, generateTweenSvg, generateTweenDiffSvg, generateVisualDiffsSvg, controlDefinitions, solveSplineEditor, solveSplineGrid, solveAltSplines, getGuidePositions, getGlyphList, parseGlyphToControlPoints, generateFontGlyphData, getSplineOutlinePath } from './lib/fable/Api'
import { buildFontDataUrl } from './fontExport'
import { DControlPoint } from './lib/fable/generator/DactylSpline'

self.onmessage = (e) => {
    const { id, type, args } = e.data
    const start = performance.now()
    try {
        let result
        switch (type) {
            case 'font':
                result = generateSvg(...args, (p) => {
                    self.postMessage({ id, type: 'progress', value: p });
                })
                break
            case 'glyphsFromDefs':
                result = generateSplineDebugSvgFromDefs(...args, (p) => {
                    self.postMessage({ id, type: 'progress', value: p });
                })
                break
            case 'tweens': {
                const [char, axes, steps = 9] = args
                const data = {}
                const EXCLUDED_TWEEN_AXES = ['tracking', 'leading', 'debug']
                const tweenControls = controlDefinitions.filter(c => !EXCLUDED_TWEEN_AXES.includes(c.name))
                const totalVariations = tweenControls.reduce((sum, c) => sum + (c.type_ === 'checkbox' ? 3 : steps), 0)
                let completed = 0

                tweenControls.forEach(ctrl => {
                    const variations = []
                    const vals = ctrl.type_ === 'checkbox'
                        ? [0, 1, 'diff']
                        : Array.from({ length: steps }, (_, i) => ctrl.min + (ctrl.max - ctrl.min) * (i / (steps - 1)))

                    for (const val of vals) {
                        const svg = val === 'diff'
                            ? generateTweenDiffSvg(char, { ...axes, [ctrl.name]: 0 }, { ...axes, [ctrl.name]: 1 })
                            : generateTweenSvg(char, { ...axes, [ctrl.name]: val })
                        variations.push({ val, svg })

                        completed++
                        self.postMessage({ id, type: 'progress', value: completed / totalVariations });
                    }
                    data[ctrl.name] = variations
                })
                result = data
                break
            }
            case 'visualDiffs':
                result = generateVisualDiffsSvg(...args, (p) => {
                    self.postMessage({ id, type: 'progress', value: p });
                })
                break
            case 'solveSpline': {
                const [ctrlPtsRaw, isClosed, maxIter, glyphAxes] = args
                const ctrlPts = ctrlPtsRaw.map(p => new DControlPoint(p.ty, p.x, p.y, p.th_in, p.th_out))
                result = solveSplineEditor(ctrlPts, isClosed, maxIter, glyphAxes)
                break
            }
            case 'parseGlyph': {
                const [char, glyphAxes] = args
                result = parseGlyphToControlPoints(char, glyphAxes)
                break
            }
            case 'getGuides': {
                const [guideAxes] = args
                result = getGuidePositions(guideAxes)
                break
            }
            case 'getGlyphList': {
                result = getGlyphList()
                break
            }
            case 'solveSplineGrid': {
                result = solveSplineGrid()
                break
            }
            case 'fontData': {
                const [fontAxes] = args
                result = generateFontGlyphData(fontAxes)
                break
            }
            case 'fontPreview': {
                const [fontAxes] = args
                result = buildFontDataUrl(generateFontGlyphData(fontAxes), 'DactylPreview')
                break
            }
            case 'splineOutline': {
                const [ctrlPtsRaw, isClosed, glyphAxes] = args
                const ctrlPts = ctrlPtsRaw.map(p => new DControlPoint(p.ty, p.x, p.y, p.th_in, p.th_out))
                result = getSplineOutlinePath(ctrlPts, isClosed, glyphAxes)
                break
            }
            case 'solveAltSplines': {
                const [ctrlPtsRaw, isClosed, glyphAxes] = args
                const ctrlPts = ctrlPtsRaw.map(p => new DControlPoint(p.ty, p.x, p.y, p.th_in, p.th_out))
                result = solveAltSplines(ctrlPts, isClosed, glyphAxes)
                break
            }
            default:
                throw new Error(`Unknown generation type: ${type}`)
        }
        console.log(`API [${type}] took ${(performance.now() - start).toFixed(1)}ms`)
        self.postMessage({ id, result })
    } catch (error) {
        console.log(`API [${type}] failed after ${(performance.now() - start).toFixed(1)}ms`)
        self.postMessage({ id, error: error.message })
    }
}
