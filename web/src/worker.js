import { generateSvg, generateSvgPerGlyph, generateSplineDebugSvgFromDefs, generateTweenSvg, generateTweenDiffSvg, generateVisualDiffsSvg, controlDefinitions, solveSplineEditor, solveSplineGrid, solveAltSplines, getGuidePositions, getGlyphList, parseGlyphToControlPoints, generateFontGlyphDataPerGlyph, getSplineOutlinePath } from './lib/fable/Api'
import { buildFontDataUrl } from './fontExport'
import { generateGrowthSvg, generateGrowthField } from './growthSvg'
import { generateBranchSvg } from './branchSvg'
import { generateTextureSvg } from './textureSvg'
import { DControlPoint } from './lib/fable/generator/DactylSpline'

self.onmessage = (e) => {
    const { id, type, args } = e.data
    const start = performance.now()
    try {
        let result
        let transfer = []
        switch (type) {
            case 'font':
                result = generateSvg(...args, (p) => {
                    self.postMessage({ id, type: 'progress', value: p });
                })
                break
            // Same as 'font' but every character occurrence gets its own axes
            // (see glyphRandom.js's buildPerGlyphTextAxes)
            case 'fontPerGlyph': {
                const [fText, fBaseAxes, fAxesList, fAutoscale] = args
                result = generateSvgPerGlyph(fText, fBaseAxes, fAxesList, fAutoscale, (p) => {
                    self.postMessage({ id, type: 'progress', value: p });
                })
                break
            }
            case 'glyphsFromDefs':
                result = generateSplineDebugSvgFromDefs(...args, (p) => {
                    self.postMessage({ id, type: 'progress', value: p });
                })
                break
            case 'tweens': {
                const [char, axes, steps = 9] = args
                const data = {}
                const EXCLUDED_TWEEN_AXES = ['spacing', 'leading']
                const tweenControls = controlDefinitions.filter(c => !EXCLUDED_TWEEN_AXES.includes(c.name) && c.category !== 'debug')
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
                const [ctrlPtsRaw, isClosed, maxIter, flatness, endWeight] = args
                const ctrlPts = ctrlPtsRaw.map(p => new DControlPoint(p.ty, p.x, p.y, p.th_in, p.th_out))
                result = solveSplineEditor(ctrlPts, isClosed, maxIter, flatness, endWeight)
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
            case 'growth': {
                const [growText, growAxes, growParams] = args
                result = generateGrowthSvg(growText, growAxes, growParams, (p) => {
                    self.postMessage({ id, type: 'progress', value: p });
                })
                break
            }
            case 'growthField': {
                const [gText, gAxes, gParams] = args
                result = generateGrowthField(gText, gAxes, gParams ?? {}, (p) => {
                    self.postMessage({ id, type: 'progress', value: p });
                })
                if (result) transfer = [result.rg.buffer]
                break
            }
            case 'branch': {
                const [brText, brAxes, brParams] = args
                result = generateBranchSvg(brText, brAxes, brParams, (p) => {
                    self.postMessage({ id, type: 'progress', value: p });
                })
                break
            }
            case 'texture': {
                const [txText, txAxes, txParams] = args
                result = generateTextureSvg(txText, txAxes, txParams, (p) => {
                    self.postMessage({ id, type: 'progress', value: p });
                })
                break
            }
            // chars/axesList are the optional per-glyph random overrides; empty = uniform font
            case 'fontData': {
                const [fontAxes, chars = '', axesList = []] = args
                result = generateFontGlyphDataPerGlyph(fontAxes, chars, axesList, (p) => {
                    self.postMessage({ id, type: 'progress', value: p });
                })
                break
            }
            case 'fontPreview': {
                const [fontAxes, chars = '', axesList = []] = args
                // Two phases with very different costs, so the bar is weighted by
                // measured share rather than split evenly: outlines+kerns in F#
                // ~67%, then paper.js union/opentype assembly ~33%. Without this
                // the bar hit 100% and sat there for the last third of the work.
                const GLYPH_DATA_SHARE = 0.67
                const report = (p) => self.postMessage({ id, type: 'progress', value: p })
                const glyphData = generateFontGlyphDataPerGlyph(fontAxes, chars, axesList,
                    (p) => report(p * GLYPH_DATA_SHARE))
                result = buildFontDataUrl(glyphData, 'DactylPreview',
                    (p) => report(GLYPH_DATA_SHARE + p * (1 - GLYPH_DATA_SHARE)))
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
        self.postMessage({ id, result }, transfer)
    } catch (error) {
        console.log(`API [${type}] failed after ${(performance.now() - start).toFixed(1)}ms`)
        self.postMessage({ id, error: error.message })
    }
}
