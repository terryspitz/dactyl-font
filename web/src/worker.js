import { generateSvg, generateSplineDebugSvgFromDefs, generateTweenSvg, generateVisualTestsSvg, generateVisualDiffsSvg, controlDefinitions } from './lib/fable/Api'

self.onmessage = (e) => {
    const { id, type, args } = e.data
    try {
        let result
        switch (type) {
            case 'font':
                result = generateSvg(...args, (p) => {
                    self.postMessage({ id, type: 'progress', value: p });
                })
                break
            case 'splinesFromDefs':
                result = generateSplineDebugSvgFromDefs(...args, (p) => {
                    self.postMessage({ id, type: 'progress', value: p });
                })
                break
            case 'tweens': {
                const [char, axes] = args
                const steps = 9
                const data = {}
                const totalVariations = controlDefinitions.filter(c => c.type_ !== 'checkbox').length * steps
                let completed = 0

                controlDefinitions.filter(c => c.type_ !== 'checkbox').forEach(ctrl => {
                    const variations = []
                    const min = ctrl.min
                    const max = ctrl.max
                    const range = max - min

                    for (let i = 0; i < steps; i++) {
                        const val = min + (range * (i / (steps - 1)))
                        const tempAxes = { ...axes, [ctrl.name]: val }
                        const svg = generateTweenSvg(char, tempAxes, null) // Pass null for inner progress
                        variations.push({ val, svg })

                        completed++
                        self.postMessage({ id, type: 'progress', value: completed / totalVariations });
                    }
                    data[ctrl.name] = variations
                })
                result = data
                break
            }
            case 'visualTests':
                result = generateVisualTestsSvg()
                break
            case 'visualDiffs':
                result = generateVisualDiffsSvg(...args, (p) => {
                    self.postMessage({ id, type: 'progress', value: p });
                })
                break
            default:
                throw new Error(`Unknown generation type: ${type}`)
        }
        self.postMessage({ id, result })
    } catch (error) {
        self.postMessage({ id, error: error.message })
    }
}
