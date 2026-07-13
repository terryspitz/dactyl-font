// GPU preview for the Grow tab.  The worker computes the three-channel growth
// field (d1 = distance to nearest spine, dOpp = distance to nearest opposing
// spine, cross = cross-glyph opposition flag) once per text/axes change; this
// component uploads it as an RGB32F texture and derives the ink + keyline
// bands in a fragment shader:
//
//   g = gap - cross * fuse * (gap + fuseMerge)
//   f = clamp(dOpp - g, rMin, rMax) - d1
//
// grow / gap / fuse / layer offsets are uniforms, so dragging sliders (and
// animating growth) re-renders on the GPU without touching the worker at all.

import { useRef, useEffect } from 'react'
import { LAYER_COLORS, layerIsoLevels } from './growth'

const VERT = `#version 300 es
in vec2 aPos;
out vec2 vUV;
void main() {
  vUV = aPos * 0.5 + 0.5;
  gl_Position = vec4(aPos, 0.0, 1.0);
}`

// Manual bilinear sampling via texelFetch: float-texture LINEAR filtering is
// an optional extension, texelFetch works everywhere WebGL2 does.
const FRAG = `#version 300 es
precision highp float;
uniform sampler2D uField;
uniform vec2 uTexSize;
uniform float uRMin;
uniform float uRMax;
uniform float uGap;
uniform float uFuse;
uniform float uFuseMerge;
uniform int uBandCount;
uniform float uBandOffsets[4];
uniform vec3 uBandColors[4];
uniform vec3 uBg;
in vec2 vUV;
out vec4 outColor;

vec3 sampleField(vec2 uv) {
  vec2 p = uv * uTexSize - 0.5;
  vec2 i = floor(p);
  vec2 fr = p - i;
  ivec2 mx = ivec2(uTexSize) - 1;
  ivec2 i00 = clamp(ivec2(i), ivec2(0), mx);
  ivec2 i11 = clamp(ivec2(i) + 1, ivec2(0), mx);
  vec3 a = texelFetch(uField, ivec2(i00.x, i00.y), 0).rgb;
  vec3 b = texelFetch(uField, ivec2(i11.x, i00.y), 0).rgb;
  vec3 c = texelFetch(uField, ivec2(i00.x, i11.y), 0).rgb;
  vec3 d = texelFetch(uField, ivec2(i11.x, i11.y), 0).rgb;
  return mix(mix(a, b, fr.x), mix(c, d, fr.x), fr.y);
}

void main() {
  vec3 dd = sampleField(vUV);
  // Relax (and overshoot) the gap only where opposition is cross-glyph.
  float g = uGap - dd.b * uFuse * (uGap + uFuseMerge);
  float f = clamp(dd.g - g, uRMin, uRMax) - dd.r;
  float aa = fwidth(f) * 0.7 + 1e-4;
  vec3 col = uBg;
  for (int i = 3; i >= 0; i--) {
    if (i >= uBandCount) continue;
    float t = smoothstep(-aa, aa, f - uBandOffsets[i]);
    col = mix(col, uBandColors[i], t);
  }
  outColor = vec4(col, 1.0);
}`

const hexToRgb = (hex) => {
    const v = parseInt(hex.slice(1), 16)
    return [((v >> 16) & 255) / 255, ((v >> 8) & 255) / 255, (v & 255) / 255]
}

function initGL(canvas) {
    const gl = canvas.getContext('webgl2', { antialias: false, alpha: false })
    if (!gl) return null
    const compile = (type, src) => {
        const s = gl.createShader(type)
        gl.shaderSource(s, src)
        gl.compileShader(s)
        if (!gl.getShaderParameter(s, gl.COMPILE_STATUS)) {
            console.error('shader compile failed:', gl.getShaderInfoLog(s))
            return null
        }
        return s
    }
    const vs = compile(gl.VERTEX_SHADER, VERT)
    const fs = compile(gl.FRAGMENT_SHADER, FRAG)
    if (!vs || !fs) return null
    const prog = gl.createProgram()
    gl.attachShader(prog, vs)
    gl.attachShader(prog, fs)
    gl.linkProgram(prog)
    if (!gl.getProgramParameter(prog, gl.LINK_STATUS)) {
        console.error('program link failed:', gl.getProgramInfoLog(prog))
        return null
    }
    gl.useProgram(prog)
    const quad = gl.createBuffer()
    gl.bindBuffer(gl.ARRAY_BUFFER, quad)
    gl.bufferData(gl.ARRAY_BUFFER, new Float32Array([-1, -1, 3, -1, -1, 3]), gl.STATIC_DRAW)
    const aPos = gl.getAttribLocation(prog, 'aPos')
    gl.enableVertexAttribArray(aPos)
    gl.vertexAttribPointer(aPos, 2, gl.FLOAT, false, 0, 0)
    const texture = gl.createTexture()
    gl.bindTexture(gl.TEXTURE_2D, texture)
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.NEAREST)
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.NEAREST)
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_S, gl.CLAMP_TO_EDGE)
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_T, gl.CLAMP_TO_EDGE)
    const uni = (n) => gl.getUniformLocation(prog, n)
    return {
        gl,
        u: {
            texSize: uni('uTexSize'), rMin: uni('uRMin'), rMax: uni('uRMax'),
            gap: uni('uGap'), fuse: uni('uFuse'), fuseMerge: uni('uFuseMerge'),
            bandCount: uni('uBandCount'),
            bandOffsets: uni('uBandOffsets'), bandColors: uni('uBandColors'),
            bg: uni('uBg'),
        },
    }
}

export default function GrowCanvas({ field, params, zoom }) {
    const canvasRef = useRef(null)
    const ctxRef = useRef(null)
    const rafRef = useRef(0)

    // (Re)upload the field texture when it changes.
    useEffect(() => {
        if (!field) return
        if (!ctxRef.current) ctxRef.current = initGL(canvasRef.current)
        const ctx = ctxRef.current
        if (!ctx) return
        const { gl } = ctx
        gl.pixelStorei(gl.UNPACK_ALIGNMENT, 1)
        gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGB32F, field.nx, field.ny, 0, gl.RGB, gl.FLOAT, field.rg)
    }, [field])

    // Draw (and animate) whenever field/params/zoom change.
    useEffect(() => {
        const ctx = ctxRef.current
        const canvas = canvasRef.current
        if (!ctx || !field || !canvas) return
        const { gl, u } = ctx

        const thickness = field.thickness
        const isoLevels = params.layers ? layerIsoLevels(thickness) : [0]
        const colors = (params.layers ? LAYER_COLORS : ['#000000']).map(hexToRgb)
        const rMin = thickness / 2

        // Field texel grid → display pixels: same 0.5 scale as the SVG path,
        // times zoom, rendered at devicePixelRatio for crispness.
        const wUnits = (field.nx - 1) * field.cell
        const hUnits = (field.ny - 1) * field.cell
        const dispW = Math.max(1, (wUnits / 2) * zoom)
        const dispH = Math.max(1, (hUnits / 2) * zoom)
        const dpr = Math.min(window.devicePixelRatio || 1, 2)
        canvas.width = Math.min(4096, Math.round(dispW * dpr))
        canvas.height = Math.min(4096, Math.round(dispH * dpr))
        canvas.style.width = `${dispW}px`
        canvas.style.height = `${dispH}px`

        const draw = (growValue) => {
            gl.viewport(0, 0, canvas.width, canvas.height)
            gl.uniform2f(u.texSize, field.nx, field.ny)
            gl.uniform1f(u.rMin, rMin)
            gl.uniform1f(u.rMax, rMin + growValue * field.growScale)
            gl.uniform1f(u.gap, params.gap)
            gl.uniform1f(u.fuse, params.fuse ?? 0)
            gl.uniform1f(u.fuseMerge, rMin)
            gl.uniform1i(u.bandCount, isoLevels.length)
            const offsets = new Float32Array(4)
            isoLevels.forEach((v, i) => { offsets[i] = v })
            gl.uniform1fv(u.bandOffsets, offsets)
            const cols = new Float32Array(12)
            colors.forEach((c, i) => cols.set(c, i * 3))
            gl.uniform3fv(u.bandColors, cols)
            gl.uniform3f(u.bg, 1, 1, 1)
            gl.drawArrays(gl.TRIANGLES, 0, 3)
        }

        if (params.animate) {
            const t0 = performance.now()
            const tick = () => {
                // Ramp 0 → grow and hold briefly before looping.
                const period = 3000
                const t = ((performance.now() - t0) % period) / period
                const ramp = Math.min(1, t / 0.8)
                draw(params.grow * ramp)
                rafRef.current = requestAnimationFrame(tick)
            }
            tick()
            return () => cancelAnimationFrame(rafRef.current)
        }
        draw(params.grow)
    }, [field, params, zoom])

    return <canvas ref={canvasRef} style={{ display: 'block', margin: '10px' }} />
}
