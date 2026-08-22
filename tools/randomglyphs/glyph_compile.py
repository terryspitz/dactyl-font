"""Inverse compiler: Hershey centreline polylines -> Dactyl glyph strings.

The mirror of GlyphStringDefs.parse_curve.  Offline: run when adding a source
font, never at runtime.  Emits strings the existing F# parser can read back.
"""
import sys, math, statistics; sys.path.insert(0,'.')
from hershey_jhf import parse_jhf

# Dactyl guides (Axes.DefaultAxes: width 300, height 600, x_height .6, descender .5)
L,C,R,W = 0.,150.,300.,450.
B,X,H,T,D = 0.,360.,300.,600.,-300.
YG={'t':T,'x':X,'h':H,'b':B,'d':D}
XG={'l':L,'c':C,'r':R,'w':W}

def build_codebook(guides):
    """All A{m}B{n} weighted averages plus single letters -> {expr: value}."""
    book={}
    for a,va in guides.items(): book[a]=va
    ks=list(guides)
    for a in ks:
        for b in ks:
            if a>=b: continue
            for m in range(1,5):
                for n in range(1,5):
                    if m==1 and n==1: expr=f"{a}{b}"
                    elif n==1:        expr=f"{a}{m}{b}"
                    elif m==1:        expr=f"{a}{b}{n}"
                    else:             expr=f"{a}{m}{b}{n}"
                    v=(m*guides[a]+n*guides[b])/(m+n)
                    book.setdefault(expr,v)
    return book
YBOOK=build_codebook(YG); XBOOK=build_codebook(XG)
def snap(book,v):
    e=min(book, key=lambda k: (abs(book[k]-v), len(k)))
    return e, book[e]

# ---- Hershey -> Dactyl coordinate mapping (metrics derived empirically) ----
HB, HCAP = -9.0, 21.0          # baseline y, cap height in Hershey units
def make_mapper(fontglyphs):
    widths=[r-l for _,l,r,polys in fontglyphs if polys and r>l]
    medw=statistics.median(widths) if widths else 18.0
    sy = (T-B)/HCAP
    sx = (R-L)/medw
    def m(x,y,left): return ((x-left)*sx, (y-HB)*sy)
    return m

# ---- geometry helpers ----
def rdp(pts, eps):
    if len(pts)<3: return pts[:]
    def d(p,a,b):
        if a==b: return math.dist(p,a)
        (x0,y0),(x1,y1),(x2,y2)=p,a,b
        return abs((y2-y1)*x0-(x2-x1)*y0+x2*y1-y2*x1)/math.hypot(x2-x1,y2-y1)
    dmax,idx=0,0
    for i in range(1,len(pts)-1):
        dd=d(pts[i],pts[0],pts[-1])
        if dd>dmax: dmax,idx=dd,i
    if dmax>eps:
        return rdp(pts[:idx+1],eps)[:-1]+rdp(pts[idx:],eps)
    return [pts[0],pts[-1]]

def seg_is_curved(orig, a, b, tol):
    """Do the original points between kept vertices a..b bow away from the chord?"""
    ia,ib=orig.index(a),orig.index(b)
    if ib<ia: ia,ib=ib,ia
    span=orig[ia:ib+1]
    if len(span)<3: return False
    (x1,y1),(x2,y2)=span[0],span[-1]
    ln=math.hypot(x2-x1,y2-y1)
    if ln<1e-9: return False
    dev=max(abs((y2-y1)*x0-(x2-x1)*y0+x2*y1-y2*x1)/ln for x0,y0 in span[1:-1])
    return dev>tol

def turn_angle(p,q,r):
    a=math.atan2(q[1]-p[1],q[0]-p[0]); b=math.atan2(r[1]-q[1],r[0]-q[0])
    d=abs(math.degrees(b-a))%360
    return min(d,360-d)

def compile_stroke(pts, eps, curve_tol, corner_deg):
    """polyline (dactyl coords) -> glyph-string fragment"""
    uniq=[pts[0]]
    for p in pts[1:]:
        if math.dist(p,uniq[-1])>1e-6: uniq.append(p)
    if len(uniq)==1:
        (x,y)=uniq[0]; return snap(YBOOK,y)[0]+snap(XBOOK,x)[0]
    closed = math.dist(uniq[0],uniq[-1]) < eps*1.5 and len(uniq)>3
    if closed:
        # A closed contour repeats its start point at the end; keeping both leaves a
        # zero-length segment that the solver renders as a lump. Drop the duplicate and
        # let the trailing separator close the path instead.
        uniq=uniq[:-1]
        if len(uniq)<3: return None

    # 1. corners: sharp turns in the ORIGINAL polyline (not RDP chords).
    #    Measured over a small window so a coarsely-sampled arc doesn't read as a corner.
    corner_idx=set()
    rng = range(len(uniq)) if closed else range(1,len(uniq)-1)
    for i in rng:
        p=uniq[i-1]; q=uniq[i]; r=uniq[(i+1)%len(uniq)]
        if turn_angle(p,q,r)>corner_deg: corner_idx.add(i)

    # 2. RDP within each corner-to-corner span, so corners are always kept
    breaks=[0]+sorted(corner_idx)+[len(uniq)-1]
    keep=[]
    for a,b in zip(breaks[:-1],breaks[1:]):
        seg=rdp(uniq[a:b+1], eps)
        keep.extend(seg[:-1])
    keep.append(uniq[breaks[-1]])
    if len(keep)<2: return None
    keepi=[]
    j=0
    for p in keep:
        while j<len(uniq) and uniq[j]!=p: j+=1
        keepi.append(min(j,len(uniq)-1)); j=max(j,0)

    # 3. separators: curved if the original bows away from the chord
    seps=[]
    for i in range(len(keep)-1):
        a,b=keepi[i],keepi[i+1]
        span=uniq[a:b+1]
        seps.append('~' if span_bows(span,curve_tol) else '-')

    out=[]
    for i,(x,y) in enumerate(keep):
        tok=snap(YBOOK,y)[0]+snap(XBOOK,x)[0]
        if keepi[i] in corner_idx and 0<i<len(keep)-1 and seps[i-1]=='~' and seps[i]=='~':
            tok+='K'
        out.append(tok+(seps[i] if i<len(seps) else ''))
    s="".join(out)
    if closed: s += (seps[-1] if seps else '-')
    return s

def span_bows(span, tol):
    if len(span)<3: return False
    (x1,y1),(x2,y2)=span[0],span[-1]
    ln=math.hypot(x2-x1,y2-y1)
    if ln<1e-9: return False
    return max(abs((y2-y1)*x0-(x2-x1)*y0+x2*y1-y2*x1)/ln for x0,y0 in span[1:-1])>tol

def compile_glyph(rec, mapper, eps=26.0, curve_tol=9.0, corner_deg=62.0):
    idx,left,right,polys = rec
    frags=[]
    for pl in polys:
        dp=[mapper(x,y,left) for x,y in pl]
        f=compile_stroke(dp, eps, curve_tol, corner_deg)
        if f: frags.append(f)
    return " ".join(frags)
