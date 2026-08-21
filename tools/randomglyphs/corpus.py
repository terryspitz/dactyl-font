"""Combined stroke inventory: Hershey simplex faces + Dactyl's own glyphs.

Offline.  `SIMPLEX` lists only faces that pass verify_centreline.py; run that
before adding to it.
"""
import sys, os, re, math
HERE=os.path.dirname(os.path.abspath(__file__))
REPO=os.path.abspath(os.path.join(HERE,'..','..'))
sys.path.insert(0,HERE)
from hershey_jhf import parse_jhf
from glyph_compile import compile_glyph, make_mapper, YBOOK, XBOOK, snap, YG, XG

SIMPLEX=['futural','rowmans','greeks','scripts']   # verified single-line centrelines

pt_pat=re.compile(r"^([txhbd0-9]+|\([txhbd0-9]+\))([oe])?([lrcw0-9]+|\([lrcw0-9]+\))([oe])?([NSEW])?(K)?(J)?")
def wavg(cs,tbl):
    vals=[];i=0;cs=cs.strip('()')
    while i<len(cs):
        ch=cs[i];i+=1;num=''
        while i<len(cs) and cs[i].isdigit(): num+=cs[i];i+=1
        vals+=[tbl[ch]]*(int(num) if num else 1)
    return sum(vals)/len(vals)
def split_pts(stroke):
    parts=re.split(r"([-~])",stroke); out=[];i=0
    while i<len(parts):
        p=parts[i]
        if p: out.append((p, parts[i+1] if i+1<len(parts) else ''))
        i+=2
    return out
def decode(stroke):
    toks=split_pts(stroke); pts=[]; seps=[]
    for p,s in toks:
        m=pt_pat.match(p)
        if not m: return None
        pts.append((wavg(m.group(3),XG), wavg(m.group(1),YG))); seps.append(s)
    closed = seps[-1] in ('-','~')
    return pts, seps, closed

def role(pts,closed):
    xs=[p[0] for p in pts]; ys=[p[1] for p in pts]
    w=max(xs)-min(xs); h=max(ys)-min(ys)
    if len(pts)==1: return 'dot'
    if closed: return 'bowl'
    if w<60 and h>200: return 'stem'
    if h<60 and w>100: return 'bar'
    if len(pts)==2: return 'diag'
    return 'arc'

def build():
    inv={}; combos=[]
    # Hershey simplex
    for name in SIMPLEX:
        g=parse_jhf(os.path.join(HERE,'data','hershey',f'{name}.jhf')); m=make_mapper(g)
        for rec in g:
            if not rec[3]: continue
            d=compile_glyph(rec,m)
            if not d.strip(): continue
            rs=[]
            for s in d.split(' '):
                if not s.strip(): continue
                dec=decode(s)
                if dec is None: continue
                pts,seps,closed=dec
                r=role(pts,closed); rs.append(r)
                inv.setdefault(r,[]).append((name,s,pts,seps,closed))
            if rs: combos.append(tuple(rs))
    # Dactyl's own
    src=open(os.path.join(REPO,'src','generator','GlyphStringDefs.fs')).read()
    blk=src[src.index('let glyphMap'):src.index('let altGlyphMap')]
    for c,d in re.findall(r"^\s*'(.)',\s*\"([^\"]*)\"", blk, re.M):
        if not d.strip() or c=='□': continue
        rs=[]
        for s in d.split(' '):
            if not s.strip(): continue
            dec=decode(s)
            if dec is None: continue
            pts,seps,closed=dec
            r=role(pts,closed); rs.append(r)
            inv.setdefault(r,[]).append(('dactyl',s,pts,seps,closed))
        if rs: combos.append(tuple(rs))
    return inv, combos

if __name__=='__main__':
    inv,combos=build()
    print("stroke inventory:", {k:len(v) for k,v in sorted(inv.items())})
    print("total strokes:", sum(len(v) for v in inv.values()))
    print("glyphs:", len(combos), " distinct role patterns:", len(set(combos)))
