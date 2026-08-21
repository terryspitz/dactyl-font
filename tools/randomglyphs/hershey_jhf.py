"""Parser for Hershey .jhf vector-font files (offline ingestion).

Format: 5-char glyph number, 3-char vertex count, then coordinate pairs where
each character encodes a value as ord(c) - ord('R').  The first pair is the
left/right side bearing; the pair " R" is a pen-up (start of a new stroke).
Hershey y grows downward, so it is flipped here to Dactyl's y-up convention.
"""

def parse_jhf(path):
    """Yield (index, left, right, [polyline,...]) ; polyline = [(x,y),...]  y-up."""
    out=[]
    # records can wrap across lines: a record starts with 5-char number + 3-char count
    raw=open(path, encoding='latin-1').read().split('\n')
    # rejoin wrapped records: a new record begins when line[0:5] is a right-justified int
    recs=[]; cur=None
    for line in raw:
        if not line.strip(): continue
        head=line[:5]
        if head.strip().isdigit() and len(line)>=8 and line[5:8].strip().isdigit():
            if cur is not None: recs.append(cur)
            cur=line
        elif cur is not None:
            cur += line
    if cur is not None: recs.append(cur)
    for rec in recs:
        idx=int(rec[:5]); n=int(rec[5:8])
        body=rec[8:]
        pairs=[body[i:i+2] for i in range(0,len(body),2)]
        pairs=[p for p in pairs if len(p)==2]
        if not pairs: continue
        left = ord(pairs[0][0])-ord('R'); right = ord(pairs[0][1])-ord('R')
        polys=[]; cp=[]
        for p in pairs[1:n]:
            if p==' R':
                if len(cp)>1: polys.append(cp)
                cp=[]
            else:
                x=ord(p[0])-ord('R'); y=ord(p[1])-ord('R')
                cp.append((x,-y))          # Hershey y is DOWN; flip to y-up
        if len(cp)>1: polys.append(cp)
        out.append((idx,left,right,polys))
    return out
