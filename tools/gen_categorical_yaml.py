#!/usr/bin/env python3
"""Generates the yaml of the sibling categorical analyses from the ordinal one.

distOrd.{a,r}.yaml is the master (edit THAT); this script writes
  distNom             : the nominal analysis (no diverging / profile / mean-category plots,
                        plus pie, donut, grouped bars and lollipop; category order by frequency)
and the three .u.yaml files. Run from the repository root:  python3 tools/gen_categorical_yaml.py
"""
import re, sys
ROOT = "jamovi/"

# ---------------------------------------------------------------- yaml chunk helpers
def split_chunks(txt, start_re=r"^  - name: "):
    lines = txt.split("\n"); head = []; chunks = []; cur = None; pending = []
    for ln in lines:
        if re.match(start_re, ln):
            cur = {"pre": pending, "lines": [ln]}; chunks.append(cur); pending = []
        elif re.match(r"^  #", ln) and cur is not None:
            pending.append(ln)
        elif cur is None:
            head.append(ln)
        else:
            if pending:               # a comment block followed by non-name lines: keep with previous
                cur["lines"] += pending; pending = []
            cur["lines"].append(ln)
    return head, chunks, pending

def chunk_name(c): return re.match(r"^  - name: (\S+)", c["lines"][0]).group(1)
def join(head, chunks, tail=()):
    out = list(head)
    for c in chunks: out += c["pre"] + c["lines"]
    out += list(tail)
    return "\n".join(out)

a_txt = open(ROOT + "distOrd.a.yaml").read()
r_txt = open(ROOT + "distOrd.r.yaml").read()

# ---------------------------------------------------------------- nominal
head, chunks, tail = split_chunks(a_txt)
drop = {"showDiverging", "divWidth", "divNeutral", "divLabels", "divMinLabel", "divNet",
        "showProfile", "profLines", "profPoints", "profLabels", "sortVars"}
out = []
for c in chunks:
    nm = chunk_name(c)
    if nm in drop: continue
    if nm == "invertVars":    # new options for nominal go right before it
        out.append({"pre": [], "lines": """  - name: sortVars
    title: Order variables
    type: List
    options:
      - name: none
        title: Entry
      - name: name
        title: A–Z
      - name: top
        title: Top share
      - name: diverse
        title: Diversity
    default: none
  - name: catSort
    title: Order categories
    type: List
    options:
      - name: levels
        title: Data order
      - name: frequency
        title: Frequency
      - name: name
        title: A–Z
    default: levels""".split("\n")})
    out.append(c)
NEW_OPTS = """  # PLOT: Pie chart.
  - name: showPie
    title: Pie chart
    type: Bool
    default: false
  - name: pieLabels
    title: '% labels'
    type: Bool
    default: true
  - name: pieMinLabel
    title: Smallest % to label
    type: Integer
    min: 0
    max: 50
    default: 5
  - name: pieGrid
    title: Charts per row
    type: Integer
    min: 1
    max: 10
    default: 3
  # PLOT: Donut chart.
  - name: showDonut
    title: Donut chart
    type: Bool
    default: false
  - name: donutHole
    title: Hole size
    type: Integer
    min: 10
    max: 90
    default: 55
  - name: donutLabels
    title: '% labels'
    type: Bool
    default: true
  - name: donutMinLabel
    title: Smallest % to label
    type: Integer
    min: 0
    max: 50
    default: 6
  - name: donutN
    title: n in the centre
    type: Bool
    default: true
  - name: donutGrid
    title: Charts per row
    type: Integer
    min: 1
    max: 10
    default: 3
  # PLOT: Grouped bars.
  - name: showGrouped
    title: Grouped bars
    type: Bool
    default: false
  - name: groupWidth
    title: Bar thickness
    type: Integer
    min: 10
    max: 100
    default: 85
  - name: groupLabels
    title: '% labels'
    type: Bool
    default: true
  # PLOT: Lollipop plot.
  - name: showLollipop
    title: Lollipop plot
    type: Bool
    default: false
  - name: lolliSize
    title: Dot size
    type: Number
    min: 1
    max: 6
    default: 3"""
nom_a = join(head, out, [NEW_OPTS, "..."]).replace("distOrd", "distNom").replace("Compare Ordinal", "Compare Nominal")
nom_a = re.sub(r"      - ordinal\n      - nominal\n", "      - nominal\n", nom_a)
nom_a = re.sub(r"\.\.\.\n\.\.\.\s*$", "...\n", nom_a)
nom_a = nom_a.replace("    suggested:\n      - nominal\n    permitted:\n      - factor", "    suggested:\n      - nominal\n    permitted:\n      - factor")
open(ROOT + "distNom.a.yaml", "w").write(nom_a.rstrip("\n") + "\n" if nom_a.rstrip().endswith("...") else nom_a)

# results
rh, rch, rt = split_chunks(r_txt, r"^  - name: ")
rout = []
tmpl = next(c for c in rch if chunk_name(c) == "plot_bars")
def image(key, title, extra_clear):
    ls = "\n".join(tmpl["lines"]) + "\n"
    ls = ls.replace("plot_bars", "plot_" + key).replace(".plot_bars", ".plot_" + key).replace("showBars", "show" + key.capitalize())
    ls = re.sub(r"title: .*\n", f"title: {title}\n", ls, count=1)
    ls = re.sub(r"(      - showN\n)(?:      - \w+\n)*", r"\1      - alpha\n      - lineWidth\n      - ignoreMissing\n" + "".join(f"      - {x}\n" for x in extra_clear), ls)
    if key in ("pie", "donut"):
        ls = ls.replace("    height: 350\n", "    height: 400\n")
    return {"pre": [f'  # IMAGE for the "{title}" plot.'], "lines": ls.rstrip("\n").split("\n")}
for c in rch:
    nm = chunk_name(c)
    if nm in ("plot_diverging", "plot_profile"): continue
    rout.append(c)
for key, title, clear in (("pie", "Pie chart", ["pieGrid", "pieLabels", "pieMinLabel"]),
                          ("donut", "Donut chart", ["donutGrid", "donutHole", "donutLabels", "donutMinLabel", "donutN"]),
                          ("grouped", "Grouped bars", ["groupWidth", "groupLabels"]),
                          ("lollipop", "Lollipop plot", ["lolliSize"])):
    rout.append(image(key, title, clear))
nom_r = join(rh, rout, rt).replace("distOrd", "distNom").replace("Compare Ordinal", "Compare Nominal")
nom_r = nom_r.replace("      - divNeutral\n", "      - catSort\n")
nom_r = nom_r.replace("      - useDesc\n      - catSort\n", "      - useDesc\n      - catSort\n")
open(ROOT + "distNom.r.yaml", "w").write(nom_r)

# ---------------------------------------------------------------- UI
sys.path.insert(0, "tools")
from ui_gen import *
def appearance_ui(nominal=False):
    vo = [D("orientation"), D("sortVars")] + ([D("catSort")] if nominal else []) + [C("invertVars"), C("useDesc"), C("ignoreMissing")]
    return [("Variables", vo),
            ("Appearance", [D("colourPalette"), C("reversePalette"), D("plotTheme"), T("alpha", suffix="%"), T("lineWidth")]),
            ("Text and labels", [T("textSize"), T("decimals"), S("title"), D("legendPos"), S("legendTitle"), C("showN")])]
def ui(name, title, boxes, groups):
    s = head_ui(title, name) + appearance_groups(groups)
    for label, check, its, collapsed in boxes:
        s += box(label, check, its, collapsed=collapsed)
    open(ROOT + name + ".u.yaml", "w").write(s)
ORD_BOXES = [
 ("Stacked Bars", "showBars", [T("barWidth", "(showBars)", "%"), C("barLabels", "(showBars)"), T("barMinLabel", "(showBars && barLabels)", "%")], False),
 ("Diverging Bars", "showDiverging", [T("divWidth", "(showDiverging)", "%"), D("divNeutral", "(showDiverging)"), C("divLabels", "(showDiverging)"),
                                      T("divMinLabel", "(showDiverging && divLabels)", "%"), C("divNet", "(showDiverging)")], True),
 ("Heatmap", "showHeat", [D("heatPalette", "(showHeat)"), C("heatLabels", "(showHeat)"), T("heatGap", "(showHeat)", "%")], True),
 ("Waffle Chart", "showWaffle", [T("waffleRows", "(showWaffle)"), T("waffleCols", "(showWaffle)"), T("waffleGap", "(showWaffle)", "%"), T("waffleGrid", "(showWaffle)")], True),
 ("Dot Pictogram", "showPicto", [T("pictoRows", "(showPicto)"), T("pictoCols", "(showPicto)"), D("pictoShape", "(showPicto)"), T("pictoSize", "(showPicto)"), T("pictoGrid", "(showPicto)")], True),
 ("Parliament Chart", "showParliament", [T("parlSeats", "(showParliament)"), T("parlRows", "(showParliament)"), T("parlSize", "(showParliament)"), T("parlGrid", "(showParliament)")], True),
 ("Response Profile", "showProfile", [C("profLines", "(showProfile)"), C("profPoints", "(showProfile)"), C("profLabels", "(showProfile)")], True)]
ui("distOrd", "Compare Ordinal", ORD_BOXES, appearance_ui())
NOM_BOXES = [b for b in ORD_BOXES if b[1] in ("showBars", "showHeat", "showWaffle", "showPicto", "showParliament")] + [
 ("Pie Chart", "showPie", [C("pieLabels", "(showPie)"), T("pieMinLabel", "(showPie && pieLabels)", "%"), T("pieGrid", "(showPie)")], True),
 ("Donut Chart", "showDonut", [T("donutHole", "(showDonut)", "%"), C("donutLabels", "(showDonut)"), T("donutMinLabel", "(showDonut && donutLabels)", "%"), C("donutN", "(showDonut)"), T("donutGrid", "(showDonut)")], True),
 ("Grouped Bars", "showGrouped", [T("groupWidth", "(showGrouped)", "%"), C("groupLabels", "(showGrouped)")], True),
 ("Lollipop Plot", "showLollipop", [T("lolliSize", "(showLollipop)")], True)]
ui("distNom", "Compare Nominal", NOM_BOXES, appearance_ui(True))
print("categorical yaml written")
