#!/usr/bin/env python3
"""Derives jamovi/distPoly.a.yaml and distPoly.r.yaml (Compare Polytomous) from distDich.*.yaml
(Compare Dichotomous, the source of truth), dropping what needs exactly two groups: the split
violin, the back-to-back histogram, the percentile ladder, the lead plot and the density overlap. Run from the repository root: python3 tools/gen_group_analyses.py"""
import re

ROOT = "jamovi/"
DROP_OPTIONS = ["violinSplit", "showPyramid", "pyBins", "pyStat", "showLadder", "ladderSteps", "showLead", "leadMark", "densOverlap"]
DROP_RESULTS = ["plot_pyramid", "plot_ladder", "plot_lead"]


def drop_items(text, names):
    """Remove top-level list items ('  - name: X' plus its indented body and the comment lines just above it)."""
    lines = text.split("\n")
    out, i = [], 0
    while i < len(lines):
        m = re.match(r"^  - name: (\S+)\s*$", lines[i])
        if m and m.group(1) in names:
            while out and out[-1].startswith("  #"):      # comment attached to the item
                out.pop()
            i += 1
            while i < len(lines) and (lines[i].startswith("    ") or lines[i].startswith("      ")):
                i += 1
            continue
        out.append(lines[i])
        i += 1
    return "\n".join(out)


def derive(src, kind):
    s = open(ROOT + src).read()
    s = s.replace("distDich", "distPoly").replace("Compare Dichotomous", "Compare Polytomous")
    names = DROP_OPTIONS if kind == "a" else DROP_RESULTS
    s = drop_items(s, names)
    if kind == "r":
        s = re.sub(r"      - (pyBins|pyStat|showPyramid|violinSplit|ladderSteps|showLadder|showLead|leadMark|densOverlap)\n", "", s)
        s = s.replace("Source of truth for both group analyses (see distPoly.a.yaml).", "DERIVED from distDich.r.yaml by tools/gen_group_analyses.py: edit that file.")
    else:
        s = re.sub(r"# Compare Polytomous \(distPoly\): OPTIONS\..*?\n# jmvtools compiles this file \(with distPoly\.r\.yaml\) into R/distPoly\.h\.R\.",
                   "# Compare Polytomous (distPoly): OPTIONS. DERIVED from distDich.a.yaml by tools/gen_group_analyses.py: edit that file.\n# jmvtools compiles this file (with distPoly.r.yaml) into R/distPoly.h.R.", s, flags=re.S)
    open(ROOT + f"distPoly.{kind}.yaml", "w").write(s)


derive("distDich.a.yaml", "a")
derive("distDich.r.yaml", "r")
