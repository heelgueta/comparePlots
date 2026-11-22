#!/usr/bin/env python3
"""Writes jamovi/distNum.u.yaml. Run from the repository root: python3 tools/gen_numeric_ui.py"""
import sys
sys.path.insert(0, "tools")
from ui_gen import *
# ---------------- distNum ----------------
num = head_ui("Compare Numeric", "distNum")
num += appearance_groups([
    ("Variables", [D("orientation"), D("sortVars"), D("transform"), C("invertVars"), C("useDesc"), S("valueLabel")]),
    ("Colours and Lines", [D("colourPalette"), C("reversePalette"), D("plotTheme"), T("alpha", suffix="%"), T("lineWidth")]),
    ("Data Points", [T("pointSize"), T("pointAlpha", suffix="%"), D("pointColour"), T("jitterSpread", suffix="%")]),
    ("Text and Labels", [T("textSize"), S("title"), D("legendPos"), C("showN")])])
num += box("Box Plot", "showBox", [T("boxWidth", "(showBox)", "%"), C("boxOutliers", "(showBox)"), C("boxNotch", "(showBox)"),
                                   C("boxMean", "(showBox)"), C("boxJitter", "(showBox)")], collapsed=False)
num += box("Violin Plot", "showViolin", [T("violinWidth", "(showViolin)", "%"), D("violinScale", "(showViolin)"), T("violinAdjust", "(showViolin)"),
                                         C("violinTrim", "(showViolin)"), C("violinBox", "(showViolin)"), C("violinJitter", "(showViolin)")])
num += box("Means and Error Bars", "showErrorBar", [
    D("ebCenter", "(showErrorBar)"), D("ebInterval", "(showErrorBar)"),
    T("ebLevel", "(showErrorBar && (ebInterval:ci || ebInterval:pctile))", "%"),
    T("ebMult", "(showErrorBar && (ebInterval:se || ebInterval:sd))"),
    C("ebJitter", "(showErrorBar)"), T("ebSize", "(showErrorBar)"), T("ebCap", "(showErrorBar)", "%"),
    C("ebConnect", "(showErrorBar)"), C("ebLabels", "(showErrorBar)")])
num += box("Raincloud Plot", "showRaincloud", [T("rcCloudWidth", "(showRaincloud)", "%"), T("rcAdjust", "(showRaincloud)"),
                                              C("rcRain", "(showRaincloud)"), C("rcBox", "(showRaincloud)")])
num += box("Ridgeline (Density) Plot", "showRidge", [T("ridgeOverlap", "(showRidge)", "%"), T("ridgeAdjust", "(showRidge)"),
                                                     C("ridgeTrim", "(showRidge)"), C("ridgeMedian", "(showRidge)")])
num += box("Histogram", "showHist", [T("histBins", "(showHist)"), D("histStat", "(showHist)"), C("histDensity", "(showHist)"), C("histFreeY", "(showHist)")])
num += box("Overlaid Densities", "showDensity", [T("densAdjust", "(showDensity)"), C("densFilled", "(showDensity)"), C("densTrim", "(showDensity)")])
num += box("Cumulative Distribution (ECDF)", "showEcdf", [C("ecdfMedian", "(showEcdf)")])
num += box("Data Points (Strip Plot)", "showStrip", [D("stripSummary", "(showStrip)")])
num += box("Barcode Plot", "showBarcode", [T("barcodeHeight", "(showBarcode)", "%")])
open(ROOT + "distNum.u.yaml", "w").write(num)

# ---------------- Compare Dichotomous / Compare Polytomous ----------------
# Same layout rules as Compare Numeric: ONE Appearance box (no separate Data box), then one box per plot.
def group_ui(title, name, dich):
    g = head_ui(title, name, group_var=True)
    g += appearance_groups([
        ("Variables", [D("groupMode"), D("orientation"), D("sortVars"), D("transform"), C("invertVars"), C("invertGroups"),
                       C("useDesc"), C("ignoreMissingGroup"), S("valueLabel")]),
        ("Colours and Lines", [D("colourPalette"), C("reversePalette"), D("plotTheme"), T("alpha", suffix="%"), T("lineWidth"), D("refLine")]),
        ("Data Points", [T("pointSize"), T("pointAlpha", suffix="%"), D("pointColour"), T("jitterSpread", suffix="%")]),
        ("Text and Labels", [T("textSize"), D("xLabelAngle"), S("title"), D("legendPos"), S("legendTitle"), C("showN")])])
    g += box("Box Plot", "showBox", [T("boxWidth", "(showBox)", "%"), C("boxOutliers", "(showBox)"), C("boxNotch", "(showBox)"),
                                     C("boxMean", "(showBox)"), C("boxJitter", "(showBox)")], collapsed=False)
    violin = [T("violinWidth", "(showViolin)", "%"), D("violinScale", "(showViolin)"), T("violinAdjust", "(showViolin)"),
              C("violinTrim", "(showViolin)"), C("violinBox", "(showViolin)")]
    if dich:
        violin.append(C("violinSplit", "(showViolin)"))
    violin.append(C("violinJitter", "(showViolin)"))
    g += box("Violin Plot", "showViolin", violin)
    g += box("Means and Error Bars", "showErrorBar", [
        D("ebCenter", "(showErrorBar)"), D("ebInterval", "(showErrorBar)"),
        T("ebLevel", "(showErrorBar && (ebInterval:ci || ebInterval:pctile))", "%"),
        T("ebMult", "(showErrorBar && (ebInterval:se || ebInterval:sd))"),
        C("ebJitter", "(showErrorBar)"), T("ebSize", "(showErrorBar)"), T("ebCap", "(showErrorBar)", "%"),
        C("ebConnect", "(showErrorBar)"), C("ebLabels", "(showErrorBar)")])
    g += box("Raincloud Plot", "showRaincloud", [T("rcCloudWidth", "(showRaincloud)", "%"), T("rcAdjust", "(showRaincloud)"),
                                                C("rcRain", "(showRaincloud)"), C("rcBox", "(showRaincloud)")])
    g += box("Ridgeline (Density) Plot", "showRidge", [T("ridgeOverlap", "(showRidge)", "%"), T("ridgeAdjust", "(showRidge)"),
                                                       C("ridgeTrim", "(showRidge)"), C("ridgeMedian", "(showRidge)")])
    g += box("Histogram", "showHist", [T("histBins", "(showHist)"), D("histStat", "(showHist)"), D("histPosition", "(showHist)"),
                                       C("histDensity", "(showHist)"), C("histFreeY", "(showHist)")])
    if dich:
        g += box("Back-to-Back Histogram", "showPyramid", [T("pyBins", "(showPyramid)"), D("pyStat", "(showPyramid)")])
    g += box("Overlaid Densities", "showDensity", [T("densAdjust", "(showDensity)"), C("densFilled", "(showDensity)"), C("densTrim", "(showDensity)")])
    g += box("Cumulative Distribution (ECDF)", "showEcdf", [C("ecdfMedian", "(showEcdf)")])
    g += box("Data Points (Strip Plot)", "showStrip", [D("stripSummary", "(showStrip)")])
    g += box("Barcode Plot", "showBarcode", [T("barcodeHeight", "(showBarcode)", "%")])
    return g

open(ROOT + "distDich.u.yaml", "w").write(group_ui("Compare Dichotomous", "distDich", True))
open(ROOT + "distPoly.u.yaml", "w").write(group_ui("Compare Polytomous", "distPoly", False))
