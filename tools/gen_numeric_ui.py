#!/usr/bin/env python3
"""Writes jamovi/distNum.u.yaml. Run from the repository root: python3 tools/gen_numeric_ui.py"""
import sys
sys.path.insert(0, "tools")
from ui_gen import *
# ---------------- distNum ----------------
num = head_ui("Compare Numeric", "distNum")
num += appearance([
    D("orientation"), D("sortVars"), C("invertVars"), D("colourPalette"), C("reversePalette"), D("plotTheme"),
    T("textSize"), C("useDesc"), T("alpha", suffix="%"), D("legendPos"), T("lineWidth"), T("pointSize"),
    T("jitterSpread", suffix="%"), C("showN"), S("valueLabel"), S("title")])
num += box("Box Plot", "showBox", [T("boxWidth", "(showBox)", "%"), C("boxOutliers", "(showBox)"), C("boxNotch", "(showBox)"),
                                   C("boxMean", "(showBox)"), C("boxJitter", "(showBox)")], collapsed=False)
num += box("Violin Plot", "showViolin", [T("violinWidth", "(showViolin)", "%"), D("violinScale", "(showViolin)"), T("violinAdjust", "(showViolin)"),
                                         C("violinTrim", "(showViolin)"), D("violinInner", "(showViolin)"), C("violinJitter", "(showViolin)")])
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


