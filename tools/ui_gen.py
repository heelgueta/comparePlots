"""Helpers that write jamovi .u.yaml files (used by gen_categorical_yaml.py and gen_numeric_ui.py)."""
import sys, re
ROOT = "jamovi/"

def ctrl(kind, name, enable=None, suffix=None, fmt=None, ind=0):
    p = " " * ind
    s = f"{p}- type: {kind}\n{p}  name: {name}\n"
    if fmt: s += f"{p}  format: {fmt}\n"
    if suffix: s += f"{p}  suffix: '{suffix}'\n"
    if enable: s += f"{p}  enable: '{enable}'\n"
    return s

def grid(items, ind):
    """items: list of (kind,name,enable,suffix,fmt); two columns"""
    p = " " * ind
    out = f"{p}- type: LayoutBox\n{p}  margin: large\n{p}  stretchFactor: 1\n{p}  children:\n"
    for i, (kind, name, enable, suffix, fmt) in enumerate(items):
        out += (f"{p}    - type: LayoutBox\n{p}      cell:\n{p}        column: {i % 2}\n{p}        row: {i // 2}\n"
                f"{p}      stretchFactor: 1\n{p}      children:\n")
        out += ctrl(kind, name, enable, suffix, fmt, ind + 8)
    return out

def box(label, check, items, collapsed=True, note=""):
    """one CollapseBox: the plot's own checkbox first, then its settings"""
    s = f"  # {note}\n" if note else ""
    s += f"  - type: CollapseBox\n    label: {label}\n    collapsed: {'true' if collapsed else 'false'}\n    stretchFactor: 1\n    children:\n"
    s += f"      - type: LayoutBox\n        margin: large\n        children:\n" + ctrl("CheckBox", check, ind=10)
    s += grid(items, 6)
    return s

def T(name, enable=None, suffix=None): return ("TextBox", name, enable, suffix, "number")
def S(name, enable=None): return ("TextBox", name, enable, None, "string")
def C(name, enable=None): return ("CheckBox", name, enable, None, None)
def D(name, enable=None): return ("ComboBox", name, enable, None, None)

def head_ui(title, name):
    return (f"# {title} ({name}): USER INTERFACE, hand-written.\n"
            "# compilerMode is `tame`: jmc keeps this layout and only adds controls for NEW options.\n"
            "# Each plot has its own CollapseBox that holds the plot's checkbox AND its settings; the settings are\n"
            "# greyed out (enable:) while the checkbox is off, or while a sibling option they depend on is off.\n"
            f"title: {title}\nname: {name}\njus: '3.0'\nstage: 0\ncompilerMode: tame\nchildren:\n"
            "  - type: VariableSupplier\n    persistentItems: false\n    stretchFactor: 1\n    children:\n"
            "      - type: TargetLayoutBox\n        label: Variables\n        children:\n"
            "          - type: VariablesListBox\n            name: vars\n            isTarget: true\n")

def appearance(items):
    return ("  # Appearance: options shared by all plots. Open by default.\n"
            "  - type: CollapseBox\n    label: Appearance\n    collapsed: false\n    stretchFactor: 1\n    children:\n"
            + grid(items, 6))

