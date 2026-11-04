# Building comparePlots (macOS, no separate R needed)

jamovi ships its own R, which is enough to build and test:

```sh
# 1. once: environment (adjust the app path if jamovi lives elsewhere)
cat > ~/.jmvenv.sh <<'ENV'
export R_HOME=/Applications/jamovi.app/Contents/Frameworks/R.framework/Versions/Current/Resources
export R_LIBS=/Applications/jamovi.app/Contents/Resources/modules/base/R
export DYLD_LIBRARY_PATH=/Applications/jamovi.app/Contents/Frameworks
export PATH=$R_HOME/bin:$PATH
ENV

# 2. once: the jamovi compiler into a user library
source ~/.jmvenv.sh; mkdir -p ~/jmvtools-lib
R --no-save -q -e 'options(repos=c(jamovi="https://repo.jamovi.org", CRAN="https://cloud.r-project.org")); install.packages("jmvtools", lib="~/jmvtools-lib")'

# 3. every time: tests, build, install into jamovi (writes comparePlots_1.0.0.jmo, git-ignored)
source ~/.jmvenv.sh; export R_LIBS=$HOME/jmvtools-lib:$R_LIBS
R --no-save -q -e 'testthat::test_dir("tests/testthat")'
R --no-save -q -e 'jmvtools::install(home="/Applications/jamovi.app")'
git checkout jamovi/0000.yaml     # the compiler rewrites it without comments
```

- Do not edit `R/*.h.R` or `jamovi/*.u.yaml` of the generated analyses by hand:
  `python3 tools/gen_numeric_ui.py` (numeric UI) and `python3 tools/gen_categorical_yaml.py`
  (nominal yaml + all categorical UIs, from the `distOrd.{a,r}.yaml` master) write them.
- In headless R, pass variable names inline (`vars = names(df)[1:4]`) or with `!!`; a bare variable
  name is read as a column name by jmvcore.
- Do not `library(comparePlots)` in a test script; use `asNamespace("comparePlots")`.
