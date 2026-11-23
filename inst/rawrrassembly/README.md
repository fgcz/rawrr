# Setup

All commands in this document are executed live at render time against the
compiled `rawrr.exe` binary and the `sample.raw` demo file. Their **absolute
paths** are defined once here and exported as environment variables, so every
code chunk below uses the exact same files.

```{r setup}
#| echo: true
RAWEXE  <- "/home/cp/.cache/R/rawrr/rawrrassembly/linux-x64/rawrr"
RAWFILE <- "/home/cp/src/git.bioconductor.org/rawrr/inst/extdata/sample.raw"

Sys.setenv(RAWEXE  = RAWEXE)
Sys.setenv(RAWFILE = RAWFILE)

stopifnot(file.exists(RAWEXE), file.exists(RAWFILE))

# Run all later code chunks (bash included) in a scratch dir so that the
# temp input/output files they create do not clutter the source tree.
workdir <- tempfile("rawrrdoc-")
dir.create(workdir)
knitr::opts_knit$set(root.dir = workdir)

cat("rawrr.exe:", RAWEXE, "\n")
cat("sample.raw:", RAWFILE, "\n")
cat("scratch dir:", workdir, "\n")
```

# Usage

```
rawrr.exe <version-option>
rawrr.exe <help-option>

rawrr.exe <raw file> <mode>
rawrr.exe <raw file> <mode> <input file> <output file>
rawrr.exe <raw file> <mode> <option 2> <option 3> <input file> <output file>
```

- The **raw file path** is always the first argument.
- The **mode** is always the second argument; it selects the operation.
- Arguments after the mode are mode-specific (single options, or `<input file>` / `<output file>` pairs).
- The R package always passes the same base layout `<rawfile> <mode> <inputfile> <outputfile>` and fills `<inputfile>` / `<outputfile>` with temp files under `tempdir()`.

# Help & version (no raw file required)

| Token | Action |
|-------|--------|
| `help`, `--help`, `-h`, `h`, `/h` | Print usage and the list of options, then exit 0. |
| `version`, `--version`, `-v`, `-V`, `/v` | Print the assembly version (e.g. `1.17.2`), then exit 0. |

## Version

```{bash version}
"$RAWEXE" version
```

## Built-in help

```{bash help}
"$RAWEXE" help
```

# Modes

## `index` — scan index as CSV on stdout

```{bash index}
"$RAWEXE" "$RAWFILE" index | head -4
```

::: {.callout-note title="Full output"}
The complete index is `575` lines (a header line plus one line per scan);
`sample.raw` holds `574` scans.
:::

## `headerR` — metadata as R code

```{bash headerR}
"$RAWEXE" "$RAWFILE" headerR /dev/null header.R
head -6 header.R
```

## `filter` — scan numbers matching a filter, one per line

```{bash filter}
"$RAWEXE" "$RAWFILE" filter ms 10 filter.txt
head -8 filter.txt
```

If the filter is invalid the process exits with status `1`.

## `scans` — full spectra for requested scan numbers as R code

```{bash scans}
printf '3\n4\n5\n' > scanfile.txt
"$RAWEXE" "$RAWFILE" scans scanfile.txt scans.R
sed -n '1,10p' scans.R
```

## `barebone` — reduced spectra as R code

```{bash barebone}
printf '3\n4\n5\n' > scanfile.txt
"$RAWEXE" "$RAWFILE" barebone scanfile.txt barebone.R
sed -n '1,13p' barebone.R
```

## `chromatogram` — BPC and TIC as CSV

```{bash chromatogram}
"$RAWEXE" "$RAWFILE" chromatogram ms chrom.csv
head -6 chrom.csv
```

## `xic` — extracted-ion chromatograms within a mass tolerance as R code

```{bash xic}
printf '669.8381\n726.8357\n' > massfile.txt
"$RAWEXE" "$RAWFILE" xic 10 ms massfile.txt xic.R
sed -n '1,11p' xic.R
```

## `trailer` — trailer labels, and values for one label

```{bash trailer-labels}
"$RAWEXE" "$RAWFILE" trailer | head -12
```

The three-argument form prints the per-scan values of one label (one per line).

```{bash trailer-values}
"$RAWEXE" "$RAWFILE" trailer 'Ion Injection Time (ms):' | head -8
"$RAWEXE" "$RAWFILE" trailer 'Master Index:' | head -8
```

## `getFilters` — all scan filters of the raw file

```{bash getfilters}
"$RAWEXE" "$RAWFILE" getFilters
```

## `isValidFilter` — check whether an argument is a valid filter

```{bash isvalidfilter}
"$RAWEXE" "$RAWFILE" isValidFilter 'FTMS + c NSI Full ms [350.0000-1800.0000]'
```

# Exit / error behavior

- Unknown mode → prints the option list and exits 1.
- Missing raw file → `No RAW file specified!` and returns.
- Raw file open error, I/O error, or being acquired → message on stdout and returns.
- After `filter` with an invalid filter string the process exits 1.

# Notes for the R wrapper

The package's shared helper `.rawrrSystem2Source()` always hands
`<rawfile> <mode> <tfi> <tfo>`, where `tfi` is a temp file holding `input`
(scan numbers or masses, one per line) and `tfo` is the temp R-script file that
is `source()`d back on exit. Beware: it stages the `version` subcommand through
`.getRawrrAssemblyVersion()` and dispatches all other modes through this one
pathway or direct `system2()`.

# AI generation settings

This document was drafted and converted to an executable Quarto workbook with
assistance from an AI coding agent. The details below are recorded for
reproducibility and transparency.

| Setting | Value |
|---------|-------|
| Agent | Qwen Code (Alibaba Group) v0.26.4 — interactive CLI software-engineering agent |
| Underlying model | DeepSeek-V4-Flash-DSpark (model identity `DeepSeek-V4-Flash-DSpark@1a8b8d87`), served through the Qwen Code agent |
| Inputs analysed | `R/rawrr.R` (all `system2()` / `.rawrrSystem2Source()` calls), `inst/rawrrassembly/rawrr.cs` (`Program.Main` dispatch), compiled `rawrr.exe` (v1.17.2) |
| Reference data | `inst/extdata/sample.raw` (demo file, 574 FTMS scans of a Q Exactive HF-X) |
| Generation method | Reverse engineering: mapped every R wrapper invocation to its C# mode and argument layout; examples produced by **actually executing** the binary against `sample.raw` (captured output, not hallucinated) |
| Verification | Each example re-run via `quarto render`; rendered HTML checked for expected live output (`1.17.2`, index header, scan filters, etc.) |
| Runtime environment | Linux (x86-64), R 4.6.1, Quarto 1.4.554 |
| Style constraints | Command-line (Linux) workflow, reproducible output, all CLI behavior cross-checked against both the R wrapper and the C# source |

**Important:** because the embedded live output and the built-in `help` text
depend on the exact binary version, re-render the document with the matching
build (see the *Setup* chunk) whenever `rawrr.exe` changes.
