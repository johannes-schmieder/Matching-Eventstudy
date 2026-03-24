# Stata Code Style Guide

This guide describes the coding style for Stata do-files in this project. Follow these conventions to maintain consistency.

## Overall File Structure

```stata
/*---------------------------------------------------------*/
*! Project Title
*! Author: Name1, Name2
* filename.do
/*---------------------------------------------------------*/
set more off
program drop _all
graph drop _all
clear

set seed 190424
set linesize 100

/*===========================================================================================*/
/*                                      Main Program                                         */
/*===========================================================================================*/
capture program drop main
program define main
    // Global paths
    global log ./log/
    global data ./data/
    cap mkdir $log
    cap mkdir $data

    // =====================================
    // Settings
    // =====================================
    local setting1 value1
    local setting2 value2

    // =====================================
    // Pipeline Steps
    // =====================================

    // Step 1: Description
    step1_program, in(${data}input.dta) out(${data}output.dta)

    // Step 2: Description
    step2_program, in(${data}output.dta) logfile(${log}results)

end // main program

/*===========================================================================================*/
/*                                    Sub Programs                                           */
/*===========================================================================================*/

/*---------------------------------------------*/
/* Step 1: Short Description */
/*---------------------------------------------*/
cap program drop step1_program
program define step1_program
    syntax, in(str) out(str) [optional_param(real 0.5)]

    use `in', clear
    // ... processing ...
    save `out', replace

end // step1_program

main  // Call main at end of file
```

## Key Principles

1. **Main program calls sub-programs**: Pipeline is visible at a glance in `main`
2. **Sub-programs use `syntax`**: Standardized argument parsing with required/optional params
3. **Clear section markers**: Use `/*===...===*/` for major sections, `/*---...---*/` for subsections
4. **End comments**: Mark program ends with `end // program_name`
5. **Variable labels**: Always label variables with `label var varname "Description"`

## Program Syntax Pattern

```stata
cap program drop my_program
program define my_program
    syntax, in(str) out(str) ///   /* required string params */
        [optional_str(str) ///      /* optional string */
         optional_num(real 0.5) /// /* optional number with default */
         flag ]                     /* optional flag (no value) */

    if "`optional_str'" == "" local optional_str default_value

    use `in', clear
    // ... processing ...
    save `out', replace

end // my_program
```

---

## latexlog Usage

`latexlog` generates LaTeX files and compiles them to PDF. See `docs/latexlog.sthlp` for full documentation.

Basic workflow:

```stata
local file ${log}myresults.tex

// Open document
latexlog `file': open
latexlog `file': title "Document Title"

// Add content (see below)
// ...

// Close and compile
latexlog `file': close
latexlog `file': pdf, view   // compile and open PDF viewer
```

### Sections and Text

```stata
latexlog `file': section "Section Name"
latexlog `file': subsection "Subsection Name"
latexlog `file': writeln "Raw LaTeX text here."
latexlog `file': writeln "\begin{equation} y = x^2 \end{equation}"
```

### Documenting Analysis: Equations Before Results

Explain the model mathematically before showing results:

```stata
latexlog `file': writeln "Let $ I_{it} = e_{it} W_{it}$ where $e$ is employment."
latexlog `file': writeln "The earnings gap decomposes as:"
latexlog `file': writeln "\begin{equation}"
latexlog `file': writeln "  \log I^B - \log I^W = "
latexlog `file': writeln "  \underbrace{(\log(1-u^B) - \log(1-u^W))}_{\text{employment}} + "
latexlog `file': writeln "  \underbrace{(\log W^B - \log W^W)}_{\text{wage}}"
latexlog `file': writeln "\end{equation}"

// Then show the figure/table implementing the decomposition
twoway (line earnings_gap time) (line wage_gap time) (line employment_gap time)
latexlog `file': addfig, file(figs/decomp.pdf) title("Earnings Gap Decomposition") float
```

### Adding Figures

Single figure:
```stata
twoway scatter y x
latexlog `file': addfig, file(figs/myfig.pdf) eol title("Figure Title") float
```

Subfigure environment (multiple figures in grid):
```stata
latexlog `file': subfigure, open title("Panel Title")

cellgraph var1, by(group)  // cellgraph auto-exports; no graph export needed
latexlog `file': subfigure, addfig file(figs/var1_by_group.pdf) ///
    caption("Panel A") width(0.45)

cellgraph var2, by(group)
latexlog `file': subfigure, addfig file(figs/var2_by_group.pdf) ///
    caption("Panel B") width(0.45) eol  // eol = end of line (new row)

latexlog `file': subfigure, close notes("Figure notes go here.")
```

### Exporting Tables from `table` and `collect`

```stata
// Create table
table (var) (time), ///
    statistic(mean myvar) ///
    statistic(sd myvar) ///
    statistic(count myvar) ///
    nformat(%12.1f)

// Style the collection
collect style header result, level(hide)
collect style cell result[sd], sformat("[%s]")
collect style cell result[count], nformat("%12.0gc")
collect style cell result, halign(center)

// Preview and export
collect preview
latexlog `file': collect export, booktabs title("Table Title")
```

### Manual LaTeX Tables

```stata
latexlog `file': writeln "\begin{table}[htbp]"
latexlog `file': writeln "\centering"
latexlog `file': writeln "\caption{My Table}"
latexlog `file': writeln "\begin{tabular}{lcc}"
latexlog `file': writeln "\toprule"
latexlog `file': writeln "Variable & Value 1 & Value 2 \\\\"
latexlog `file': writeln "\midrule"
latexlog `file': writeln "Row 1 & `=string(val1, "%9.3f")' & `=string(val2, "%9.3f")' \\\\"
latexlog `file': writeln "\bottomrule"
latexlog `file': writeln "\end{tabular}"
latexlog `file': writeln "\end{table}"
```

### Preamble Options

```stata
latexlog `file': open, predocopen("\usepackage{amsmath,amssymb}")
latexlog `file': writeln "\newcommand{\var}{\mathbb{V}\text{ar}}"
```

---

## Stata `table` Command

The modern `table` command with `collect` framework:

### Basic Summary Statistics

```stata
local vars age income educ

table (var) (time) if year >= 2010, ///
    statistic(mean `vars') ///
    statistic(sd `vars') ///
    statistic(count N) ///
    nformat(%12.1f) nototals
```

### By Groups

```stata
table (var) (black), ///
    statistic(mean income educ) ///
    statistic(sd income educ)
```

### Percentiles

```stata
table (var), ///
    statistic(min myvar) ///
    statistic(p25 myvar) ///
    statistic(p50 myvar) ///
    statistic(p75 myvar) ///
    statistic(max myvar) ///
    statistic(mean myvar) ///
    nformat(%12.1fc)

collect label levels result min "Min", modify
collect label levels result p25 "25th pct", modify
```

### Cross-tabulations

```stata
table (industry) (time), ///
    statistic(percent, across(industry)) ///
    nformat(%9.1f) ///
    totals(time)
```

### Styling

```stata
// Hide result level labels
collect style header result, level(hide)

// Format specific results
collect style cell result[sd], sformat("[%s]")
collect style cell result[count], nformat("%12.0gc")

// Alignment and borders
collect style cell result, halign(center)
collect style cell var[N], border(top)

// Rename labels
collect label levels var N_firm "Number of Firms", modify
collect label levels black 0 "White" 1 "Black", modify

// Conditional formatting
foreach var of varlist `vars' {
    qui sum `var'
    if abs(r(mean)) < 10 {
        collect style cell var[`var'], nformat(%12.2f)
    }
}
```

---

## cellgraph Usage

`cellgraph` creates descriptive plots with automatic binning. It **auto-exports** the graph to PDF—no separate `graph export` command needed. See `docs/cellgraph.sthlp` for full documentation.

### Basic Usage

```stata
cellgraph income, by(year)                    // mean by year
cellgraph income, by(year) stat(mean p25 p50 p75)  // multiple statistics
cellgraph income, by(year black)              // by two groups
```

### Binscatter

```stata
cellgraph y, by(x) binscatter(20)             // 20 bins
cellgraph y, by(x) binscatter(20) scatter     // scatter points
cellgraph y, by(x black) binscatter(20) scatter lfit  // with linear fit
```

### Common Options

```stata
cellgraph y, by(x) ///
    stat(mean)           /// statistic to plot
    binscatter(20)       /// number of bins
    scatter              /// show points
    lfit                 /// add linear fit
    coef                 /// show regression coefficient
    noci                 /// no confidence interval
    notitle              /// suppress auto title
    mcounts              /// show observation counts
    legend(off)          /// turn off legend
    xscale(log)          /// log x-axis
    xlabel(1 10 100 1000)

// Multiple variables
cellgraph var1 var2 var3, by(time) line lpattern
```

### Color Specification

```stata
cellgraph y, by(x group) ///
    colors(dknavy; cranberry; dknavy%60; cranberry%60)
```

---

## Common Patterns

### Preserve/Restore

```stata
preserve
    collapse (mean) y, by(x)
    // ... analysis on collapsed data ...
restore
```

### Temporary Files

```stata
tempfile mytemp
save `mytemp', replace
// ... later ...
use `mytemp', clear
```

### Loop with Locals

```stata
local vars var1 var2 var3
foreach var of local vars {
    cellgraph `var', by(time)
    latexlog `file': subfigure, addfig file(figs/`var'.pdf) width(0.3)
}
```

### Formatted Values in LaTeX

```stata
sum myvar
local mean_fmt : display %9.3f r(mean)
latexlog `file': writeln "The mean is `mean_fmt'."

// Or inline:
latexlog `file': writeln "Value: `=string(myvar, "%9.2f")'"
```

### Assertions for Data Validation

```stata
assert income >= 0 if employed == 1
assert missing(wage) if employed == 0
```

---

## Naming Conventions

- **Variables**: lowercase with underscores (`log_daily_wage`, `firm_effect_EU`)
- **Programs**: lowercase with underscores (`describe_person_panel`)
- **Locals**: lowercase with underscores (`local file_stub`)
- **Globals**: uppercase (`$log`, `$data`)
- **Temporary variables**: use `tempvar` or prefix with underscore

## Comment Style

```stata
// Single line comment
/* Multi-line
   comment */

// Inline after code
gen x = y + z  // explanation

// Section headers in code
// =====================================
// Section Name
// =====================================

// Numbered steps
*******************************************************
* Step 1: Description                                  *
*******************************************************
```
