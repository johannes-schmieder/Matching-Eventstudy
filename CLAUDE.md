# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Example implementation of a **matching event study design** for analyzing the impact of job displacement on worker earnings/employment. Simulates worker-firm panel data, performs propensity score matching within cells, then estimates event study regressions. Based on Schmieder, von Wachter, and Heining (2023, AER).

## Running the Code

The entire pipeline runs from a single Stata command:
```stata
do do/matching_eventstudy_example.do
```

**Required Stata packages** (not bundled):
- [cellgraph](https://github.com/johannes-schmieder/cellgraph) — grouped visualizations
- [latexlog](https://github.com/johannes-schmieder/latexlog) — LaTeX report generation

**Bundled ado files** in `do/ado/`: `psmatch2.ado` (propensity score matching), `writeln.ado`, `SignificantDigits.ado`.

## Architecture & Data Flow

All logic lives in `do/matching_eventstudy_example.do`, organized as Stata programs called sequentially from `main`:

1. **`simulate_yearly_data`** — Generates synthetic person-year panel (100 firms, 20K workers, 1990–2005). Creates displacement events (plant closings + mass layoffs) and wage dynamics. Outputs `data/person_year_data.dta`.

2. **`generate_treatment_control`** — Core matching algorithm. Loops over baseline years × cells (industry × gender). Within each cell: applies sample restrictions (employed, firm size ≥50, tenure ≥3, age 20–55), runs `psmatch2` 1:1 without replacement on lagged earnings/age/education/tenure/firm size. Creates balanced panels with relative time window [−5, +10]. Stacks all sub-experiments with unique person IDs across cells (`psmatchid`). Outputs `data/analysis_data.dta`.

3. **`descriptive_analysis`** — Summary statistics, industry distributions, consistency checks, raw means by event time. Outputs to `log/descriptive_analysis.tex` and `log/Consistency/`, `log/Disp_event_raw/`.

4. **`eventstudy_analysis`** — Estimates four event study specifications (OLS, JLS with year+person FE, relative year with person FE, full SWH with all interactions) plus employment model. Uses `eventstudy_figure` helper for plotting. Outputs to `log/eventstudy_analysis.tex` and `log/Eventstudy/`.

## Key Design Decisions

- Workers can appear in multiple cells/sub-experiments — the generated `psmatchid` (not original `personid`) is the correct ID for fixed effects.
- Matching is exact on cell dimensions (industry × gender × year), propensity-score-based within cells.
- `tsfill` ensures balanced panels for all matched pairs.
- Output goes to `log/` subdirectories; `.dta` and `.log` files are gitignored.
