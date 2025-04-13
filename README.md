
# `patter.workflows`

**Iterative animal-tracking workflows**

[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/patter)](https://CRAN.R-project.org/package=patter)

`patter.workflows` provides iterative workflows for animal-tracking
analyses, with a focus on the
[`patter`](https://github.com/edwardlavender/patter) package. This is
built around the
[`proj.verse`](https://github.com/edwardlavender/proj.verse). The core
routines provide an iterative framework for repeated applications of
algorithms that estimate the positions of tagged individuals
(coordinates) or patterns of space use. This facilitates analyses of
multiple individuals, time blocks and algorithm sensitivity (i.e.,
multiple parameters). Routines handle the iteration, parallelisation and
errors, and record both algorithm outputs and call statistics (such as
computation time). Outputs can be saved in memory or written to disk,
depending on memory requirements. These routines were motivated by the
iterative analyses in Lavender et al. (2024a) and (2025).

> **Note:** `patter.workflows` is a new `R` package. Like all new
> packages, you should use it with a degree of caution. Please share
> feedback and issues.

# Installation

1.  **Install [`patter`](https://github.com/edwardlavender/patter)** and
    its dependencies by following the instructions
    [here](https://github.com/edwardlavender/patter).

2.  **Install `patter.workflows`** with:

``` r
install.packages(c("devtools", "rmarkdown"))
devtools::install_github("edwardlavender/patter.workflows", 
                         dependencies = TRUE, 
                         build_vignettes = rmarkdown::pandoc_available())
```

3.  **For examples**, see `?example-patter.workflows`.

# Functionality

## Coordinate estimation

**`patter.workflows` leverages `proj.lapply::cl_lapply_workflow()`** to
implement `algorithm` workflows iteratively. To implement this function,
provide an `algorithm_*()` function and a `constructor_*()` function
that constructs a named list of arguments for `algorithm_*()`.

The following `algorithm` functions are provided:

- Use `algorithm_coa()` to implement the COA algorithm. This wraps
  `patter::coa()`;
- Use `algorithm_rsp()` to implement the RSP algorithm. This wraps
  `RSP::runRSP()`;
- Use `algorithm_particle()` to implement a particle algorithm. This
  wraps `patter::pf_filter()` and `pf_smoother_two_filter()`;

These functions are supported by in-built `constructor_*()` functions:

- For `algorithm_coa()`, use `constructor_coa()`;
- For `algorithm_rsp()`, use `constructor_rsp()`;
- For `algorithm_particle()`, a custom `constructor_*()` function is
  required;

For custom `constructor_*()` functions, ancillary helpers are available:

- See `get_input` functions (e.g., `get_dataset_detections()`) to get
  algorithm inputs;

Interoperability between `patter.workflow` structures and `trackyverse`
packages is provided by `as_*()` functions:

- `as_actel()` converts
  [`patter`](https://github.com/edwardlavender/patter) detection
  datasets to [`actel`](https://github.com/hugomflavio/actel) format;
- `as_glatos()` converts
  [`patter`](https://github.com/edwardlavender/patter) detection
  datasets to
  [`glatos`](https://github.com/ocean-tracking-network/glatos) format;

## Mapping

TO DO

## Visualisation

`patter.workflow` makes additional routines for plotting available.

`lapply_qplot_*()` functions are ‘quick’ plotting routines:

- Use `lapply_qplot_coord()` to plot estimates coordinates for multiple
  units (e.g., individuals);
- Use `lapply_qplot_ud()` to plot estimated utilisation distributions
  for multiple units;

`gg*()` functions provide more sophisticated `ggplot2` wrappers:

- Use `ggmaps()` to map patterns of space use for multiple units;

# Examples

For package examples, see `?patter.workflows`.

# Disclaimer and troubleshooting

`patter.workflows` is a new `R` package. All routines are experimental.
Researchers interested in using the package are encouraged to get in
touch while the methods and package remain at an early stage of
evolution (<edward.lavender@eawag.ch>).

# Citation

**To cite `patter.workflows` in publications**, please follow the
guidance for [`patter`](https://github.com/edwardlavender/patter).

**Thank you for citing the package.** Your citations help to justify
continued investments in its development.

## Code of Conduct

Please note that the patter.workflows project is released with a
[Contributor Code of
Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
