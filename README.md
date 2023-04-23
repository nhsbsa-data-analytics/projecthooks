
# projecthooks

_Project hooks to pass to `{golem}` to standardise and automate dashboard creation_

`projecthooks` provides a way to create templates for `{shiny}` apps. This makes it easy to create apps in exactly the same way and automates almost all of the work in creating a new app.

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![pre-commit](https://img.shields.io/badge/pre--commit-enabled-brightgreen?logo=pre-commit&logoColor=white)](https://github.com/pre-commit/pre-commit)
[![R-CMD-check](https://github.com/nhsbsa-data-analytics/projecthooks/workflows/R-CMD-check/badge.svg)](https://github.com/nhsbsa-data-analytics/projecthooks/actions)
<!-- badges: end -->

The goal of projecthooks is to ...

## Installation

You can install the `{projecthooks}` from this repository with:

``` r
# install.packages("devtools")
devtools::install_github("nhsbsa-data-analytics/projecthooks")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
devpacker::create_shiny("my_new_app", git_only = T, project_hook = projecthooks::mi_dash)
```
