# Development version


# v0.1.5 (2021-12-10)

* added a vignette showing the example Earth Engine scripts (#15)
* updated the getting started vignette (#14)


# v 0.1.4 (2021-12-01)

* add dependency on `chk` package to improve checks, and returned errors
* use new example data in tests and examples

# v 0.1.3 (2021-12-01)

* add new function `use_example_ee_script` to provide users with an example
script for sampling NDVI with Earth Engine [#10]
* added new example data based on the exports from the example scripts [#11]

# v 0.1.2 (2021-11-23)

* switched to a pkgdown site with gh actions [#1]
* switched to R CMD check with gh actions [#2]
* switch testing to `tinytest` [#3]
* updated links throughout, made a redirect page for old site
* fix minor bug to allow flexible column names in `filter_qa` [#4], `model_params` [#6]
* added testing for flexible column names [#5] [#7]


# v 0.1.1 (2019-02-13)
* fixed data.table recycling change ([#23](https://gitlab.com/robit.a/irg/-/issues/23)
* minor edits to 'Getting Started' vignette

# v 0.1.0 (2019-01-04)
## Initial release

One vignette ('Getting started with irg') and: 

Five filtering functions:
* `filter_qa`
* `filter_winter`
* `filter_roll`
* `filter_top`

Two scaling functions:
* `scale_doy`
* `scale_ndvi`

Three modeling functions:
* `model_start`
* `model_params`
* `model_ndvi`

One instantaneous rate of green-up function:
* `calc_irg`

Two meta functions:
* `filter_ndvi`
* `irg`
