library(data.table)
library(irg)

ndvi <- fread(system.file("extdata", "sampled-ndvi-MODIS-MOD13Q1.csv", package = "irg"))
ndvi_raw <- copy(ndvi)

filter_ndvi(ndvi)
scale_doy(ndvi)
scale_ndvi(ndvi)

model_params(
	ndvi,
	returns = 'columns',
	xmidS = 0.44,
	xmidA = 0.80,
	scalS = 0.05,
	scalA = 0.01
)

model_ndvi(ndvi, observed = TRUE)

# calc_irg ----------------------------------------------------------------
copyNDVI <- copy(ndvi)[, xmidS := NULL]
expect_error(calc_irg(copyNDVI),
						 'must include', fixed = FALSE)

copyNDVI <- copy(ndvi)[, scalS := NULL]
expect_error(calc_irg(copyNDVI),
						 'must include', fixed = FALSE)

copyNDVI <- calc_irg(na.omit(copy(ndvi)))
expect_true(all(
	c('id', 'yr', 'xmidS', 'scalS', 't', 'irg') %in% colnames(copyNDVI)
))



# irg ---------------------------------------------------------------------
out <- irg(copy(ndvi_raw))
check_names <- c('irg', 'fitted', 'scaled', 't')

expect_true(all(check_names %in% colnames(out)))
