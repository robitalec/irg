context("test-irg")

ndvi <- fread(system.file("extdata", "ndvi.csv", package = "irg"))
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

test_that("calc_irg works", {
	copyNDVI <- copy(ndvi)[, xmidS := NULL]
	expect_error(
		calc_irg(copyNDVI),
		'xmidS column not found in DT'
	)

	copyNDVI <- copy(ndvi)[, scalS := NULL]
	expect_error(
		calc_irg(copyNDVI),
		'scalS column not found in DT'
	)

	copyNDVI <- calc_irg(na.omit(copy(ndvi)))
	expect_true(all(c('id', 'yr', 'xmidS', 'scalS', 't', 'irg')
		%in% colnames(copyNDVI)))
})
