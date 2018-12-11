context("test-model")

ndvi <- fread(system.file("extdata", "ndvi.csv", package = "irg"))
filter_ndvi(ndvi)
scale_doy(ndvi)
scale_ndvi(ndvi)

test_that("model_params works", {
	expect_error(
		model_params(ndvi),
		'xmidS column not found in DT'
	)

	expect_error(
		model_params(ndvi, xmidS = 0.5),
		'xmidA column not found in DT'
	)

	expect_error(
		model_params(ndvi, xmidS = 0.5, xmidA = 0.5),
		'scalS column not found in DT'
	)

	expect_error(
		model_params(ndvi, xmidS = 0.5, xmidA = 0.5, scalS = 0.5),
		'scalA column not found in DT'
	)



})
