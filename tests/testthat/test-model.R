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

	mods <- model_params(
		ndvi,
		xmidS = 0.44,
		xmidA = 0.80,
		scalS = 0.05,
		scalA = 0.01
	)

	expect_true(all(c('id', 'yr', 'xmidS', 'xmidA', 'scalS')
									%in%
										colnames(mods)))

})

mods <- model_params(
	ndvi,
	xmidS = 0.44,
	xmidA = 0.80,
	scalS = 0.05,
	scalA = 0.01
)

test_that("model_ndvi works", {
	copyMods <- copy(mods)[, xmidS := NULL]
	expect_error(
		model_ndvi(copyMods),
		'xmidS column not found in DT'
	)

	copyMods <- copy(mods)[, xmidA := NULL]
	expect_error(
		model_ndvi(copyMods),
		'xmidA column not found in DT'
	)

	copyMods <- copy(mods)[, scalS := NULL]
	expect_error(
		model_ndvi(copyMods),
		'scalS column not found in DT'
	)

	copyMods <- copy(mods)[, scalA := NULL]
	expect_error(
		model_ndvi(copyMods),
		'scalA column not found in DT'
	)

})
