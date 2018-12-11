context("test-irg")

ndvi <- fread(system.file("extdata", "ndvi.csv", package = "irg"))
filter_ndvi(ndvi)
scale_doy(ndvi)
scale_ndvi(ndvi)

mods <- model_params(
	ndvi,
	xmidS = 0.44,
	xmidA = 0.80,
	scalS = 0.05,
	scalA = 0.01
)

test_that("calc_irg works", {
	copyMods <- copy(mods)[, xmidS := NULL]
	expect_error(
		calc_irg(copyMods),
		'xmidS column not found in DT'
	)

	copyMods <- copy(mods)[, xmidA := NULL]
	expect_error(
		calc_irg(copyMods),
		'xmidA column not found in DT'
	)

	copyMods <- copy(mods)[, scalS := NULL]
	expect_error(
		calc_irg(copyMods),
		'scalS column not found in DT'
	)

	copyMods <- copy(mods)[, scalA := NULL]
	expect_error(
		calc_irg(copyMods),
		'scalA column not found in DT'
	)

})
