context("test-irg")

ndvi <- fread(system.file("extdata", "ndvi.csv", package = "irg"))
filter_ndvi(ndvi)
scale_doy(ndvi)
scale_ndvi(ndvi)

mods <- model_params(
	ndvi,
	returns = 'models',
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

	copyMods <- copy(mods)[, scalS := NULL]
	expect_error(
		calc_irg(copyMods),
		'scalS column not found in DT'
	)

	copyMods <- copy(mods)[!is.na(xmidS)]
	expect_true(all(
		c('id', 'yr', 'xmidS', 'scalS', 't', 'irg')
		%in%
			colnames(calc_irg(copyMods, fitted = FALSE))))

	# fitted errors
	expect_error(calc_irg(copyMods, fitted = TRUE),
							 'did not find duplicates', fixed = FALSE)

	expect_error(calc_irg(model_ndvi(copyMods), fitted = FALSE),
							 'duplicates found', fixed = FALSE)

})
