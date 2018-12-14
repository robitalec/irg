context("test-model")

ndvi <- fread(system.file("extdata", "ndvi.csv", package = "irg"))
filter_ndvi(ndvi)
scale_doy(ndvi)
scale_ndvi(ndvi)


test_that("model_start works", {
	## column checks
	copyNDVI <- copy(ndvi)[, scaled := NULL]
	expect_error(
		model_start(copyNDVI),
		'scaled column not found in DT - did you filter and scale?'
	)

	copyNDVI <- copy(ndvi)[, t := NULL]
	expect_error(
		model_start(copyNDVI),
		't column not found in DT - did you scale doy?'
	)

	copyNDVI <- copy(ndvi)[, id := NULL]
	expect_error(
		model_start(copyNDVI),
		"id \\('id'\\) column not found in DT"
	)

	copyNDVI <- copy(ndvi)[, yr := NULL]
	expect_error(
		model_start(copyNDVI),
		"year \\('yr'\\) column not found in DT"
	)

	expect_true(all(
		c('id', 'yr', 'xmidS_start', 'xmidA_start') %in%
			colnames(model_start(ndvi))
	))

})



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

	expect_true(all(c('id', 'yr', 'xmidS', 'xmidA', 'scalS', 'fitted')
									%in%
										colnames(model_ndvi(mods))))
})
