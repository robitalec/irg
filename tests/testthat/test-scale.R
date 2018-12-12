context("test-scale")

ndvi <- fread(system.file("extdata", "ndvi.csv", package = "irg"))



### scale_ndvi ###########################################
test_that("scale_ndvi works", {

	# Columns mising are detected
	expect_error(scale_ndvi(ndvi),
							 'missing one of "rolled", "winter", "top". did you filter?')

	filter_ndvi(ndvi)

	expect_true("winter" %in% colnames(ndvi))
	expect_true("rolled" %in% colnames(ndvi))
	expect_true("top" %in% colnames(ndvi))

	# Was scaled added
	scale_ndvi(ndvi)
	expect_true("scaled" %in% colnames(ndvi))

	# Did it rescale to 0-1?
	expect_lte(max(ndvi$scaled, na.rm = TRUE), 1)
	expect_gte(min(ndvi$scaled, na.rm = TRUE), 0)
})

### scale_doy ###########################################
test_that("scale_doy works", {

	# Columns mising are detected
	expect_error(scale_doy(ndvi, doy = 'potato'),
							 'column not found in DT')

	# Was t added
	expect_true("t" %in% colnames(scale_doy(ndvi)))

	# Did it rescale to 0-1?
	expect_lte(max(ndvi$t), 1)
	expect_gte(min(ndvi$t), 0)
})
