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
})

### scale_doy ###########################################
test_that("scale_doy works", {

	# Columns mising are detected
	expect_error(scale_doy(ndvi, doy = 'potato'),
							 'doy column not found in DT')


	expect_true("t" %in% colnames(scale_doy(ndvi)))


})
