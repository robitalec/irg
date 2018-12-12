context("test-internal")


ndvi <- fread(system.file("extdata", "ndvi.csv", package = "irg"))


test_that("check_col works", {
	expect_error(check_col(ndvi, 'potato'),
							 'potato column not found in DT')

	expect_error(check_col(ndvi, 'potato', arg = 'cheese'),
							 "column not found in DT",
							 fixed = FALSE)

})

test_that("check_type works", {
	expect_error(
		check_type(ndvi, 'DayOfYear', 'potato'),
		'DayOfYear does not match required type: potato'
	)

})


test_that("overwrite_col works", {
	copyNDVI <- copy(ndvi)[, potato := 'golden']
	expect_warning(overwrite_col(copyNDVI, 'potato'),
								 'overwriting potato column')

})


# test_that("check_truelength works", {
	# see #10
	# error = 'please run data.table::alloc.col on your DT to allocate columns'
# })
