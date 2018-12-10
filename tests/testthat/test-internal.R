context("test-internal")

test_that("check_col works", {
	stop(paste0(it, ' column not found in DT', extra))

})

test_that("check_type works", {
	stop(paste0(col, ' does not match required type: ', type))

})


test_that("overwrite_col works", {
	# warning(paste0('overwriting ', col, ' column'))

})






test_that("check_truelength works", {

	# see #10
	# error = 'please run data.table::alloc.col on your DT to allocate columns'
})
