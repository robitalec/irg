context("test-filter")

ndvi <- fread(system.file("extdata", "ndvi.csv", package = "irg"))

#TODO: add test alloc.col?


### filter_qa ###########################################
test_that("filter qa works", {

	# Columns mising are detected
	miss <- copy(ndvi)[, NDVI := NULL]
	expect_error(filter_qa(miss),
							 'NDVI column not found in DT')

	expect_error(filter_qa(ndvi, qa = 'potato'),
							 'qa column not found in DT')

	# qa length 1
	expect_error(filter_qa(ndvi, qa = c('a', 'b')))

	# if qa != 0 and != 1, filtered is NULL
	# if qa = 0 or  = 1, filtered is not NULL
})

ndviqa <- filter_qa(copy(ndvi))

### filter_winter ######################################
test_that("filter winter works", {

	# Columns mising are detected
	expect_error(filter_winter(ndvi),
							 'filtered column not found in DT')

	expect_error(filter_winter(ndviqa, doy = 'potato'),
							 'doy column not found in DT')

	expect_error(filter_winter(ndviqa, id = 'potato'),
							 'id column not found in DT')

	# Probs length 1
	expect_error(filter_winter(ndvi, probs = c(0.2, 0.3)),
							 'probs must be length 1')

	# Overwrite winter column
	expect_warning(filter_winter(copy(ndviqa)[, winter := 1]),
							 'overwriting winter column')

	# Cast to integer silently
	expect_silent(filter_winter(copy(ndviqa), limits = c(10.5, 100.2)))

	# sel one id, if above quantile,

})

ndviwnt <- filter_winter(copy(ndviqa))


### filter_roll ########################################
test_that("filter roll works", {
	# Columns mising are detected
	expect_error(filter_roll(ndvi),
							 'filtered column not found in DT, did you run filter_qa?')

	expect_error(filter_roll(ndviqa),
							 'winter column not found in DT, did you run filter_winter?')

	expect_error(filter_roll(ndviwnt, id = 'potato'),
							 'id column not found in DT')

	# Overwrite roll column
	expect_warning(filter_roll(copy(ndviwnt)[, rolled := 1]),
								 'overwriting rolled column')

})


ndviroll <- filter_roll(ndviwnt)

### filter_top ########################################
test_that("filter top works", {
	# Columns mising are detected
	expect_error(filter_top(ndvi),
							 'filtered column not found in DT, did you run filter_qa?')

	expect_error(filter_top(ndviqa),
							 'winter column not found in DT, did you run filter_winter?')

	expect_error(filter_top(ndviwnt, id = 'potato'),
							 'id column not found in DT')

	# Probs length 1
	expect_error(filter_top(ndviwnt, probs = c(0.2, 0.3)),
							 'probs must be length 1')

	# Overwrite top column
	expect_warning(filter_top(copy(ndviroll)[, top := 1]),
								 'overwriting top column')

})

### filter_ndvi #######################################
test_that("filter ndvi meta works", {
	# Overwrite top column
	expect_warning(filter_ndvi(copy(ndvi)[, filtered := 1]),
								 'overwriting filtered column')

	expect_warning(filter_ndvi(copy(ndvi)[, winter := 1]),
								 'overwriting winter column')

	expect_warning(filter_ndvi(copy(ndvi)[, rolled := 1]),
								 'overwriting rolled column')

	expect_warning(filter_ndvi(copy(ndvi)[, top := 1]),
								 'overwriting top column')

})
