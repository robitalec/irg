context("test-filter")

ndvi <- fread(system.file("extdata", "ndvi.csv", package = "irg"))

### filter_qa ###########################################
test_that("filter qa works", {

	# Columns mising are detected
	miss <- copy(ndvi)[, NDVI := NULL]

	expect_error(filter_qa(miss),
							 'NDVI column not found in DT')

	miss <- copy(ndvi)[, SummaryQA := NULL]
	expect_error(filter_qa(miss),
							 'QA column not found in DT')

	# QA length 1
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

	miss <- copy(ndvi)[, DayOfYear := NULL]
	expect_error(filter_winter(miss),
							 'doy column not found in DT')

	miss <- copy(ndvi)[, id := NULL]
	expect_error(filter_winter(miss),
							 'id column not found in DT')

	# Probs length 1
	expect_error(filter_winter(miss, probs = c(0.2, 0.3)),
							 'probs must be length 1')

	# Overwrite winter column
	DT <- copy(ndviqa)[, winter := 1]

	expect_warning(filter_winter(DT),
							 'overwriting winter column')

	# Cast to integer silently
	expect_silent(filter_winter(copy(ndviqa), limits = c(10.5, 100.2)))

	# sel one id, if above quantile,

})

ndviwnt <- filter_qa(copy(ndviqa))


### filter_roll ########################################
test_that("filter roll works", {
	# Columns mising are detected
	expect_error(filter_roll(ndvi),
							 'filtered column not found in DT, did you run filter_qa?')

	expect_error(filter_roll(ndviqa),
							 'winter column not found in DT, did you run filter_winter?')

	miss <- copy(ndviwnt)[, id := NULL]
	expect_error(filter_roll(miss),
							 'id column not found in DT')
})
