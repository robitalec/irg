context("test-filter")

ndvi <- fread(system.file("extdata", "ndvi.csv", package = "irg"))

test_that("filter qa works", {

	miss <- copy(ndvi)[, 'NDVI' := NULL]

	expect_error(filter_qa(miss),
							 'NDVI column not found in DT')

	miss <- copy(ndvi)[, 'SummaryQA' := NULL]
	expect_error(filter_qa(miss),
							 'QA column not found in DT')

})
