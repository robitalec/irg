library(data.table)

ndvi <- fread(system.file("extdata", "sampled-ndvi-MODIS-MOD13Q1.csv", package = "irg"))

# check_col ---------------------------------------------------------------
expect_error(
	irg:::check_col(ndvi, 'potato'),
	'potato column not found in DT'
)

expect_error(
	irg:::check_col(ndvi, 'potato', arg = 'cheese'),
	"column not found in DT",
	fixed = FALSE
)

# check_type --------------------------------------------------------------
expect_error(
	irg:::check_type(ndvi, 'DayOfYear', 'potato'),
	'DayOfYear does not match required type: potato'
)

# overwrite_col -----------------------------------------------------------
copyNDVI <- copy(ndvi)[, potato := 'golden']
expect_warning(
	irg:::overwrite_col(copyNDVI, 'potato'),
	'overwriting potato column'
)
