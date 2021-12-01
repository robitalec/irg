library(data.table)

ndvi <- fread(system.file("extdata", "sampled-ndvi-MODIS-MOD13Q1.csv", package = "irg"))

# overwrite_col -----------------------------------------------------------
copyNDVI <- copy(ndvi)[, potato := 'golden']
expect_warning(
	irg:::overwrite_col(copyNDVI, 'potato'),
	'overwriting potato column'
)
