library(data.table)
library(irg)

ndvi <- fread(system.file("extdata", "sampled-ndvi-MODIS-MOD13Q1.csv", package = "irg"))

# filter_qa ---------------------------------------------------------------
# Columns mising are detected
miss <- copy(ndvi)[, NDVI := NULL]
expect_error(filter_qa(miss),
						 'column not found in DT')

expect_error(filter_qa(ndvi, qa = 'potato'),
						 'column not found in DT')

# qa length 1
expect_error(filter_qa(ndvi, qa = c('a', 'b')))


# check type of NDVI
ndvi_int <- copy(ndvi)[, NDVI := as.numeric(NDVI)]
expect_warning(filter_qa(ndvi_int), 'column as integer', fixed = FALSE)

#
ndviqa <- filter_qa(copy(ndvi))


# filter_winter -----------------------------------------------------------
# Columns mising are detected
expect_error(filter_winter(ndvi),
						 'column not found in DT')

expect_error(filter_winter(ndviqa, doy = 'potato'),
						 'column not found in DT')

expect_error(filter_winter(ndviqa, id = 'potato'),
						 'column not found in DT')

# Probs length 1
expect_error(filter_winter(ndvi, probs = c(0.2, 0.3)),
						 'probs must be length 1')

# Overwrite winter column
expect_warning(filter_winter(copy(ndviqa)[, winter := 1]),
						 'overwriting winter column')

# Cast to integer silently
ndviqa[, DayOfYear := as.numeric(DayOfYear)]
expect_silent(filter_winter(copy(ndviqa), limits = c(10.5, 100.2)))

# sel one id, if above quantile,

ndviwnt <- filter_winter(copy(ndviqa))


# filter_roll -------------------------------------------------------------
# Columns mising are detected
expect_error(filter_roll(ndvi),
						 'filtered column not found in DT, did you run filter_qa?')

expect_error(filter_roll(ndviqa),
						 'winter column not found in DT, did you run filter_winter?')

expect_error(filter_roll(ndviwnt, id = 'potato'),
						 'column not found in DT')

# Overwrite roll column
expect_warning(filter_roll(copy(ndviwnt)[, rolled := 1]),
							 'overwriting rolled column')

ndviroll <- filter_roll(ndviwnt)


# filter_top --------------------------------------------------------------
# Columns mising are detected
expect_error(filter_top(ndvi),
						 'filtered column not found in DT, did you run filter_qa?')

expect_error(filter_top(ndviqa),
						 'winter column not found in DT, did you run filter_winter?')

expect_error(filter_top(ndviwnt, id = 'potato'),
						 'column not found in DT')

# Probs length 1
expect_error(filter_top(ndviwnt, probs = c(0.2, 0.3)),
						 'probs must be length 1')

# Overwrite top column
expect_warning(filter_top(copy(ndviroll)[, top := 1]),
							 'overwriting top column')


# filter_ndvi -------------------------------------------------------------
# Overwrite top column
expect_warning(filter_ndvi(copy(ndvi)[, filtered := 1]),
							 'overwriting filtered column')

expect_warning(filter_ndvi(copy(ndvi)[, winter := 1]),
							 'overwriting winter column')

expect_warning(filter_ndvi(copy(ndvi)[, rolled := 1]),
							 'overwriting rolled column')

expect_warning(filter_ndvi(copy(ndvi)[, top := 1]),
							 'overwriting top column')
