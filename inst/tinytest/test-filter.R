library(data.table)
library(irg)

ndvi <- fread(system.file("extdata", "sampled-ndvi-MODIS-MOD13Q1.csv", package = "irg"))

# filter_qa ---------------------------------------------------------------
# Columns mising are detected
miss <- copy(ndvi)[, NDVI := NULL]
expect_error(filter_qa(miss),
						 'must include', fixed = FALSE)

expect_error(filter_qa(ndvi, qa = 'potato'),
						 'must include', fixed = FALSE)

# qa length 1
expect_error(filter_qa(ndvi, qa = c('a', 'b')),
						 'must include', fixed = FALSE)


# check type of NDVI
ndvi_char <- copy(ndvi)[, NDVI := as.character(NDVI)]
expect_error(filter_qa(ndvi_char),
						 'must be numeric', fixed = FALSE)

#
ndviqa <- filter_qa(copy(ndvi))


# filter_winter -----------------------------------------------------------
# Columns mising are detected
expect_error(filter_winter(ndvi),
						 'must include', fixed = FALSE)

expect_error(filter_winter(ndviqa, doy = 'potato'),
						 'must include', fixed = FALSE)

expect_error(filter_winter(ndviqa, id = 'potato'),
						 'must include', fixed = FALSE)

# Probs length 1
expect_error(filter_winter(ndvi, probs = c(0.2, 0.3)),
						 'must be length 1', fixed = FALSE)

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
						 'must include', fixed = FALSE)

expect_error(filter_roll(ndviqa),
						 'must include', fixed = FALSE)

expect_error(filter_roll(ndviwnt, id = 'potato'),
						 'must include', fixed = FALSE)

# Overwrite roll column
expect_warning(filter_roll(copy(ndviwnt)[, rolled := 1]),
							 'overwriting rolled column')

ndviroll <- filter_roll(ndviwnt)


# filter_top --------------------------------------------------------------
# Columns mising are detected
expect_error(filter_top(ndvi),
						 'must include', fixed = FALSE)

expect_error(filter_top(ndviqa),
						 'must include', fixed = FALSE)

expect_error(filter_top(ndviwnt, id = 'potato'),
						 'must include', fixed = FALSE)

# Probs length 1
expect_error(filter_top(ndviwnt, probs = c(0.2, 0.3)),
						 'must be length', fixed = FALSE)

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
