library(data.table)
library(irg)

ndvi <- fread(system.file("extdata", "ndvi.csv", package = "irg"))[yr < 2007]


# Test column names are flexible
ndvi_new_names <- copy(ndvi)
old_names <- colnames(ndvi_new_names)
setnames(ndvi_new_names,
				 old_names,
				 c('identity', 'year', 'day_of_year', 'ndvi', 'qa'))
ndvi_new_names_raw <- copy(ndvi_new_names)

# Filter QA
ndvi_new_names <- copy(ndvi_new_names_raw)
expect_equal(nrow(filter_qa(ndvi_new_names, ndvi = 'ndvi', qa = 'qa')),
						 nrow(ndvi_new_names))

# Filter winter
ndvi_new_names <- copy(ndvi_new_names_raw)
expect_equal(nrow({
	filter_qa(ndvi_new_names, ndvi = 'ndvi', qa = 'qa')
	filter_winter(ndvi_new_names, probs = 0.025, limits = c(60L, 300L),
								doy = 'day_of_year', id = 'identity')
	}),
	nrow(ndvi_new_names))

# Filter roll
ndvi_new_names <- copy(ndvi_new_names_raw)
expect_equal(nrow({
	filter_qa(ndvi_new_names, ndvi = 'ndvi', qa = 'qa')
	filter_winter(ndvi_new_names, probs = 0.025, limits = c(60L, 300L),
								doy = 'day_of_year', id = 'identity')
	}),
	nrow(ndvi_new_names))

# Filter top
ndvi_new_names <- copy(ndvi_new_names_raw)
expect_equal(nrow({
	filter_qa(ndvi_new_names, ndvi = 'ndvi', qa = 'qa', good = c(0, 1))
	filter_winter(ndvi_new_names, probs = 0.025, limits = c(60L, 300L),
								doy = 'day_of_year', id = 'identity')
	filter_roll(ndvi_new_names, window = 3L, id = 'identity')
	filter_top(ndvi_new_names, probs = 0.925, id = 'identity')
	}),
	nrow(ndvi_new_names)
)

# Scale
ndvi_new_names <- copy(ndvi_new_names_raw)
expect_equal(nrow(scale_doy(ndvi_new_names, doy = 'day_of_year')),
						 nrow(ndvi_new_names))


# Model
ndvi_new_names <- copy(ndvi_new_names_raw)
expect_equal(nrow({
	filter_qa(ndvi_new_names, ndvi = 'ndvi', qa = 'qa', good = c(0, 1))
	filter_winter(ndvi_new_names, probs = 0.025, limits = c(60L, 300L),
								doy = 'day_of_year', id = 'identity')
	filter_roll(ndvi_new_names, window = 3L, id = 'identity')
	filter_top(ndvi_new_names, probs = 0.925, id = 'identity')
	scale_doy(ndvi, doy = 'day_of_year')
	scale_ndvi(ndvi)
	model_start(ndvi, id = 'identity', year = 'year')
	}),
	nrow(ndvi_new_names)
)

