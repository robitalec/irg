# Test column names are flexible


# Packages ----------------------------------------------------------------
library(data.table)
library(irg)



# Data --------------------------------------------------------------------
ndvi <- fread(system.file("extdata", "sampled-ndvi-MODIS-MOD13Q1.csv", package = "irg"))


# Change names ------------------------------------------------------------
id <- 'identity'
yr <- 'year'
ndvi_new_names <- copy(ndvi)
old_names <- colnames(ndvi_new_names)
setnames(ndvi_new_names,
				 old_names,
				 c(id, yr, 'day_of_year', 'ndvi', 'qa'))
ndvi_new_names_raw <- copy(ndvi_new_names)



# Filter ------------------------------------------------------------------
# Filter QA
filter_qa(ndvi_new_names, ndvi = 'ndvi', qa = 'qa')
expect_equal(nrow(ndvi_new_names),
						 nrow(ndvi_new_names_raw))

# Filter winter
filter_winter(ndvi_new_names, probs = 0.025, limits = c(60L, 300L),
							doy = 'day_of_year', id = 'identity')
expect_equal(nrow(ndvi_new_names),
						 nrow(ndvi_new_names_raw))

# Filter roll
filter_roll(ndvi_new_names, window = 3L, id = 'identity')
expect_equal(nrow(ndvi_new_names),
						 nrow(ndvi_new_names_raw))

# Filter top
filter_top(ndvi_new_names, probs = 0.925, id = 'identity')
expect_equal(nrow(ndvi_new_names),
						 nrow(ndvi_new_names_raw))



# Scale -------------------------------------------------------------------
# Scale
scale_doy(ndvi_new_names, doy = 'day_of_year')
expect_equal(nrow(ndvi_new_names),
						 nrow(ndvi_new_names_raw))



# Model -------------------------------------------------------------------
# Model
scale_ndvi(ndvi_new_names)
model_start(ndvi_new_names, id = 'identity', year = 'year')
expect_equal(nrow(ndvi_new_names),
						 nrow(ndvi_new_names_raw))

# Option 1 - returns = 'models'
mods <- model_params(
	ndvi_new_names,
	id = 'identity',
	year = 'year',
	returns = 'models',
	xmidS = 'xmidS_start',
	xmidA = 'xmidA_start',
	scalS = 0.05,
	scalA = 0.01
)

# Fit to the whole year (requires assignment)
fit <- model_ndvi(mods, observed = FALSE)

expect_equal(colnames(fit),
						 c(id, yr, 'xmidS', 'xmidA', 'scalS', 'scalA',
						 	'nls_error', 't', 'fitted'))


# Option 2 - returns = 'columns'
model_params(
	ndvi_new_names,
	id = 'identity',
	year = 'year',
	returns = 'columns',
	xmidS = 'xmidS_start',
	xmidA = 'xmidA_start',
	scalS = 0.05,
	scalA = 0.01
)

# Fit double logistic curve to NDVI time series for the observed days
model_ndvi(ndvi_new_names, observed = TRUE)
expect_equal(nrow(ndvi_new_names),
						 nrow(ndvi_new_names_raw))

# irg ---------------------------------------------------------------------
fit_raw <- copy(fit)
calc_irg(fit, id, yr)
expect_equal(nrow(fit),
						 nrow(fit_raw))

fit_raw_2 <- copy(fit_raw)
calc_irg(fit_raw, id, yr, scaled = TRUE)
expect_equal(nrow(fit_raw),
						 nrow(fit_raw_2))
