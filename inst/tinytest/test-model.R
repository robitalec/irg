library(data.table)
library(irg)

ndvi <- fread(system.file("extdata", "sampled-ndvi-MODIS-MOD13Q1.csv", package = "irg"))

filter_ndvi(ndvi)
scale_doy(ndvi)
scale_ndvi(ndvi)


# model_start -------------------------------------------------------------
## column checks
copyNDVI <- copy(ndvi)[, scaled := NULL]
expect_error(
	model_start(copyNDVI),
	'scaled column not found in DT - did you filter and scale?'
)

copyNDVI <- copy(ndvi)[, t := NULL]
expect_error(
	model_start(copyNDVI),
	't column not found in DT - did you scale doy?'
)

copyNDVI <- copy(ndvi)[, id := NULL]
expect_error(
	model_start(copyNDVI),
	"id \\('id'\\) column not found in DT"
)

copyNDVI <- copy(ndvi)[, yr := NULL]
expect_error(
	model_start(copyNDVI),
	"year \\('yr'\\) column not found in DT"
)

expect_true(all(
	c('id', 'yr', 'xmidS_start', 'xmidA_start') %in%
		colnames(model_start(ndvi))
))


# model_params ------------------------------------------------------------
expect_error(
	model_params(ndvi, returns = 'columns'),
	'starting parameters must be provided',
	fixed = FALSE
)

expect_error(
	model_params(ndvi, xmidS = 'potato'),
	"xmidS \\('potato'\\) column not found in DT"
)

expect_error(
	model_params(ndvi, xmidS = 'xmidS_start', xmidA = 'potato'),
	"xmidA \\('potato'\\) column not found in DT"
)

expect_error(
	model_params(ndvi, xmidS = 'xmidS_start', xmidA = 'xmidA_start',
							 scalS = 'potato'),
	"scalS \\('potato'\\) column not found in DT"
)

expect_error(
	model_params(ndvi, xmidS = 'xmidS_start', xmidA = 'xmidA_start',
							 scalS = 'id', #just to skip to scalA
							 scalA = 'potato'),
	"scalA \\('potato'\\) column not found in DT"
)

expect_error(
	model_params(ndvi, xmidS = 'xmidS_start', xmidA = 'xmidA_start',
							scalS = 'id', #just to skip to returns
							scalA = 'id'), #just to skip to returns
	'argument "returns" is NULL, must provide one of "models" or "columns"'
)

mods <- model_params(
	ndvi,
	returns = 'models',
	xmidS = 0.44,
	xmidA = 0.80,
	scalS = 0.05,
	scalA = 0.01
)
expect_true(all(c('id', 'yr', 'xmidS', 'xmidA', 'scalS', 'scalA')
								%in%
									colnames(mods)))

expect_true(nrow(mods) < nrow(ndvi))

model_params(
	ndvi,
	returns = 'columns',
	xmidS = 'xmidS_start',
	xmidA = 'xmidA_start',
	scalS = 0.05,
	scalA = 0.01
)
expect_true(all(c('id', 'yr', 'xmidS', 'xmidA', 'scalS', 'scalA')
								%in%
									colnames(ndvi)))



model_params(
	ndvi,
	returns = 'columns',
	xmidS = 'xmidS_start',
	xmidA = 'xmidA_start',
	scalS = 0.05,
	scalA = 0.01
)

# model_ndvi --------------------------------------------------------------
copyNDVI <- copy(ndvi)[, xmidS := NULL]
expect_error(
	model_ndvi(copyNDVI),
	'xmidS column not found in DT'
)

copyNDVI <- copy(ndvi)[, xmidA := NULL]
expect_error(
	model_ndvi(copyNDVI),
	'xmidA column not found in DT'
)

copyNDVI <- copy(ndvi)[, scalS := NULL]
expect_error(
	model_ndvi(copyNDVI),
	'scalS column not found in DT'
)

copyNDVI <- copy(ndvi)[, scalA := NULL]
expect_error(
	model_ndvi(copyNDVI),
	'scalA column not found in DT'
)

modNDVI <- model_ndvi(ndvi, observed = FALSE)
expect_true(all(c('id', 'yr',
									'xmidS', 'xmidA', 'scalS', 'scalA', 'fitted')
								%in%
									colnames(modNDVI)))

expect_true(nrow(ndvi) < nrow(modNDVI))

expect_true(nrow(ndvi) == nrow(model_ndvi(ndvi, observed = TRUE)))
