# Test use example script


# Packages ----------------------------------------------------------------
library(data.table)
library(irg)



# Test sensors ------------------------------------------------------------
expect_error(use_example_ee_script(sensor = 'potato'),
						 'sensor must be one of',
						 fixed = FALSE)

expect_message(use_example_ee_script('MODIS'),
							 'Example script saved to',
							 fixed = FALSE)
