# Makefile

# Target to run the Shiny app
shiny:
	cd shiny_app; Rscript app.R

figures:
	Rscript analysis/figures/all.R

# Phony targets
#.PHONY: shiny
