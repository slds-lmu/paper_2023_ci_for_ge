# Makefile

# Target to run the Shiny app
shiny:
	cd shiny_app; Rscript app.R

# Phony targets
#.PHONY: shiny
