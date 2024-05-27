# Makefile

# Target to run the Shiny app
shiny:
	cd shiny_app; Rscript -e "shiny::runApp('app.R')"

# Phony targets
.PHONY: shiny
