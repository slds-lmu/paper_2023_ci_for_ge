# Makefile

# Target to run the Shiny app
shiny:
	Rscript -e 'renv::restore("shiny_app/", prompt = FALSE)'
	cd shiny_app; Rscript -e "shiny::runApp('app.R')"

# Phony targets
.PHONY: shiny
