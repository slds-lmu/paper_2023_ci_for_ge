# Makefile

# Target to run the Shiny app
shiny:
	Rscript -e "shiny::runApp('shiny_app/app.R')"

# Phony targets
.PHONY: shiny
