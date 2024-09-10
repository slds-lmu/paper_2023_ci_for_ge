# Makefile


figures_paper:
	Rscript analysis/figures/paper_figures.R

figures_granular:
	Rscript analysis/figures/granular/make_plots.R

process_all:
	Rscript analysis/processing/truth_losses.R
	Rscript analysis/processing/main.R
	Rscript analysis/processing/ablation.R

process_truth:
	Rscript analysis/processing/truth_losses.R

process_main:
	Rscript analysis/processing/main.R

process_ablation:
	Rscript analysis/processing/ablation.R

simulate_simple:
	Rscript bates.R
	Rscript chen.R
	Rscript friedman1.R
	Rscript janitza.R

# Phony targets
#.PHONY: shiny
