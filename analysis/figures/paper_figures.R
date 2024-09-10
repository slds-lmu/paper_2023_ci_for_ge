library(here())

data_downloaded = dir.exists(here("results", "main")) && dir.exists(here("results", "ablation"))

if (!data_downloaded) {
	stopf("To recreate the plots you first need to download the folders main/ and ablation/ from zotero and move them into ./results/.")
}

paths = c(
  "analysis/figures/appendix/ablation.R",
	"analysis/figures/appendix/bccv.R",
	"analysis/figures/appendix/evaluation_density.R",
	"analysis/figures/appendix/losses.R",
	"analysis/figures/appendix/point_estimates.R",
	"analysis/figures/appendix/pq_vs_r.R",
	"analysis/figures/appendix/problematic_dgps.R",
	"analysis/figures/appendix/runtime.R",
	"analysis/figures/appendix/width_outliers.R",
	"analysis/figures/appendix/az.R",
	"analysis/figures/main/Round1.R",
	"analysis/figures/main/Round2.R",
	"analysis/figures/main/Round3.R",
	"analysis/figures/main/Round4.R"
)

print(getwd())
for (path in paths) {
	system(sprintf("Rscript %s", here::here(path)))
}
