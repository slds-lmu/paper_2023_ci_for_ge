library(batchtools)
reg = loadRegistry(Sys.getenv("CI_PATH_CONZ"))
tbl = rbindlist(map(findDone()[[1]], loadResult))
saveRDS(tbl, "~/ci_conz.rds")
