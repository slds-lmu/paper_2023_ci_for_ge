library(here)
library(data.table)

colon = read.table(here("data", "covariance", "colon.data.txt"))
colon = colon[, -1]

fwrite(colon, here("data", "original", "colon.csv"))
rm(list = ls())

breast2 <- read.table(here("datamodels", "covariance", "breast.2.class.data.txt"))
breast2 = breast2[, -1]

fwrite(breast2, here("data", "original", "breast.csv"))

rm(list = ls())

prostate = read.table(here("datamodels", "covariance", "prostate.data.txt"))

prostate = prostate[, -1]
cov_prostate = cov(prostate)

saveRDS(cov_prostate, here("data", "covariance", "prostate.rds"))
rm(list = ls())

load(here("datamodels", "covariance", "leukemia.Rda"))

cov_leukemia = cov(x)
saveRDS(cov_leukemia, here("data", "covariance", "leukemia.rds"))
rm(list = ls())
