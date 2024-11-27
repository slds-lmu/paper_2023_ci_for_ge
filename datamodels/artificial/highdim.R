library(pensim)
library(nanoparquet)

set.seed(1)

ds = list()

firstonlys = c(FALSE, TRUE)
ps = 100 * 2^c(0, 1, 2, 3, 4, 5, 6)
nblocks = 20

for (firstonly in firstonlys) {
  for (p in ps) {
      d = create.data(
          nsamples = 600000,
          nvars = rep(p / nblocks, nblocks),
          cors = rep(0.5, nblocks),
          response = "binary",
          associations = rnorm(nblocks),
          firstonly = rep(firstonly, nblocks)
      )$data 

      # round all numeric columns except for the target so we need less space

      for (name in names(d)[-length(names(d))]) {
        d[[name]] = round(d[[name]], 3)
      }

      write_parquet(d, sprintf("/gscratch/sfische6/highdim-data/highdim_%s_%s.parquet", if (firstonly) "firstonly" else "all", p))

      ds = append(ds, list(d))
  }
}

