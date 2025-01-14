library(pensim)
library(nanoparquet)

set.seed(1)

ps = 100 * 2^c(0, 1, 2, 3, 4, 5, 6)
nblocks = 25

for (p in ps) {
  d = create.data(
    nsamples = 600000,
    nvars = rep(p / nblocks, nblocks),
    cors = rep(0.8, nblocks),
    response = "binary",
    associations = rep(1, nblocks),
    firstonly = rep(TRUE, nblocks)
  )$data 

  for (i in 1:500) {
    sub = d[((i - 1) * 1000 + 1):(i * 1000), ]
    write_parquet(sub, sprintf("/gscratch/sfische6/highdim-data/highdim_%s_%d.parquet", p, i))
  }
  if (p == 6400) {
    write_parquet(d[500001:600000, 1:3200], sprintf("/gscratch/sfische6/highdim-data/highdim_%s_holdout_1.parquet", p))
    write_parquet(d[500001:600000, 3201:ncol(d)], sprintf("/gscratch/sfische6/highdim-data/highdim_%s_holdout_2.parquet", p))
  } else {
    write_parquet(d[500001:600000, ], sprintf("/gscratch/sfische6/highdim-data/highdim_%s_holdout.parquet", p))
  }
}
