library(duckdb)
library(DBI)

data_ids = c(
  45692, 45694, 45655, 45666, 45667, 45670, 45671, 45695, 45696, 45570, 45689, 45704,
  45654, 45665, 45668, 45669, 45672, 45693
)

size = 10000

for (data_id in data_ids) {
	print(paste0("data id is: ", data_id))
for (repl in 1:2) {
  con = dbConnect(duckdb::duckdb(), ":memory:", path = tempfile())
  # the ids for the data subset
  use_ids_path = here::here("data", "splits", data_id, paste0(size, ".parquet"))
  
  # for some resamplings where we also calculate the proxy quantities / the true values,
  # we need a large holdout set to approximate them
  DBI::dbExecute(con, paste0("CREATE OR REPLACE VIEW use_table AS SELECT * FROM read_parquet('", use_ids_path, "')"))
  use_ids = dbGetQuery(con, sprintf("SELECT row_id FROM use_table WHERE iter = %i", repl))$row_id

  print(length(use_ids))
  
  DBI::dbDisconnect(con, shutdown = TRUE)
}
}

for (data_id in data_ids) {
  con = dbConnect(duckdb::duckdb(), ":memory:", path = tempfile())
  holdout_ids_path = here::here("data", "splits", data_id, "holdout_100000.parquet")
  DBI::dbExecute(con, paste0("CREATE OR REPLACE VIEW holdout_table AS SELECT * FROM read_parquet('", holdout_ids_path, "')"))
      holdout_ids = dbGetQuery(con, paste0("SELECT row_id FROM holdout_table"))$row_id
  print(length(holdout_ids))

  DBI::dbDisconnect(con, shutdown = TRUE)
 }

