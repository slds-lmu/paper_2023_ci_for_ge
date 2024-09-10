library(ggplot2)
library(data.table)
library(here)
library(mlr3misc)

tbl = readRDS(here("results", "raw", "runtime.rds"))
tbl = tbl[, list(time = mean(time)), by = c("name", "size", "learner_name")]

tbl <- dcast(tbl, name + learner_name ~ size, value.var = "time")

replace_names <- function(df1, methodcol, df2) {
  # Perform the name replacement by matching old names with new names
  df1[[methodcol]] <- ifelse(df1[[methodcol]] %in% df2$orig_names,
                      df2$new_names[match(df1[[methodcol]], df2$orig_names)],
                      stop())
  return(df1)
}

nmtbl = read.csv(here("analysis", "MethodNames2.csv"))

# Replace the values from tbl$orig_names with those in tbl$new_names
# in the column method of the ci data frame
tbl = replace_names(tbl, "name", nmtbl)


xtable::xtable(tbl[learner_name == "ranger", -"learner_name"])
