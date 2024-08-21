MethodNames <- readr::read_csv("Aggr_Plots/MethodNames.csv")

replace_names <- function(df1, methodcol, df2) {
  # Perform the name replacement by matching old names with new names
  df1[[methodcol]] <- ifelse(df1[[methodcol]] %in% df2$orig_names, 
                      df2$new_names[match(df1[[methodcol]], df2$orig_names)], 
                      df1[[methodcol]])
  return(df1)
}
