library(mlr3oml)
library(ggplot2)

col = ocl(410)

data_info = list_oml_data(col$data_ids)[NumberOfClasses == 2, .(name = name, majority_ratio = round(MajorityClassSize / NumberOfInstances, digits = 3))]

ggplot(data = data_info, aes(x = name, y = majority_ratio)) +
  geom_bar(stat = "identity")
