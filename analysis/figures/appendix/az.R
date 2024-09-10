library(ggplot2)
library(data.table)
library(here)

theme_set(theme_bw())

theme_update(text = element_text(size = 16))

az = readRDS(here("results", "raw", "az.rds"))
az$ratio = az$est / az$truth

y_breaks = c(0, 0.5, 1, sqrt(2), 2)
y_labels = c("0", "0.5", "1", expression(sqrt(2)), "2") 

x_breaks = c(1000, 5000, 10000)
x_labels = as.character(x_breaks)

p = ggplot(az, aes(x = n, y = ratio)) + 
  geom_point() + 
  labs(
    x = "n",
    y = expression(hat(sigma)[rocv] / sigma[true])) +
  geom_hline(yintercept = sqrt(2), linetype = "dotted", color = "red") + 
  scale_y_continuous(breaks = y_breaks, labels = y_labels, limits = c(0, 2)) + 
  scale_x_continuous(breaks = x_breaks, labels = x_labels)

print(p)

ggsave(here("figures", "appendix", "appendix_az.png"), height = 4, width = 6, dpi = 600)

