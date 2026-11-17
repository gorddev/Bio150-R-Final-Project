source("Load_Data.R")

library(ggplot2)

res.scatter <- res |>
  dplyr::mutate(
    Week = as.integer(Generation)
  ) |>
  dplyr::filter(Selection=="Selection")

ggplot(data = res.scatter, mapping = aes(x=Week, y=ZOI_R, color=Treatment)) +
  geom_point() +
  geom_smooth(method="lm")