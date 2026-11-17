source("../Load_Data.R")

library(ggplot2)

res.start_image("Zone of Interest by Antibiotic and Treatment Type");

ggplot(res.by_antibiotic, aes(x = Antibiotic, y = ZOI, fill = Treatment)) +
  #geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_boxplot(
    position = position_dodge(width = 0.8),
    outlier.shape = 21,
    width = 0.65,
    outlier.alpha = 0.7
  ) +
  scale_fill_manual(values = c("Wild-Type" = "#2b5c8f", "Mutant" = "#e07a5f")) +
  #scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "Zone of Interest by Antibiotic and Treatment Type",
    x = "Antibiotic",
    y = "ZOI (cm)",
    fill = "Treatment"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.major.x = element_blank(),
    legend.position = "top"
  )

res.end_image()