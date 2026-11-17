source("../Load_Data.R");

with(control_data, interaction.plot(
  x.factor = Generation,
  trace.factor = Treatment,
  response = ZOI_R,
  fun = mean,
  type = "b",
  pch = c(16, 17),
  col = c("steelblue", "firebrick"),
  ylim = c(3,9),
  xlab = "Generation",
  ylab = "Mean rifampin zone of inhibition",
  trace.label = "Treatment",
  main = "Control"
))