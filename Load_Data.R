library(ggplot)

# Read data
resistance <- read.csv(
  "Resistance_Data.csv",
  na.strings = c("", "NA")
)

# Inspect lineage identifiers with incomplete time series.
# Check these for naming errors before final analysis.
lineage_counts <- table(resistance$Lineage)
lineage_counts[lineage_counts != 5]

# Repeated-measures ANOVA needs complete time series.
# with no missing measurements.
valid <- tapply(
  seq_len(nrow(resistance)),
  resistance$Lineage,
  function(i) {
    x <- resistance[i, ]
    nrow(x) == 5 &&
      setequal(x$Generation, 1:5) &&
      !anyNA(x$ZOI_R) &&
      length(unique(x$Treatment)) == 1 &&
      length(unique(x$Selection)) == 1
  }
)

dat <- subset(
  resistance,
  Lineage %in% names(valid)[valid]
)

dat$Lineage <- factor(dat$Lineage)
dat$Treatment <- factor(
  dat$Treatment,
  levels = c("Wild-Type", "Mutant")
)
dat$Selection <- factor(
  dat$Selection,
  levels = c("Control", "Selection")
)

# Treat generation as categorical:
dat$Generation <- factor(dat$Generation, levels = 1:5)

# Mean ZOI for each combination
means <- aggregate(
  ZOI_R ~ Generation + Treatment + Selection,
  data = dat,
  FUN = mean
)
means

par(mfrow = c(1, 1))

control_data <- subset(dat, Selection == "Control")

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