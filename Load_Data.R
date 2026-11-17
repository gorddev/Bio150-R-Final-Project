library(ggplot)

# Read data
res <- read.csv(
  "Resistance_Data.csv",
  na.strings = c("", "NA")
)


####################################
######## Cleans the data ###########
####################################

# Inspect lineage identifiers with incomplete time series.
# Check these for naming errors before final analysis.
lineage_counts <- table(res$Lineage)
lineage_counts[lineage_counts != 5]

# Repeated-measures ANOVA needs complete time series.
# with no missing measurements.
valid <- tapply(
  seq_len(nrow(res)),
  res$Lineage,
  function(i) {
    x <- res[i, ]
    nrow(x) == 5 &&
      setequal(x$Generation, 1:5) &&
      !anyNA(x$ZOI_R) &&
      length(unique(x$Treatment)) == 1 &&
      length(unique(x$Selection)) == 1
  }
)

res <- subset(
  res,
  Lineage %in% names(valid)[valid]
)

###################################
########## Creates main dataset ###
###################################

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
res.means <- aggregate(
  ZOI_R ~ Generation + Treatment + Selection,
  data = dat,
  FUN = mean
)
res.means

par(mfrow = c(1, 1))

control_data <- subset(dat, Selection == "Control")

######################################################
############# Separate by antibiotic type ############
######################################################

res.by_antibiotic <- res |>
  dplyr::filter(!is.na(ZOI_T), Selection == "Selection") |>
  tidyr::pivot_longer(
    cols = c(ZOI_S, ZOI_T, ZOI_R, ZOI_P),
    names_to = "Antibiotic",
    values_to = "ZOI"
  ) |> 
  dplyr::mutate(
    Antibiotic = as.factor(Antibiotic),
    Selection = as.factor(Selection),
    Treatment = as.factor(Treatment),
  )
res.by_antibiotic$Antibiotic <- dplyr::recode(res.by_antibiotic$Antibiotic, 
  ZOI_R = "Rifampin", 
  ZOI_P = "Penicillin", 
  ZOI_S = "Streptomycin",
  ZOI_T = "Tetracycline"
) 