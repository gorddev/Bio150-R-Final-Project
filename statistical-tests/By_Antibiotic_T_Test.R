source("Load_Data.R")

if (!requireNamespace("purrr", quietly=TRUE)) {
  stop("Library purrr required to progress.")
}

this.r_names <- levels(res.by_antibiotic$Antibiotic)
this.c_names <- c("t", "P-Value", "Conf. 5%", "Conf. 95%")

this.output <- data.frame(
  matrix(
    NA, 
    nrow = length(this.r_names), 
    ncol = length(this.c_names), 
    dimnames = list(this.r_names, this.c_names)
  )
)

colnames(this.output) <- this.c_names

purrr::walk(levels(res.by_antibiotic$Antibiotic), \(x) {
  r <- res.by_antibiotic |>
    dplyr::filter(Antibiotic == x)

  r.mutant <- r |> dplyr::filter(Treatment == "Mutant")
  r.wild <- r |> dplyr::filter(Treatment == "Wild-Type")
  t <- t.test(r.mutant$ZOI, r.wild$ZOI)

  this.output[x, "t"] <<- t$statistic
  this.output[x, "P-Value"] <<- t$p.value
  this.output[x, "Conf. 5%"] <<- t$conf.int[1]
  this.output[x, "Conf. 95%"] <<- t$conf.int[2]
})

this.output

write.csv(this.output, file="output/By_Antibiotic_T_Test.csv")