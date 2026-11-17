source("Load_Data.R")

this.r_names <- levels(res$Generation)
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

purrr::walk(levels(res$Generation), \(x) {
  r <- res |> dplyr::filter(Generation == x)

  r.mutant <- r |> dplyr::filter(Treatment == "Mutant")
  r.wild <- r |> dplyr::filter(Treatment == "Wild-Type")

  print("mutant")
  print(r.mutant)
  print("wild")
  print(r.wild)

  t <- t.test(r.mutant$ZOI_R, r.wild$ZOI_R)


  this.output[x, "t"] <<- t$statistic
  this.output[x, "P-Value"] <<- t$p.value
  this.output[x, "Conf. 5%"] <<- t$conf.int[1]
  this.output[x, "Conf. 95%"] <<- t$conf.int[2]

})


this.output
this.output |>
  dplyr::mutate(
    Week = as.factor(as.integer(levels(res$Generation))),
    Significant = `P-Value` <= 0.05) |>
  dplyr::select(Week, dplyr::everything()) |>
  dplyr::tibble()

write.csv(this.output, file="output/By_Week.csv")