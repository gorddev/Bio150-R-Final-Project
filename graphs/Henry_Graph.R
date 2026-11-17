source("../Load_Data.R")

#Shows all 5 generagtions

boxplot(ZOI_R ~ Generation:Treatment, data = res, 
    main = "Radius of Rifampin ZOI by Treatment Type and Generation",
    xlab = "Treatment Type and Generations",
    ylab = "ZOI Radius aroung ",
    col = rep(c("coral2", "cornflowerblue"), each = 5)) 
legend("topleft",
    inset = c(0.31, 0),
    legend = c("Wild type", "Mutant"),
    fill = c("coral2", "cornflowerblue"),
    title = "Treament Type")

#Shows average of all generagtions
boxplot(ZOI_R ~ Treatment, data = res,
    main = "Radius of Rifampin ZOI by Treatment Type and Generation",
    xlab = "Treatment Type and Generations",
    ylab = "ZOI Radius aroung ",
    col = rep(c("coral2", "cornflowerblue"))) 

legend("topleft",
    inset = c(0.31, 0),
    legend = c("Wild type", "Mutant"),
    fill = c("coral2", "cornflowerblue"),
    title = "Treament Type")

#ONE SIDED t test for data 
t.test(ZOI_R ~ Treatment, data = res, alternative = c("less"))


