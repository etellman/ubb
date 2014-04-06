
root.dir <- "~/Documents/U/ubb/sccc/math146"

data.dir <- paste(root.dir, "data/bps/PC-Text", sep = "/")
hw10.data.dir <- paste(data.dir, "ch010", sep = "/")

hw.dir <- paste(root.dir, "week10/hw", sep = "/")
figures.dir <- paste(hw.dir, "figures", sep = "/")

setwd(hw.data.dir)
￼
# exercise 25
2.4 / sqrt(10)

# exercise 26
(69 - 70) / (2.8) 
(71 - 70) / (2.8) 

round(pnorm(71, 70, 2.8) - pnorm(69, 70, 2.8), 4)

2.8/5

round((69 - 70) / (0.56), 4) 
round((71 - 70) / (0.56), 4)

round(pnorm(71, 70, 0.56), 4)
round(pnorm(69, 70, 0.56), 4)
round(pnorm(71, 70, 0.56) - pnorm(69, 70, 0.56), 4)
