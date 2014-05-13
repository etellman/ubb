root.dir <- '~/Documents/U/ubb/sccc/math146'

notes.dir <- paste(root.dir, "week14/notes", sep = "/")
data.dir <- paste(root.dir, "data/bps/PC-Text/ch14", sep = "/")
setwd(data.dir)

60/sqrt(840)
272+4.14

x <- (1 - 0.975)/2
qnorm(c(x, 1 - x))

# ex 14.4
5 + 0.2 * qnorm(0.95)

# ex 14.5
ex5 <- read.delim("ex14-05.dat", header = TRUE, sep = '\t')

str(ex5)
mean(ex5$IQ) - 2.576 * 15/sqrt(31)

105.
