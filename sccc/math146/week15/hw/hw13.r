
root.dir <- "~/Documents/U/ubb/sccc/math146"

hw.dir <- paste(root.dir, "week15/hw", sep = "/")
data.dir <- paste(root.dir, "data/bps/PC-Text/ch14", sep = "/")
setwd(data.dir)
￼
# exercise 40
248 - 2.576 * 65/sqrt(269)

(137 - 120)/(65 / sqrt(169))

pnorm(3.4)

65/sqrt(169)

# ex 42
s <- 0.78/sqrt(148)
s

2 * (1 - pnorm((5.29 - 5.19)/s))

# ex 48
2 * (1 - pnorm(2.1))

# ex 49
1 - pnorm(1.33)

# ex 50
ex50 <- read.csv("ex14-50.dat", sep = '\t')
rename(ex50, c(Pounds = "pounds"))

str(ex50)
mean(ex50$pounds)
s <- 3000/sqrt(nrow(ex50))

plot <- ggplot(data = ex50, aes(x = pounds)) + 
  geom_histogram(binwidth = 1500, fill = "lightblue", color = "black") +
  labs(x = "Load") +
  theme(axis.title.y = element_blank())
print(plot)

file <- paste(hw.dir, "ex50.pdf", sep = "/")
ggsave(filename = file, plot = plot, width = 4, height = 2.5)

file

qnorm(0.95) * s

ggplot(data = ex50

# ex 51
ex51 <- csv.get("ex14-51.dat", sep = '\t', lowernames = T)

attributes(ex51$change) <- NULL

ex51
nrow(ex51)
mean(ex51$change)

plot <- ggplot(data = ex51, aes(x = change)) + 
  geom_histogram(binwidth = 2, fill = "lightblue", color = "black") +
  labs(x = "Percent Change") +
  theme(axis.title.y = element_blank())
print(plot)

file <- paste(hw.dir, "ex51.pdf", sep = "/")
ggsave(filename = file, plot = plot, width = 4, height = 2.5)

s51 <- 2.5/sqrt(nrow(ex51))
print(s51)

mu <- qnorm(0.995) * s
str(mu)

mean(ex51$change) - mu
ggplot(data = ex51

# ex 52
z <- (30841 - 31500)/670.82
30841 - 32000
z
pnorm(z)

# ex 53
z <- -3.587/s51
z
pnorm(z)

# ex 55
ex55 <- csv.get("ex14-55.dat", sep = '\t', lowernames = T)

attributes(ex55$change) <- NULL

m55 <- mean(ex55$diff)
s55 <- 0.22 / sqrt(nrow(ex55))
s55

z55 <- m55 / s55
z55

1 - pnorm(z55)

# ex 57
s57 <- 15/sqrt(72)
s57

qnorm(0.9)
