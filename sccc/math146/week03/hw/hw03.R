root.dir <- "~/Documents/U/ubb/sccc/math146"

data.dir <- paste(root.dir, "data/bps/PC-Text", sep = "/")
hw3.data.dir <- paste(data.dir, "ch03", sep = "/")

hw.dir <- paste(root.dir, "week03/hw", sep = "/")
figures.dir <- paste(hw.dir, "figures", sep = "/")

setwd(hw3.data.dir)

# exercise 25
ex25.range <- function(mean, sd) {
  c(mean - 2 * sd, mean + 2 * sd)
}

ex25.range(373, 67)

# exercise 27
pnorm(70, 100, 15)

# exercise 28
plot <- ggplot(data.frame(x = c(-25, 525)), aes(x = x)) +
  stat_function(fun = function(x) dnorm(x, 250, 176)) +
  stat_function(fun = limit.domain(function(x) dnorm(x, 250, 176), -25, 25), 
                geom = "area", fill = "lightblue") +
  stat_function(fun = limit.domain(function(x) dnorm(x, 250, 176), 475, 525), 
                geom = "area", fill = "lightblue") 
print(plot)

ggsave(paste(figures.dir, 'ex28c.pdf', sep = '/'))

# exercise 46
ex46 <- read.delim("ex03-46.dat", header = TRUE, sep = '\t')
str(ex46)

plot <- ggplot(ex46, aes(x = IQ)) + 
  geom_histogram(fill = "lightblue", color = "black", binwidth = 10) +
  labs(x = "IQ", y = "Count") +
  ggtitle("IQ")
print(plot)

ggsave("~/Documents/U/ubb/sccc/math146/week03/hw/figures/ex46.eps", width = 4, height = 2.5)

iq <- ex46$IQ

round(mean(iq), digits = 2)
round(sd(iq), digits = 2)

ex46.expected.1sd <- round( .68 * 31 )
ex46.actual.1sd <- length(iq[mean(iq) - 1 * sd(iq) < iq & iq < mean(iq) + 1 * sd(iq)])

ex46.expected.2sd <- round( .95 * 31 )
ex46.actual.2sd <- length(iq[mean(iq) - 2 * sd(iq) < iq & iq < mean(iq) + 2 * sd(iq)])

ex46.expected.1sd
ex46.actual.1sd

ex46.expected.2sd
ex46.actual.2sd

round(mean(iq) - 2 * sd(iq), digits = 2)

# exercise 48
ex48 <- c(4.33, 5.05, 5.44, 5.79, 6.81)
diff(ex48)

z1 <- round((5.05 - 5.43)/0.54, digits = 4)
round(pnorm(z2), 4)

z2 <- round((5.79 - 5.43)/0.54, digits = 4)
round(pnorm(z2), 4)
z2

pnorm(5.05, 5.43, 0.54)
pnorm(5.79, 5.43, 0.54)

