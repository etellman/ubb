math205.dir <- '~/Documents/U/ubb/asu/math205'
data.dir <- paste(math205.dir, 'data', 'bps', 'PC-Text', 'ch03', sep = '/')
setwd(data.dir)

week.dir <- paste(math205.dir, 'week03', sep = '/')
figure.dir <- paste(week.dir, 'hw', 'figures', sep = '/')

# exercise 25
ex25.range <- function(mean, sd) {
  c(mean - 2 * sd, mean + 2 * sd)
}

ex25.range(373, 67)

# exercise 27
pnorm(70, 100, 15)

# exercise 28
plot <- ggplot(data.frame(x = c(-3, 3)), aes(x = x)) +
  stat_function(fun = function(x) dnorm(x, 0, 1)) +

  # part a
  # stat_function(fun = limit.domain(function(x) dnorm(x, 0, 1), -3, -2.25), 
  #               geom = "area", fill = "grey") +
  
  # part b
  # stat_function(fun = limit.domain(function(x) dnorm(x, 0, 1), -2.25, 3), 
  #               geom = "area", fill = "grey") +

  # part c
  # stat_function(fun = limit.domain(function(x) dnorm(x, 0, 1), 1.75, 3), 
  #               geom = "area", fill = "grey") +

  # part d
  stat_function(fun = limit.domain(function(x) dnorm(x, 0, 1), -2.25, 2.25), 
                geom = "area", fill = "grey") +

  theme_ubb + theme(axis.title = element_blank()) 
print(plot)

save.plot(plot, figure.dir, 'ex28d.pdf')

# exercise 46
ex46 <- read.delim("ex03-46.dat", header = TRUE, sep = '\t')
str(ex46)

plot <- ggplot(ex46, aes(x = IQ)) + 
  geom_histogram(fill = "grey", color = "black", binwidth = 10) +
  scale_x_continuous(breaks = seq(70, 140, 10)) + 
  scale_y_continuous(breaks = seq(0, 10, 2)) + 
  theme_ubb + theme(axis.title.y = element_blank()) 
print(plot)

save.plot(plot, figure.dir, 'ex46.pdf')

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

