
root.dir <- "~/Documents/U/ubb/sccc/math146"

hw.dir <- paste(root.dir, "week14/hw", sep = "/")
setwd(hw.dir)
￼
# exercise 24
choose(20, 5) * 0.25^5 * 0.75^15

# exercise 25
success <- 0:5
ex25 <- data.frame(s = success, p = dbinom(success, size = 5, 0.65))
ex25

sink('r.tex')
  xtable(ex25, digits = 4)
sink()

plot <- ggplot(ex25, aes(x = factor(s), y = p)) + 
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +
  xlab("Years") +
  theme(axis.title.y = element_blank())
print(plot)

ggsave('ex25.pdf', plot, width = 4, height = 2.5)

sqrt(5 * 0.65 * 0.35)

# ex 26
sqrt(12 * .5 * .5)
choose(12, 8) * 0.5^8 * 0.5^4

dbinom(8, 12, 0.5) + dbinom(9, 12, 0.5) + dbinom(10, 12, 0.5) + dbinom(11, 12, 0.5) + dbinom(12, 12, 0.5)  

1 - pbinom(7, 12, 0.5)

# ex 27
dbinom(0, 20, 0.05) 

# ex 28
s <- sqrt(500 * .5 * .5)
z <- (235 - 250)/s
pnorm(z)

1 - round(pnorm(x), 4)
round(1 - pnorm(x), 4)
1 - pbinom(235, 500, 0.5)
1 - pnorm(235, 250, sqrt(125))

# ex 31
choose(8, 6) * 0.75^6 * 0.25^2
dbinom(6, size = 8, 0.75)

.75 * 80
z <- 1/sqrt(60 * 0.75 * 0.25)
pnorm(z)
1 - pbinom(59, size = 80, 0.75)

# ex 34
0.13 * 1200
s <- sqrt(1200 * .13 * .87)
s

156 + s

156 + 2 * s
200/.13

# ex 35
s <- sqrt(250 * .25 * .75)
print(s)

.75 * 250
.7 * 250
.8 * 250

z <- (187.5 - 175)/s
z

pnorm(-z)

pnorm(z) - pnorm(-z)

.75 * 200

pnorm(1.886) - pnorm(-1.886)
pnorm(-1.886) 

pnorm(160, 150, 5.303) - pnorm(140, 150, 5.303)

# ex 36
s <- sqrt(10000 * .5 * .5)
print(s)

z <- (5066 - 5000)/s
z
1 - (pnorm(z) - pnorm(-z))

1 - pnorm(1.886) + pnorm(-1.886) 

pnorm(160, 150, 5.303) - pnorm(140, 150, 5.303)

# ex 38
.05 * 20
s <- sqrt(5000 * .5 * .5)
print(s)

z <- (5067 - 5000)/s

pnorm(-z)

1 - pnorm(1.886) + pnorm(-1.886) 

pnorm(160, 150, 5.303) - pnorm(140, 150, 5.303)

# ex 39
mu <- .95 * 1400
.8^20 + choose(20, 1) * 0.20^1 * 0.8^19 + choose(20, 2) * 0.20^2 * 0.8^18 

mu
s <- sqrt(1400 * .95 * .05)
s

(1050 - mu)/s
dbinom(0, 20, 0.05) + dbinom(1, 20, 0.05) + dbinom(2, 20, 0.05) 
dbinom(20, 20, 0.95) + dbinom(19, 20, 0.95) + dbinom(18, 20, 0.95) 

# ex 40
0.95 * 1400
s <- sqrt(1400 * 0.95 * 0.05)
(0.75 * 1400 - 1330) / s 

0.75 * 1400

