
root.dir <- "~/Documents/U/ubb/sccc/math146"

hw.dir <- paste(root.dir, "week14/hw", sep = "/")
setwd(hw.dir)
￼
# exercise 24
choose(20, 5) * 0.25^5 * 0.75^15

# exercise 25
success <- 0:5
ex25 <- data.frame(s = success, p = dbinom(success, size = 5, 0.5))
ex25

plot <- ggplot(ex25, aes(x = factor(s), y = p)) + 
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +
  xlab("Successes") +
  theme(axis.title.y = element_blank())
print(plot)

ggsave('ex25.pdf', plot, width = 4, height = 2.5)

# ex 26
dbinom(8, 12, 0.5) + dbinom(9, 12, 0.5) + dbinom(10, 12, 0.5) + dbinom(11, 12, 0.5) + dbinom(12, 12, 0.5)  

1 - pbinom(7, 12, 0.5)

# ex 27
dbinom(0, 20, 0.05) 

# ex 28
sqrt(125)
x <- (235 - 250)/sqrt(125)
x

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
s <- sqrt(156 * .13 * .87)

156 + 2 * s
200/.13

# ex 35
s <- sqrt(150 * .25 * .75)
print(s)

(140 - 150)/5.303

.7 * 200

pnorm(1.33333) - pnorm(-1.33333)

pnorm(1.886) - pnorm(-1.886)
pnorm(-1.886) 

pnorm(160, 150, 5.303) - pnorm(140, 150, 5.303)

# ex 36
s <- sqrt(5000 * .5 * .5)
print(s)

z <- (5067 - 5000)/s

pnorm(-z)

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
choose(20, 2) * 0.05^2 * 0.95^18 + choose(20, 1) * 0.05^1 * 0.95^19 + .95^20

choose(20, 2) * 0.05^2 * 0.95^18 + choose(20, 1) * 0.05^1 * 0.95^19 + .95^20

# ex 40
0.95 * 1400
sqrt(1330 * 0.95 * 0.05)
(0.75 * 1400 - 1330) / 7.95 

0.75 * 1400

