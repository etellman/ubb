
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

# exercise 27
x <- pnorm(140, 125, 5)
round(1 - x, 4)

# exercise 28
x <- qnorm(0.95, 0, 1)
x <- qnorm(0.95, 125, 5)

round(x, 4)

5 * (1.6449) + 125

# exercise 30
s <- 1.4 / sqrt(52)
round(pnorm(100/52, 2.2, s), 4)
100/52

# exercise 31
u <- qnorm(0.99, 0, 1)
round(u, 4)
round(u * 0.01 + .2, 4)

round(qnorm(.99, 0.2, 0.01), 4)

# exercise 32
s <- 20.2/sqrt(40)
z <- (5 - 8.7)/3.1939
round(z, 4)

round(1 - pnorm(0.407, 0, 1), 4)
round(pnorm(-1.1585, 0, 1), 4)

# ex 33
1 - pnorm(0.4070)

# exercise 38
18.96 / sqrt(14000)
.1/.1602

pnorm(0.6242, 0 , 1) - pnorm(-0.6242, 0 , 1)

# exercise 38
s <- round(18.96 / sqrt(150000), 4)
s
round(.1/s, 4)

round(pnorm(2.0408, 0 , 1) - pnorm(-2.0408, 0 , 1), 4)

# ex 39
0.4 * 150000

# exercise 40
0.4961 - 0.4675
s <- round(18.96 / sqrt(3500), 4)
s
z <- .1/s
pnorm(-z, 0 , 1)
round(pnorm(z, 0 , 1) - pnorm(-z, 0 , 1), 4)

p <- pnorm(0.7, .6, s) - pnorm(.5, .6, s)

-(p -.4048)
0.9629 - 0.9587 

0.4048 - 0.245

pnorm(2.0408) - pnorm(-2.0408)
