
root.dir <- "~/Documents/U/ubb/sccc/math146"

hw.dir <- paste(root.dir, "week11/hw", sep = "/")
figures.dir <- paste(hw.dir, "figures", sep = "/")

setwd(hw.dir)
￼
# exercise 27
0.75^8

pbinom(0, size = 8, prob = .25, lower.tail = F)

# exercise 28
1 - 0.928^10
pbinom(0, size = 10, prob = .072, lower.tail = F)

# exercise 29
(2 * 9 * 19 + 11)/20^3

# exercise 30
round(.35^3 + .65^3, 4)

dbinom(3, size = 3, prob = 0.35)

# exercise 31
.6 * .5
.5 - .4 * .5

# exercise 32
0.86 * 0.01 + 0.03

# exercise 34
.26/.41


# exercise 35
p <- 0.215 + 0.1 + 0.006 
p
.106/p

# ex 39
wd <- round(32/2506, 4)
print(wd)
round(32/59, 4)

# ex 40
m <- 1025/2506
print(m)

b <- 1679/2506
print(b)

mb <- 693/2506
print(mb)

round(32/59, 4)

round(693/1025, 4)
.6761 * .4090

# ex 41
seedlings <- rbind(
                   data.frame(cover = 'a', value = 60, damage = T),
                   data.frame(cover = 'a', value = 151, damage = F),

                   data.frame(cover = 'b', value = 76, damage = T),
                   data.frame(cover = 'b', value = 158, damage = F),

                   data.frame(cover = 'c', value = 44, damage = T),
                   data.frame(cover = 'c', value = 177, damage = F),

                   data.frame(cover = 'd', value = 29, damage = T),
                   data.frame(cover = 'd', value = 176, damage = F)
                   )

209/871

ex19 <- rbind(
  data.frame(treatment = "lithium", subjects = 24, success = 6),
  data.frame(treatment = "placebo", subjects = 24, success = 4),
  data.frame(treatment = "desipramine", subjects = 24, success = 14)
)

seedlings
ddply(seedlings, .(cover), summarize, damage = damage, 
      p = round(value / sum(value), 2))

cast(seedlings, damage ~ ., fun.aggregate = sum, margins = T)
cast(seedlings, cover + damage ~ ., fun.aggregate = sum)

# ex 46
0.05/.4

# ex 47
6/36 * 30/36

25/6^3

# ex 48
p1 <- 0.59 * 0.73
p2 <-  0.41 * 0.86 * 0.59
data.frame(p1 = p1, p2 = p2, p12 = p1 + p2)

# ex 50
0.4307/0.6387

# ex 50
0.36/0.58

# ex 56
.5^2 + 2 * .25^2

.5^2
.25^2
