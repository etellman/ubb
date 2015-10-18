root.dir <- '~/Documents/U/ubb/asu/math205'

bats <- c(62 , 52 , 68 , 23 , 34 , 45 , 27 , 42 , 83 , 56 , 40)

x <- mean(bats)

x
1 - pnorm((x - 40)/18)

-qnorm(0.025)

?qnorm

((-qnorm(0.025) * 18)/2)^2

z <- -qnorm(0.025)

1 - pnorm(8.36/(18/sqrt(11))) 

48.36 - 10.64

17.64^2

100-38

(1.96 * 18 / 2)^2
