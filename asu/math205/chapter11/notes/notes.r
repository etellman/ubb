root.dir <- "~/Documents/U/ubb/sccc/math146"

notes.dir <- paste(root.dir, "week12/notes", sep = "/")
figures.dir <- paste(notes.dir, "figures", sep = "/")

setwd(notes.dir)


coins <- rbind(
  data.frame(tosses = 10   , heads = 4 ),
  data.frame(tosses = 20   , heads = 10 ),
  data.frame(tosses = 30   , heads = 17 ),
  data.frame(tosses = 40   , heads = 21 ),
  data.frame(tosses = 50   , heads = 25 ),
  data.frame(tosses = 60   , heads = 29 ),
  data.frame(tosses = 70   , heads = 32 ),
  data.frame(tosses = 80   , heads = 35 ),
  data.frame(tosses = 90   , heads = 40 ),
  data.frame(tosses = 100   , heads = 44 ),
  data.frame(tosses = 200   , heads = 98 ),
  data.frame(tosses = 300   , heads = 146 ),
  data.frame(tosses = 400   , heads = 199 ),
  data.frame(tosses = 500   , heads = 255 ),
  data.frame(tosses = 600   , heads = 312 ),
  data.frame(tosses = 700   , heads = 368 ),
  data.frame(tosses = 800   , heads = 413 ),
  data.frame(tosses = 900   , heads = 458 ),
  data.frame(tosses = 1000  , heads = 502 ),
  data.frame(tosses = 2000  , heads = 1013 ),
  data.frame(tosses = 3000  , heads = 1510 ),
  data.frame(tosses = 4000  , heads = 2029 ),
  data.frame(tosses = 5000  , heads = 2533),
  data.frame(tosses = 6000  , heads = 3009),
  data.frame(tosses = 7000  , heads = 3516),
  data.frame(tosses = 8000  , heads = 4034),
  data.frame(tosses = 9000  , heads = 4538),
  data.frame(tosses = 10000 , heads = 5067)
  )

coins.statistics <- ddply(coins, 'tosses', transform, diff = heads - tosses / 2, 
      percentage = round(100 * heads / tosses, 3))

sink('r.tex')
  xtable(coins.statistics)
sink()

plot <- ggplot(coins.statistics, aes(x = tosses, y = diff)) +
  scale_x_log10(breaks = c(10, 100, 1000, 10000)) +
  geom_point() +
  geom_line()
print(plot)

file <- paste(figures.dir, 'diff.pdf', sep = '/')
ggsave(file, plot, width = 4, height = 2.5)

2/38

3 * sqrt(12/38 * 26/38)
sqrt(18/38 * 20/38)

sqrt(10000) * 1.16
x <- 250/116

.05 * 25000
1 * sqrt(25000)
(1000-1250)/24965
1 - pnorm(-0.01)

.05 * 25000

f <- function(n, amount) {
  sd <- 2 * amount * sqrt(20/38 * 18/38)
  se <- sqrt(n) * sd
  z <- (1000 - 1250)/se
  p <- 1 - pnorm(z)
  data.frame(n = n, amount = amount, sd = sd, se = se, z = z, p = p)
}

roulette <- rbind(
  f(25000, 1),
  f(1000, 25),
  f(100, 250),
  f(10, 2500),
  f(1, 25000)
)

sink('r.tex')
  xtable(roulette)
sink()

1.4 * 15

sqrt(200) * 5
30/71

(.5/.01)^2

.5/sqrt(1000)

18/38

.3/sqrt(2500)

.01/.006

pnorm(1.6) - pnorm(-1.6)

0.5 * sqrt(1000) 

500 

.5/sqrt(10)

1.36 * sqrt(50)

.05*50

0.8146 * 50

1.36 * sqrt(50)

10/25

0.4 * 5

15/sqrt(200)

15/100/sqrt(200)
30/200


          n & = \del{ \frac{0.5}{0.025} }^2 \\

0.5/sqrt(800)
(0.5/0.025)^2



(1 - pnorm(3/29))^20

1 - pnorm(3/(29/sqrt(20)))
