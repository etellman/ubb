root.dir <- '~/Documents/U/ubb/sccc/math146'

notes.dir <- paste(root.dir, "week15/notes", sep = "/")
data.dir <- paste(root.dir, "data/nfl", sep = "/")
setwd(data.dir)

rush <- read.csv("RUSH.csv"  )
pass <- read.csv("PASS.csv"  )
games <- read.csv("GAMES.csv")
core <- read.csv("CORE.csv"  )
team <- read.csv("TEAM.csv"  )

rush <- rename(rush, c(YDS = 'yds', BC = 'bc'))
rush <- subset(rush, yds > -10 & yds < 20)

head(rush)
by.player <- ddply(rush, .(bc), summarize, rushes = length(yds), yds = mean(yds))
str(by.player)

mean(rush$yds)

head(by.player[order(-by.player$rushes),], 20)

plot <- ggplot(data = ap, aes(x = yds)) + 
  geom_histogram(binwidth = 2, fill = "lightblue", color = "black") +
  labs(x = "Statistic") +
  theme(axis.title.y = element_blank())
print(plot)
sd(rush$yds)
mean(rush$yds)

p.value <- function(n, x, sigma, mu) {
  x.sd <- mu / sqrt(n)

  x.mean <- mean(sample(x, n))
  z <- (x.mean - mu) / x.sd

  p <- 1 - pnorm(z)
  data.frame(x.mean = x.mean, x.sd = x.sd, z = z, p = p)
}

mu <- mean(rush$yds)
mu
sd(rush$yds)

ap <- subset(rush, bc == 'AP-0700')
sd(ap$yds)

p.value(500, ap$yds, sd(rush$yds), mu)

mean(ap$yds)

sqrt(.53 * .47 / 2000)
