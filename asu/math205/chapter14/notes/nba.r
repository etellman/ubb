
root.dir <- '~/Documents/U/ubb/sccc/math146'
data.dir <- paste(root.dir, "data/nba", sep = "/")
setwd(data.dir)

nba <- read.delim('players_2013-14.csv', header = TRUE, sep = ',', strip.white = TRUE)
nba <- rename(nba, c(FGP = 'fgp', Pos = 'pos'))

nba <- subset(nba, !is.na(fgp))

nba$fgp
head(nba)

str(nba$pos)

ddply(nba, .(pos), function(df) mean(df$fgp))

centers <- subset(nba, pos == 'C')

mean(centers$fgp)
mean(nba$fgp)

plot <- ggplot(data = subset(nba), aes(x = fgp)) + 
  geom_histogram(binwidth = 0.05, fill = "lightblue", color = "black") +
  labs(x = "Statistic") +
  theme(axis.title.y = element_blank())
print(plot)

mean(nba$fgp)
sd(nba$fgp)

sd(acs$wagp)
mean(acs$wagp)
mean(women)
sd(women)
mean(men)

nrow(centers)
# null hypothesis average NBA age is 30

n <- 100

p.value <- function(n, x, mu) {
  x.sd <- sd(x) / sqrt(n)

  x.mean <- mean(sample(x, n))
  z <- (x.mean - mu) / x.sd

  p <- 1 - pnorm(z)
  data.frame(x.mean = x.mean, x.sd, z = z, p = p)
}

lapply(rep(50, 20), function(n) p.value(n, centers$fgp, 0.43))

head(nba)

str(nba)

