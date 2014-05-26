root.dir <- '~/Documents/U/ubb/sccc/math146'
data.dir <- paste(root.dir, "data/acs", sep = "/")
setwd(data.dir)

acs <- read.csv("ss12pwa.csv")
acs <- subset(acs, !is.na(wagp) & wagp > 0 & wagp < 150000)


women <- subset(acs, sex == 2)
men <- subset(acs, sex == 1)
no.degree <- subset(acs, schl < 20)

mean(acs$age)
sd(acs$age)

sd(acs$wagp)
mean(acs$wagp)
mean(women)
sd(women)
mean(men)

# null hypothesis: school doesn't help you make more money

n <- 100

p.value <- function(n, x, mu) {
  x.sd <- sd(x) / sqrt(n)

  x.mean <- mean(sample(x, n))
  z <- (x.mean - mu) / x.sd

  p <- 1 - pnorm(z)
  data.frame(x.mean = x.mean, x.sd, z = z, p = p)
}

samples <- lapply(rep(samplesPerTrial, trials), function(n) sample(rush$yds, n))
lapply(rep(100, 20), function(n) p.value(n, acs$age, 38))

str(women)

plot <- ggplot(data = subset(acs), aes(x = agep)) + 
  geom_histogram(binwidth = 5, fill = "lightblue", color = "black") +
  labs(x = "Age") +
  theme(axis.title.y = element_blank())
print(plot)
