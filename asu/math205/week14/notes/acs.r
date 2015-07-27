root.dir <- '~/Documents/U/ubb/sccc/math146'
data.dir <- paste(root.dir, "data/acs", sep = "/")
setwd(data.dir)

acs <- read.csv("ss12pwa.csv")
acs <- subset(acs, !is.na(wagp) & wagp > 0)

no.degree <- subset(acs, schl < 20)
degree <- subset(acs, schl >= 20)

mean(acs$age)
sd(acs$age)

sd(acs$wagp)
mean(acs$wagp)
mean(women)
sd(women)
mean(men)

# null hypothesis: school doesn't help you make more money

z.test <- function(n, x, sigma, mu.0) {
  x.sd <- sigma / sqrt(n)

  x.mean <- mean(sample(x, n))
  z <- (x.mean - mu.0) / x.sd

  p <- 1 - pnorm(z)
  data.frame(x.mean = x.mean, x.sd, z = z, p = p)
}

n <- 500
mu.0 <- mean(degree$wagp)

mu.0

mean(no.degree$wagp)
mean(degree$wagp)
sd(acs$wagp) / sqrt(500)

lapply(rep(n, 10), function(n) z.test(n, degree$wagp, sd(acs$wagp), mean(acs$wagp)))

(62555 - 46122) / 2329

plot <- ggplot(data = subset(acs), aes(x = agep)) + 
  geom_histogram(binwidth = 5, fill = "lightblue", color = "black") +
  labs(x = "Age") +
  theme(axis.title.y = element_blank())
print(plot)
