
math205.dir <- '~/Documents/U/ubb/asu/math205'
data.dir <- paste(math205.dir, 'data', 'bps', 'PC-Text', 'ch17', sep = '/')
setwd(data.dir)

week.dir <- paste(math205.dir, 'chapter17', sep = '/')
figure.dir <- paste(week.dir, 'hw', 'figures', sep = '/')
￼
# returns data frame containing values needed to do t-test analysis
t.summary <- function(x) {
  round(data.frame(s = sd(x), m = mean(x), n = length(x)), 4)
}

t.value <- function(mean, mu, s, n) {
  se <- s/sqrt(n)
  t.value.se(mean, mu, se)
}

t.value.se <- function(mean, mu, se) {
  round((mean - mu)/se, 4)
}

confidence <- function(mean, t.star, s, n) {
  se <- s/sqrt(n)
  confidence.se(mean, t.star, se)
}

confidence.se <- function(mean, t.star, se) {
  delta <- t.star * se
  round(c(mean - delta, mean + delta), 4)
}

# ex 25
m1 <- 0.08
m2 <- 0.35
n <- 12
s <- 0.37

t.value(c(m1, m2), 0, s, 12)

# ex 26
m <- 26.8
n <- 654
s <- 7.42

x <- rnorm(n, m, s)
x
confidence(m, 1.984, s, n)
t.test(x)

# ex 27
m <- 240
n <- 1470
se <- 1.1
t.star <- 2.581
s <- se * sqrt(n)
mu <- 243

x <- rnorm(n, m, s)

confidence.se(m, t.star, se)
t.value.se(m, mu, se)

t.test(x, mu = 243, conf.level = 0.95, alternative = "less")

# ex 28
m <- 114.9
s <- 9.3
n <- 27
t.star <- 2.056

confidence(m, t.star, s, n)

# ex 30
ex30 <- read.delim("ex17-30.dat", header = TRUE, sep = '\t')
t.test(ex30, mu = 1, alternative = "greater", conf = 0.99)

xtable(t.summary(ex30$Cond), booktabs = T)

print(xtable(t.summary(ex30$Cond), digits = 4), 
      booktabs = TRUE, include.rownames = FALSE)
s <- sd(ex30$Cond)
m <- mean(ex30$Cond)
n <- nrow(ex30)
mu <- 1
t.star <- 2.228
t.value(m, mu, s, nrow(ex30))

confidence(m, t.star, s, n)

plot <- ggplot(ex30, aes(x = Cond)) + 
  geom_histogram(binwidth = 0.03, color = "black", fill = "lightblue") +
  theme_ubb + theme(axis.title.y = element_blank()) +
  labs(x = "Conductivity") 
print(plot)

save.plot(plot, figure.dir, 'ex30.pdf')

# ex 31
ex31 <- read.delim("ex17-31.dat", header = TRUE, sep = '\t')
str(ex31)

plot <- ggplot(ex31, aes(x = Count)) + 
  geom_histogram(binwidth = 3, color = "black", fill = "lightblue") +
  theme_ubb + theme(axis.title.y = element_blank()) +
  labs(x = "Blissymbols") 
print(plot)

save.plot(plot, figure.dir, 'ex31.pdf')

print(xtable(t.summary(ex31$Count), digits = 4), 
      booktabs = TRUE, include.rownames = FALSE)

df <- t.summary(ex31$Count)
df

t.star <- 1.796
confidence(df$m, t.star, df$s, df$n)

t.test(ex31, conf = 0.90)

# ex 33
ex33 <- read.delim("ex17-33.dat", header = TRUE, sep = '\t')
ex33$diff <- ex33$Ctrl - ex33$Exp

no.outliers <- subset(ex33, diff < 20, select = c(diff))
print(xtable(t.summary(no.outliers$diff), digits = 4), 
      booktabs = TRUE, include.rownames = FALSE)

plot <- ggplot(ex33, aes(x = diff)) + 
  geom_histogram(binwidth = 5, color = "black", fill = "lightblue") +
  theme_ubb + theme(axis.title.y = element_blank()) +
  labs(x = "control - experiment") 
print(plot)

save.plot(plot, figure.dir, 'ex33.pdf')

df <- t.summary(ex33$diff)

df
t.value(df$m, 0, df$s, df$n)

t.test(ex33$diff, alternative = "greater")
t.test(subset(ex33, diff < 20, select = c(diff)), alternative = "greater")

# ex 35
ex35 <- read.delim("ex17-35.dat", header = TRUE, sep = '\t')
str(ex35)

print(xtable(t.summary(ex35$Days), digits = 4), 
      booktabs = TRUE, include.rownames = FALSE)

plot <- ggplot(ex35, aes(x = Days)) + 
  geom_histogram(binwidth = 0.5, color = "black", fill = "lightblue") +
  theme_ubb + theme(axis.title.y = element_blank()) +
  labs(x = "Days") 
print(plot)

save.plot(plot, figure.dir, 'ex35.pdf')

df <- t.summary(ex35$diff)

df
t.value(df$m, 0, df$s, df$n)

t.test(ex35$Days, conf = 0.90)

# ex 38
ex38 <- read.delim("ex17-38.dat", header = TRUE, sep = '\t')
ex38$diff <- ex38$Killrm - ex38$Procrm
str(ex38)

print(xtable(t.summary(ex38$diff), digits = 4), 
      booktabs = TRUE, include.rownames = FALSE)

plot <- ggplot(ex38, aes(x = diff)) + 
  geom_histogram(binwidth = 500, color = "black", fill = "lightblue") +
  theme_ubb + theme(axis.title.y = element_blank()) +
  labs(x = "kill - processing") 
print(plot)

save.plot(plot, figure.dir, 'ex38.pdf')

t.summary(ex38$Killrm)
t.summary(ex38$Procrm)
t.summary(ex38$diff)

t.test(ex38$diff, conf = 0.90)

# ex 44
ex44 <- read.delim("ta17-04.dat", header = TRUE, sep = '\t')
ex44$diff <- ex44$Fund - ex44$Eafe
head(ex44)

print(xtable(t.summary(ex44$diff), digits = 4), 
      booktabs = TRUE, include.rownames = FALSE)

plot <- ggplot(ex44, aes(x = diff)) + 
  geom_histogram(binwidth = 5, color = "black", fill = "lightblue") +
  theme_ubb + theme(axis.title.y = element_blank()) +
  labs(x = "fund - benchmark") 
print(plot)

save.plot(plot, figure.dir, 'ex44.pdf')

t.test(ex44$diff, conf = 0.95)

# ex 45
ex45 <- read.delim("ta17-05.dat", header = TRUE, sep = '\t')
ex45$diff <- ex45$Left - ex45$Right

print(xtable(t.summary(ex45$diff), digits = 4), 
      booktabs = TRUE, include.rownames = FALSE)
mean(ex45$Right)/mean(ex45$Left)

plot <- ggplot(ex45, aes(x = diff)) + 
  geom_histogram(binwidth = 20, color = "black", fill = "lightblue") +
  theme_ubb + theme(axis.title.y = element_blank()) +
  labs(x = "left - right") 
print(plot)

save.plot(plot, figure.dir, 'ex45.pdf')

t.test(ex45$diff, conf = 0.90, alternative = "two.sided")

