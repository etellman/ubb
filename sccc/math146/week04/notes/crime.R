root.dir <- "~/Documents/U/ubb/sccc/math146"
notes.dir <- paste(root.dir, "week04/notes", sep = "/")
figures.dir <- paste(notes.dir, "figures", sep = "/")

setwd(notes.dir)
crime <- read.delim("crime.csv", header = TRUE, sep = ',', strip.white = TRUE)

before.1991 <- subset(crime, year <= 1991)
after.1991 <- subset(crime, 1991 <= year & year < 2007)
crime.s <- crime[sample(nrow(crime), 15),]

plot <- ggplot(after.1991, aes(x = crime.rate, y = incarceration.rate)) + 
  geom_point() +
  labs(x = "Crime Rate", y = "Incarceration Rate") +
  ggtitle("1991-2006")
print(plot)

ggsave(paste(figures.dir, "crime/crime_vs_incarceration_1991-2006.eps", sep = "/"),
             width = 4, height = 2.5)

cor(subset(after.1991, select = c(crime.rate, incarceration.rate)))
cor(subset(before.1991, select = c(crime.rate, incarceration.rate)))
cor(subset(crime.s, select = c(crime.rate, incarceration.rate)))

# adds scaled columns to a crime/incarceration rate data frame
crime.scale <- function(df) {
  crime.rate.z <- scale(df$crime.rate)
  incarceration.rate.z <- scale(df$incarceration.rate)

  cbind(df, crime.rate.z, incarceration.rate.z)
}

sink(paste(notes.dir, "r.tex", sep = "/"))
  xtable(crime.scale(before.1991))
  # xtable(as.matrix(nfl.stats), digits = 0)
sink()

crime.s

after.1991.m <- melt(after.1991, id = "year")
after.1991.stats <- cast(after.1991.m, variable ~ ., function(x) c(m = mean(x), s = sd(x)))

sink(paste(notes.dir, "r.tex", sep = "/"))
  xtable(after.1991.stats)
sink()

with(before.1991, cor(crime.rate, incarceration.rate))
with(after.1991, cor(crime.rate, incarceration.rate))

plot <- ggplot(after.1991, aes(x = crime.rate, y = incarceration.rate)) + 
  geom_point() +
  labs(x = "Crime Rate", y = "Incarceration Rate") +
  ggtitle("Crime vs. Incarceration Rate (1992 - 2012)")
print(plot)

plot <- ggplot(crime, aes(x = year, y = incarceration.rate)) + 
  geom_line() +
  labs(x = "Year", y = "Incarceration Rate") +
  ggtitle("Incarceration Rate")
print(plot)

plot <- ggplot(crime, aes(x = year, y = crime.rate)) + 
  geom_line() +
  labs(x = "Year", y = "Crime Rate") +
  ggtitle("Crime Rate")
print(plot)

