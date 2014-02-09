root.dir <- "~/Documents/U/ubb/sccc/math146"
notes.dir <- paste(root.dir, "week04/notes", sep = "/")
figures.dir <- paste(notes.dir, "figures", sep = "/")

setwd(notes.dir)
crime <- read.delim("crime.csv", header = TRUE, sep = ',', strip.white = TRUE)

before.1991 <- subset(crime, year <= 1991)
after.1991 <- subset(crime, 1991 <= year & year < 2007)
between.1980.and.2000 <- subset(crime, 1980 < year & year < 2000)

crime.s <- crime[sample(nrow(crime), 15),]

plot <- ggplot(after.1991, aes(x = crime.rate, y = incarceration.rate)) + 
  geom_point() +
  labs(x = "Crime Rate", y = "Incarceration Rate") +
  ggtitle("Crime vs. Incarceration Rate (1978 - 1991)")
print(plot)

cor(subset(after.1991, select = c(crime.rate, incarceration.rate)))

# adds scaled columns to a crime/incarceration rate data frame
crime.scale <- function(df) {
  crime.rate.z <- scale(df$crime.rate)
  incarceration.rate.z <- scale(df$incarceration.rate)

  cbind(df, crime.rate.z, incarceration.rate.z)
}

sink(paste(notes.dir, "r.tex", sep = "/"))
  xtable(crime.scale(after.1991))
  # xtable(as.matrix(nfl.stats), digits = 0)
sink()

crime.s.m <- melt(crime.s, id = "year")
crime.s.stats <- cast(crime.s.m, variable ~ ., function(x) c(m = mean(x), s = sd(x)))

crime.s.stats

sink(paste(notes.dir, "r.tex", sep = "/"))
  xtable(crime.s.stats)
sink()

with(before.1991, cor(crime.rate, incarceration.rate))
with(after.1991, cor(crime.rate, incarceration.rate))

plot <- ggplot(after.1991, aes(x = crime.rate, y = incarceration.rate)) + 
  geom_point() +
  labs(x = "Crime Rate", y = "Incarceration Rate") +
  ggtitle("Crime vs. Incarceration Rate (1992 - 2012)")
print(plot)

plot <- ggplot(after.1991, aes(x = crime.rate, y = incarceration.rate)) + 
  geom_point() +
  labs(x = "Crime Rate", y = "Incarceration Rate") +
  coord_flip() +
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

max(crime$crime.rate)
subset(crime, crime.rate == max(crime.rate))
