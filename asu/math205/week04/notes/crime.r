math205.dir <- '~/Documents/U/ubb/asu/math205'

week.dir <- paste(math205.dir, 'week04', sep = '/')
notes.dir <- paste(week.dir, "notes", sep = "/")
figure.dir <- paste(notes.dir, 'figures', 'crime', sep = '/')
setwd(notes.dir)

crime <- read.delim("crime.csv", header = TRUE, sep = ',', strip.white = TRUE)

# add z-scores
add.z.scores <- function(df) {
  mean.cr <<- mean(df$crime.rate)
  sd.cr <<- sd(df$crime.rate)

  mean.ir <<- mean(df$incarceration.rate)
  sd.ir <<- sd(df$incarceration.rate)

  ddply(df, .(year), summarize, 
        year = year,
        crime.rate = crime.rate,
        incarceration.rate = incarceration.rate,
        crime.rate.z = round(z(crime.rate, mean = mean.cr, sd = sd.cr), 4),
        incarceration.rate.z = round(z(incarceration.rate, 
                                       mean = mean.ir, sd = sd.ir), 4))
}

with.z <- add.z.scores(before.1991)

print(xtable(with.z, booktabs = TRUE), include.rownames = FALSE)

before.1991 <- subset(crime, year <= 1991)
after.1991 <- subset(crime, 1991 <= year & year < 2007)

plot <- ggplot(before.1991, aes(x = crime.rate, y = incarceration.rate)) + 
  geom_point() +
  theme_ubb + 
  labs(x = "Crime Rate", y = "Incarceration Rate") 
print(plot)

save.plot(plot, figure.dir, "crime_vs_incarceration_before_1991.pdf")

plot <- ggplot(after.1991, aes(x = incarceration.rate, y = crime.rate)) + 
  geom_point() +
  theme_ubb + 
  labs(x = "Incarceration Rate", y = "Crime Rate") 
print(plot)

save.plot(plot, figure.dir, "incarceration_vs_crime_after_1991.pdf")

plot <- ggplot(subset(crime, year < 2007), 
               aes(x = incarceration.rate, y = crime.rate)) + 
  geom_point() +
  theme_ubb + 
  labs(x = "Incarceration Rate", y = "Crime Rate") 
print(plot)

save.plot(plot, figure.dir, "incarceration_vs_crime.pdf")

cor(subset(after.1991, select = c(crime.rate, incarceration.rate)))
cor(subset(before.1991, select = c(crime.rate, incarceration.rate)))

mean(before.1991$crime.rate)
mean(before.1991$incarceration.rate)

mean(before.1991$crime.rate)
  # xtable(as.matrix(nfl.stats), digits = 0)
sink()

?xtable
random.sample

after.1991.m <- melt(after.1991, id = "year")
after.1991.stats <- dcast(after.1991.m, variable ~ ., function(x) c(m = mean(x), s = sd(x)))

sink(paste(notes.dir, "r.tex", sep = "/"))
  xtable(after.1991.stats)
sink()

with(before.1991, cor(crime.rate, incarceration.rate))
with(after.1991, cor(crime.rate, incarceration.rate))

crime.m <- melt(crime, id = 'year')

str(crime.m)

plot <- ggplot(crime.m, aes(x = year, y = value)) + 
  facet_grid(. ~ variable) +
  geom_line() +
  theme_ubb + 
  labs(x = "Year", y = "Rate")
print(plot)

save.plot(plot, figure.dir, "rates_over_time.pdf")
