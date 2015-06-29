math205.dir <- '~/Documents/U/ubb/asu/math205'

week.dir <- paste(math205.dir, 'week01', sep = '/')
figure.dir <- paste(week.dir, 'notes', 'figures', sep = '/')

data.dir <- paste(math205.dir, 'data', 'prison_data', sep = '/')

setwd(data.dir)

dp <- read.delim('death_penalty.dat', header = TRUE, sep = '\t')

# count of number of cases that resulted in the death penalty
dp.count <- function(df) {
  sum(subset(df, outcome == "death", select = count))
}

# percentage of cases that resulted in the death penalty
death.fraction <- function(df) {
  round(dp.count(df) / sum(df$count), 2)
}

# summarizes a death penalty data frame
dp.summary <- function(df) {
  c(Cases = sum(df$count), 
    Cases.Fraction = round(sum(df$count) / sum(dp$count), 2),
    DP = sum(subset(df, outcome == "death", select = count)), 
    DP.Fraction = round(dp.count(df) / sum(df$count), 2))
}

ddply(dp, .(), dp.summary)
ddply(dp, .(defendant), dp.summary)
ddply(dp, .(victim), dp.summary)
df <- ddply(dp, .(defendant, victim), dp.summary)

sink("/tmp/r.tex")
xtable(df, digits = 0)
sink()

# plot by victim
plot <- ggplot(df, aes(x = reorder(victim, -DP.Fraction), y = DP.Fraction, 
                       fill = defendant)) + 
  theme(legend.position = 'top') +
  scale_fill_discrete(name = "Defendant",
                      breaks=c("white", "black"),
                      labels = c('White', 'Black')) +
  scale_x_discrete(breaks = c("black", "white"), labels = c('Black', 'White')) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(x = "Victim", y = "Death Penalty")

print(plot)

save.plot(plot, 'death_penalty.pdf', figure.dir)
