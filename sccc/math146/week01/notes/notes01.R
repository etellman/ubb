library(ggplot2) 
library(reshape)
library(xtable)

# death penalty
# apply your skills 6.8
dp <- read.delim("death_penalty.dat", header = TRUE, sep = '\t')

# percentage of cases that resulted in the death penalty
death.percent <- function(df) {
  round(100 * dp.count(df) / sum(df$count))
}

dp.count <- function(df) {
  sum(subset(df, outcome == "death", select = count))
}

dp.summary <- function(df) {
  c(Cases = sum(df$count), DP = dp.count(df), "Percent" = death.percent(df))
}

df <- ddply(dp, c(Victim = "victim"), dp.summary)

df
sink("~/Documents/U/ubb/sccc/math146/week01/notes/r.tex")
xtable(df, digits = 0)
sink()

plot <- ggplot(df, aes(x = reorder(victim, -dp.percent), y = dp.percent, fill = defendant)) + 
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  ggtitle("Louisiana Incarceration Rate") +
  labs(x = "Victim", y = "Death Penalty Percentage")
print(plot)

ggsave("~/Documents/U/ubb/sccc/math146/week01/notes/figures/death_penalty.eps", width = 5, height = 3)

# prison population
pbs <- read.delim("prisoners_by_state.txt", header = TRUE, sep = '\t')
# write.csv(pbs, "prisoners_by_state.txt")

xtabs(data = fiveYear, prison.population ~ region + year)

edit(pbs)

la <- xtabs(data = subset(fiveYear, state == "Louisiana"), incarceration.rate ~ year)

rate.by.region <- as.data.frame(as.list(by(pbs2010, pbs2010$region, function(df) { 100000 * sum(df$prison.population, na.rm = TRUE) / sum(df$state.population, na.rm = TRUE) })))

pbs2010 <- subset(pbs, year == 2010)
pbs2010 <- pbs2010[order(pbs2010$prison.population, decreasing = TRUE),]
pbs2010[1,]

plot <- ggplot(melt(la), aes(x = year, y = value)) + 
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +
  # geom_point() + coord_flip() +
  labs(x = "Year", y = "Incarceration Rate") +
  ggtitle("Louisiana Incarceration Rate")
print(plot)

ggsave("la_rate.eps", width = 5, height = 3)

str(pbs)

plot <- ggplot(data = subset(pbs, year == 2010 & state != "District of Columbia"), aes(x = prison.population / 1000)) + 
  geom_histogram(fill = "lightblue", color = "black", binwidth = 1) +
  labs(x = "Incarceration Rate", y = "Number of States") +
  ggtitle("2010 Incarceration Rate")
print(plot)

ggsave("~/Documents/U/ubb/sccc/math146/week01/notes/figures/population_histogram_2010_small_bins.eps", width = 5, height = 3)

?read.delim

world.rates <- read.delim("data/world_incarceration_rates.txt", header = TRUE, strip.white = TRUE, sep = '\t')
 
sample.rates <- subset(world.rates, country == "United States" | country == "Cuba" | country == "Russia" | country == "England and Wales" | country == "France" | country == "Japan"| country == "India" | country == "South Africa")

plot <- ggplot(rate.by.region.f, aes(x = reorder(region, -rate), y = rate)) + 
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +
  # coord_flip() +
  labs(x = "Region", y = "Incarceration Rate") +
  ggtitle("Regional Incarceration Rates")
print(plot)

plot <- ggplot(data = world.rates, aes(x = rate)) + 
  geom_histogram(fill = "lightblue", color = "black", binwidth = 50) +
  labs(x = "Incarceration Rate", y = "Number Countries") +
  ggtitle("World Incarceration Rates")
print(plot)

ggsave("rate_by_region.eps", width = 5, height = 3)

median(world.rates$rate)

pbs.m <- melt(pbs, id = c("state", "region", "sub.region", "year"))
str(pbs.m)

rates <- with(pbs, rate(prison.population, state.population))
str(rates)

pbs <- edit(cbind(pbs, state.rate = rates))


pbs.m <- subset(pbs2010.m, variable != "year" & variable != "X")

by.state <- cast(pbs.m, year + state ~ variable, sum)

str(by.state)

rate <- function(prison.population, total.population) {
  round(100000 * prison.population / total.population)
}

rate.by.region <- rate(population.by.region$prison.population, population.by.region$state.population)

rate.by.region.f <- data.frame(region = population.by.region$region, rate = rate.by.region)

rate.by.region

as.data.frame(as.list(by(pbs2010, pbs2010$region, function(df) { 100000 * sum(df$prison.population, na.rm = TRUE) / sum(df$state.population, na.rm = TRUE) })))
