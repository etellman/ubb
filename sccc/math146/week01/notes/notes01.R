library(ggplot2) 
library(reshape)
library(xtable)

# death penalty
# apply your skills 6.8
dp <- read.delim("data/death_penalty.dat", header = TRUE, sep = '\t')

countByRace <- function(race) xtabs(data = subset(dp, defendant == race), count ~ victim + outcome)
probabilityByRace <- function(race) prop.table(countByRace(race), 1)
countByRaceWithTotals <- function(race) addmargins(countByRace(race), 2)

blackDefendant <- probabilityByRace("black")
whiteDefendant <- probabilityByRace("white")

toFrame <- function(table, race) {
  frame <- data.frame(table)
  frame$defendant = c(race, race, race, race)

  frame
}

blackDefendantFrame <- toFrame(blackDefendant, "black")
whiteDefendantFrame <- toFrame(whiteDefendant, "white")

blackDefendantFrame

allDefendents <- subset(rbind(blackDefendantFrame, whiteDefendantFrame), outcome == "death", c(defendant, victim, Freq))

plot <- ggplot(allDefendents, aes(x = reorder(victim, -Freq), y = Freq, fill = defendant)) + 
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  scale_fill_grey() +
  labs(x = "Victim", y = "Frequency")
print(plot)

ggsave("death_penalty.eps", width = 5, height = 3)

withMargins <- addmargins(dpTable, 2)
percentages <- prop.table(dpTable, 1)

xtable(withMargins, digits = 0)
print(percentages, digits = 1)


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

plot <- ggplot(data = subset(pbs, year == 2010 & state != "District of Columbia"), aes(x = incarceration.rate)) + 
  geom_histogram(fill = "lightblue", color = "black", binwidth = 100) +
  labs(x = "Incarceration Rate", y = "Number of States") +
  ggtitle("2010 Incarceration Rate")
print(plot)

ggsave("2010_rate_histogram.eps", width = 5, height = 3)

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
