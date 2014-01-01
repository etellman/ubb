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
pbs <- read.delim("prisonersByState.txt", header = TRUE, sep = '\t')


xtabs(data = fiveYear, prison.population ~ region + year)

washington <- xtabs(data = subset(fiveYear, state == "Washington"), incarceration.rate ~ year)

rate.by.region <- as.data.frame(as.list(by(pbs2010, pbs2010$region, function(df) { 100000 * sum(df$prison.population, na.rm = TRUE) / sum(df$state.population, na.rm = TRUE) })))

pbs1980 <- subset(pbs, year == 1980)
pbs1980 <- pbs1980[order(pbs1980$prison.population, decreasing = TRUE),]
pbs1980[1,]

plot <- ggplot(pbs2010[1:5,], aes(x = reorder(state, -prison.population), y = prison.population / 1000)) + 
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +
  # geom_point() + coord_flip() +
  labs(x = "State", y = "Population (thousands)") +
  ggtitle("2010 Top Five by Prison Population")
print(plot)

ggsave("top_five_2010.eps", width = 5, height = 3)

plot <- ggplot(data = subset(pbs, year == 2010 & state != "District of Columbia"), aes(x = incarceration.rate)) + 
  geom_histogram(fill = "lightblue", color = "black", binwidth = 100) +
  labs(x = "Incarceration Rate", y = "Number of States") +
  ggtitle("2010 Incarceration Rate")
print(plot)

ggsave("2010_rate_histogram.eps", width = 5, height = 3)


