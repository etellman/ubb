library(ggplot2) 
library(reshape)
library(xtable)

# apply your skills 6.8
dp <- read.delim("death_penalty.dat", header = TRUE, sep = ',')

contingencyTable <- xtabs(data = dp, count ~ defendent + outcome)
addmargins(contingencyTable, 2)
xtable(prop.table(contingencyTable, 1))

# black kill black: 6%
# black kill white: 21%
# black overall: 10%

# white kill black: 0%
# white kill white: 14%
# white overall: 12%

countByRace <- function(race) xtabs(data = subset(dp, defendent == race), count ~ victim + outcome)
probabilityByRace <- function(race) prop.table(xtabs(data = subset(dp, defendent == race), count ~ victim + outcome), 1)

outcomeByRace <- function(race) { 
  list(
    countTable = addmargins(countByRace(race)),
    contingencyTable = prop.table(countByRace(race), 1),
    byDefendentRace = sum(subset(dp, defendent == race & outcome == "death", count)) / sum(subset(dp, defendent == race, count)) ,
    byVictimRace = sum(subset(dp, victim == race & outcome == "death", count)) / sum(subset(dp, victim == race, count))
  )
}

lapply(c( "black", "white"), outcomeByRace)

xtabs(data = dp, count ~ victim + outcome + defendent)
