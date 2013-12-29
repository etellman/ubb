library(ggplot2) 
library(reshape)
library(xtable)

# apply your skills 6.8
dp <- read.delim("death_penalty.dat", header = TRUE, sep = '\t')

sink("r.tex")
print(xtable(dp))
sink()

blackDefendant <- data.frame(prop.table(xtabs(data = subset(dp, defendant == "black"), count ~ victim + outcome), 1))
whiteDefendant <- data.frame(prop.table(xtabs(data = subset(dp, defendant == "white"), count ~ victim + outcome), 1))

whiteDefendant$defendant = c("white", "white", "white", "white")

allDefendents <- rbind(blackDefendant, whiteDefendant)

allDefendents <- subset(allDefendents, outcome == "death", c(defendant, victim, Freq))

withMargins <- addmargins(dpTable, 2)
percentages <- prop.table(dpTable, 1)

xtable(withMargins, digits = 0)
print(percentages, digits = 1)

countByRace <- function(race) xtabs(data = subset(dp, defendant == race), count ~ victim + outcome)
probabilityByRace <- function(race) prop.table(xtabs(data = subset(dp, defendant == race), count ~ victim + outcome), 1)

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

RSiteSearch("merge dataframes")
