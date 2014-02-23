
root.dir <- '~/Documents/U/ubb/sccc/math146'
notes.dir <- paste(root.dir, 'week06/notes', sep = '/')
figures.dir <- paste(notes.dir, 'figures', sep = '/')
data.dir <- paste(root.dir, 'data', 'stop_and_frisk', sep = '/')

setwd(data.dir)

sqf <- read.delim('sqf_2012.csv', header = TRUE, sep = ',')

str(sqf)
sqf <- subset(sqf, !is.na(sex) & !is.na(race))
length(sqf$sex)

by.sex <- ddply(sqf, .(sex), summarize, pct = length(sex) / nrow(sqf))
by.race <- ddply(sqf, .(race, age.category), summarize, count = length(race))
by.age <- ddply(sqf, .(age.category), summarize, 
                length(
                pct = round(length(age.category) / nrow(sqf), 2))

by.race.c <- cast(by.race, race ~ age.category, value = .(count), fun = sum, 
                  margins = T)

by.race.c

nrow(sqf) / 365

classify.age <- function(age) {
  if (age < 15) {
    '0to14'
  } else if (age < 20) {
    '15to19'
  } else if (age < 25) {
    '20to24'
  } else if (age < 30) {
    '25to29'
  } else if (age < 35) {
    '30to34'
  } else if (age < 40) {
    '35to39'
  } else if (age < 45) {
    '40to44'
  } else if (age < 50) {
    '45to49'
  } else if (age < 60) {
    '50to59'
  } else if (age < 60) {
    '55to59'
  } else {
    'over60'
  }
}

sqf$age.category <- sapply(sqf$age, classify.age)

sum(subset(by.age, age < 40, pct))
