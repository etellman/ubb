
root.dir <- '~/Documents/U/ubb/sccc/math146'
notes.dir <- paste(root.dir, 'week06/notes', sep = '/')
figures.dir <- paste(notes.dir, 'figures', sep = '/')
data.dir <- paste(root.dir, 'data', 'stop_and_frisk', sep = '/')

setwd(data.dir)

sqf <- read.delim('sqf_2012.csv', header = TRUE, sep = ',')
sqf <- subset(sqf, !is.na(sex) & !is.na(race))

nyc <- read.delim('DEC_10_DP_DPDP1_with_ann.csv', header = TRUE, sep = ',')


by.sex <- ddply(sqf, .(sex), summarize, pct = length(sex) / nrow(sqf))

by.race.and.age <- ddply(sqf, .(race, age.category), summarize, count = length(race))
by.race.and.age.c <- cast(by.race.and.age, race ~ age.category, 
                          value = .(count), fun = sum, margins = T)

by.race.and.action <- ddply(sqf, .(race), summarize, 
                             total = length(race), 
                             frisked = sum(frisked),
                             searched = sum(searched), 
                             force = sum(pf_hands + pf_wall + pf_grnd + pf_drwep
                                         + pf_ptwep + pf_baton + pf_hcuff
                                         + pf_pepsp + pf_other)
                            )

by.race.and.action
outcomes <- ddply(sqf, .(cs_objcs, cs_descr), summarize, 
                             frisked = sum(frisked),
                             searched = sum(searched)
                            )
outcomes

by.race.and.outcome <- ddply(sqf, .(race), summarize, 
                             total = length(race), 
                             contraband = sum(contrabn),
                             gun = sum(pistol, riflshot, machgun),
                             arrest = sum(arstmade))

by.race.and.outcome

reasons <- ddply(sqf, .(), summarize,
                     suspicous.object = sum(cs_objcs),
                     description = sum(cs_descr),
                     casing = sum(cs_casng),
                     lookout = sum(cs_lkout),
                     clothing = sum(cs_cloth),
                     drug.trans = sum(cs_drgtr),
                     furtive = sum(cs_furtv),
                     violent.crime = sum(cs_vcrim),
                     suspicous.bulge = sum(cs_bulge),
                     other = sum(cs_other)
)

t(reasons)

cast(sqf.reasons.m, variable ~ ., fun = sum)

by.race.and.outcome.c <- cast(by.race.and.outcome, .(race), fun = sum, margins = T)

sink(paste(notes.dir, 'r.tex', sep = '/'))
  xtable(t(nyc))
sink()

nrow(sqf) / 365

classify.age <- function(age) {
  if (age < 15) {
    '0to14'
  } else if (age < 25) {
    '15to24'
  } else if (age < 35) {
    '25to34'
  } else if (age < 45) {
    '35to44'
  } else if (age < 55) {
    '45to54'
  } else {
    'over55'
  }
}

classify.reason <- function(df) {
  if (df$cs_objcs) {
    'suspicious'
  } else if (age < 25) {
    '15to24'
  } else if (age < 35) {
    '25to34'
  } else if (age < 45) {
    '35to44'
  } else if (age < 55) {
    '45to54'
  } else {
    'over55'
  }
}
sqf$age.category <- sapply(sqf$age, classify.age)

sum(subset(by.age, age < 40, pct))

str(nyc)

with(nyc, sum(2795, 57841, 148676))
