# data.r

math205.dir <- '~/Documents/U/ubb/asu/math205'
data.dir <- paste(math205.dir, 'data', 'prison_data', sep = '/')
setwd(data.dir)

week.dir <- paste(math205.dir, 'week01', sep = '/')
figure.dir <- paste(week.dir, 'notes', 'figures', sep = '/')

us <- read.csv('us_population.csv', header = T, comment.char = '#', strip.white = T)
pbs <- read.csv('prisoners_by_state.csv', header = T, comment.char = '#', strip.white = T)
pbs.m <- melt(pbs, id = c('state', 'region', 'year'))

world.rates <- read.csv('world_incarceration_rates.csv', header = T, strip.white = T)

# pbs
# sink("data_2010.tex")
# xtable(subset(pbs, year == 2010))
# sink()

