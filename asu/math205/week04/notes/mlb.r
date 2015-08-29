math205.dir <- '~/Documents/U/ubb/asu/math205'

week.dir <- paste(math205.dir, 'week04', sep = '/')
figure.dir <- paste(week.dir, 'notes', 'figures', 'mlb', sep = '/')

data.dir <- paste(math205.dir, 'data', sep = '/')
mlb.dir <- paste(data.dir, 'mlb', sep = '/')
setwd(mlb.dir)

pitching <- read.delim("leagues_MLB_2014-standard-pitching_teams_standard_pitching.csv", 
                     header = T, sep = ',', strip.white = T)

batting <- read.delim("leagues_MLB_2014-standard-batting_teams_standard_batting.csv", 
                     header = T, sep = ',', strip.white = T)


set.seed(10)
pitching.sample <- pitching[sample(nrow(pitching), 8),]
pitching.sample <- subset(sample, select = c(Tm, PAge, WHIP, RA.PER.G))

whip.sample <- within(subset(sample, select = c(WHIP, RA.PER.G)), {
  RA.PER.G.rank <- rank(RA.PER.G)
  WHIP.rank <- rank(WHIP)
})

age.sample <- within(subset(sample, select = c(Tm, PAge, RA.PER.G)), {
  d2 <- (rank(RA.PER.G) - rank(PAge))^2
  RA.PER.G.rank <- rank(RA.PER.G)
  PAge.rank <- rank(PAge)
})
age.sample

sink("~/tmp/r.tex")
  print(xtable(pitching.sample, digits = 3), include.rownames = F, booktabs = T)
sink()

sx <- sd(c(10, 20, 30, 40, 50))
sy <- sd(c(1, 2, 3, 4, 5))

100/(4 * (sx * sy))

with(age.sample, cor.test(PAge, RA.PER.G, method = "spearman", exact = F))
with(sample, cor.test(WP, PAge, method = "pearson"))

sum(age.sample$d2)

plot <- ggplot(whip.sample, aes(x = WHIP, y = RA.PER.G)) + 
  geom_point() +
  theme_ubb + 
  labs(x = "WHIP", y = "Runs/Game") 
print(plot)

save.plot(plot, figure.dir, "whip_vs_rg_sample.pdf")

batting.r <- with(batting, 
  data.frame(
    ba.vs.rg = cor(BA, R.G),
    bb.vs.rg = cor(BB, R.G),
    slg.vs.rg = cor(SLG, R.G),
    lob.vs.rg = cor(LOB, R.G),
    ops.vs.rg = cor(OPS, R.G)
  )
)
batting.r <- round(batting.r, 4)
batting.r

str(batting)
subset(batting, BA > 0.27)

plot <- ggplot(batting, aes(x = BB, y = R.G)) + 
  geom_point() +
  theme_ubb + 
  labs(x = "Walks", y = "Runs/Game") 
print(plot)

save.plot(plot, figure.dir, "bb_vs_rg.pdf")

batting.r

pitching.r <- with(pitching, 
  data.frame(
    whip.vs.era = cor(WHIP, ERA),
    whip.vs.wl = cor(WHIP, WL),
    bb.vs.era = cor(BB, ERA),
    whip.vs.era = cor(WHIP, ERA),
    so.vs.era = cor(SO, ERA),
    rg.vs.wl = cor(ERA, WL)
  )
)
pitching.r <- round(pitching.r, 4)

pitching.r

str(pitching)
pitching.selected <- subset(pitching, select = c(Tm, ERA, WHIP, BB, SO, WL))
batting.selected <- subset(batting, select = c(Tm, BB, BA, SLG, OPS, R.G))

sink("~/tmp/r.tex")
  print(xtable(batting.selected, digits = 3), booktabs = T)
sink()

pitching.selected
pitching.r
