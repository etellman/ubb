math205.dir <- '~/Documents/U/ubb/asu/math205'

week.dir <- paste(math205.dir, 'week05', sep = '/')
figure.dir <- paste(week.dir, 'notes', 'figures', 'mlb', sep = '/')

data.dir <- paste(math205.dir, 'data', sep = '/')
mlb.dir <- paste(data.dir, 'mlb', sep = '/')
setwd(mlb.dir)

pitching <- read.delim("leagues_MLB_2014-standard-pitching_teams_standard_pitching.csv", 
                     header = T, sep = ',', strip.white = T)

batting <- read.delim("leagues_MLB_2014-standard-batting_teams_standard_batting.csv", 
                     header = T, sep = ',', strip.white = T)


attach(pitching)
plot <- ggplot(pitching, aes(x = H, y = RA.PER.G)) + 
  geom_point() +
  # stat_smooth(method = "lm") +
  theme_ubb + 
  labs(x = "Hits", y = "Runs/Game") 
print(plot)

cor(H, RA.PER.G)

save.plot(plot, figure.dir, "rg_vs_hits.pdf", height = 5, width = 6)

summary.all <- data.frame(h.m = mean(H), h.sd = sd(H),  
                   rg.m = mean(RA.PER.G), rg.sd = sd(RA.PER.G))
summary.all <- round(summary.all, 2)

tex.table(summary.all)

categorize <- factor(1:5)
split(1:10, categorize)
xfact <- cut(x, 5)

?pretty

pretty(pitching$H)
hits <- data.frame(cbind(h = pitching$H, rg = pitching$RA.PER.G, h.level = cut(pitching$H, pretty(pitching$H))))
hits.summary <- ddply(hits, .(h.level), summarize, h = mean(h), rg = mean(rg))

?ddply

plot <- ggplot(hits.summary, aes(x = h, y = rg)) + 
  geom_point() +
  stat_smooth(method = "lm") +
  theme_ubb + 
  labs(x = "Hits", y = "Runs/Game") 
print(plot)


head(hits.summary)

pitching$H
table(cut(pitching$H, breaks = 5))
str(pitching)
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

readHanes <- function(seed, n) {
  hanes <- sasxport.get("/Users/edwint/Downloads/BMX_G.XPT")
  hanes <- subset(hanes, !is.na(bmxht) & !is.na(bmxwt))

  set.seed(seed)
  hanes <- hanes[sample(nrow(hanes), n),]

  hanes
}


hanes <- readHanes(11, 1000)

nrow(hanes)


levels <- pretty(hanes$bmxht, n = 10)
levels
h.and.w <- data.frame(cbind(height = hanes$bmxht, 
                           weight = hanes$bmxwt, 
                           height.level = cut(hanes$bmxht, levels)))

nrow(h.and.w)
h.and.w.s <- ddply(h.and.w, .(height.level), plyr::summarize, 
                   height = mean(height), weight = mean(weight))

nrow(h.and.w.s)

plot <- ggplot(h.and.w.s, aes(x = height, y = weight)) + 
  geom_point(alpha = 0.5) +
  # stat_density2d() +
  stat_smooth(method = "lm") +
  theme_ubb + 
  labs(x = "Height", y = "Weight") 

print(plot)
?stat_smooth


?stat_density2d
