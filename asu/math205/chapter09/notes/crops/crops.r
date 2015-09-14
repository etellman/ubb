root.dir <- '~/Documents/U/ubb/asu/math205'
crops.dir <- paste(root.dir, 'week09', 'notes', 'crops', sep = '/')
setwd(crops.dir)

crops <- data.table(read.csv('crops.csv'))

crops.random <- select(crops, plot, treatment = random, yield.)
crops.regular <- select(crops, plot, treatment = regular, yield)

crops.regular

crops.summary <- function(df) {
  result <- ddply(df, .(treatment), summarize, 
        yield.mean = round(mean(yield)), 
        delta = yield.mean - 558, 
        sd = sd(yield))

  result %>% 
    mutate(sd = round(sd)) %>% 
    rename(replace = c('yield.mean' = 'mean'))
}

random.s <- crops.summary(crops.random)
regular.s <- crops.summary(crops.regular)

?rename

tex.table(crops)
tex.table(random.s)

sd(random.s$mean) 
mean(random.s$sd)

sd(regular.s$mean) 
mean(regular.s$sd)

subset(crops.random, treatment == 'A')
mean(s$yield.sd)
str(s)

random.s <- crops.summary(crops.random)
regular.s <- crops.summary(crops.regular)

random.s
regular.s

round(sd(random.s$mean), 2)
round(sd(regular.s$mean), 2)

round(mean(random.s$sd), 2)
round(mean(regular.s$sd), 2)

# potatoes
potatoes <- read.csv('potatoes.csv', strip.white = T)

plot <- ggplot(potatoes, aes(x = strip, y = yield, 
                             shape = variety, colour = variety,
                             linetype = variety)) + 
  geom_point() +
  theme_ubb +
  theme(legend.position = 'top') +
  stat_smooth(method = lm, se = F)
  
print(plot)

save.plot(plot, dir = '.', 'potatoes.png', width = 4, height = 4)
with(potatoes, cor(strip, yield))
mean(potatoes)

?strip

potatoes <- arrange(potatoes, variety)
potatoes

p.m <- ddply(potatoes, .(variety), summarize, 
             yield = mean(yield), sd = round(sd(yield), 3))

p.m

tex.table(p.m)
str(potatoes$variety)

subset(potatoes, variety == 'C')

sd(c(1,1,1,3))
