
root.dir <- "~/Documents/U/ubb/sccc/math146"

data.dir <- paste(root.dir, "data/bps/PC-Text", sep = "/")
hw9.data.dir <- paste(data.dir, "ch09", sep = "/")

hw.dir <- paste(root.dir, "week09/hw", sep = "/")
figures.dir <- paste(hw.dir, "figures", sep = "/")

setwd(hw9.data.dir)
￼
# exercise 35
ex35 <- data.frame(spanish = .26, french = 0.09, german = 0.03, other = 0.03, none = 0.59)
ex35.melted <- melt(ex35)

sum(subset(ex35.melted, variable %in% c('french', 'german', 'spanish'), 
           select = 'value'))

# exercise 36
ex36 <- data.frame(white = 0.19, silver = 0.18, black = 0.16, red = 0.13, 
                   gray = 0.12, blue = 0.12)
ex36.melted <- melt(ex36)

sum(subset(ex36.melted, variable %in% c('silver', 'white'), select = 'value'))

sum(ex36.melted$value)

# exercise 40
ex40 <- rbind(data.frame(race = 'asian', hispanic = T, probability = 0.001),
              data.frame(race = 'asian', hispanic = F, probability = 0.044),

              data.frame(race = 'black', hispanic = T, probability = 0.006),
              data.frame(race = 'black', hispanic = F, probability = 0.124),

              data.frame(race = 'white', hispanic = T, probability = 0.139),
              data.frame(race = 'white', hispanic = F, probability = 0.674),

              data.frame(race = 'other', hispanic = T, probability = 0.003),
              data.frame(race = 'other', hispanic = F, probability = 0.009)
             )

melt(ex40, id = c('race', 'hispanic'))

cast(ex40, race ~ hispanic)

sum(subset(ex40, hispanic == T | race != 'white', select = 'probability'))

pnorm(0.72, .56, .019)
diff(pnorm(c(8.9, 9.1), 9, 0.075))

p <- pnorm(c(8.9, 9.1), 9, 0.075)
print(p)
print(diff(p))

24/10000

