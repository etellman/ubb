math205.dir <- "~/Documents/U/ubb/asu/math205"

week.dir <- paste(math205.dir, "week05", sep = "/")
figure.dir <- paste(week.dir, "notes", "figures", "hanes", sep = "/")

data.dir <- paste(math205.dir, "data", sep = "/")
hanes.dir <- paste(data.dir, "hanes", sep = "/")
setwd(hanes.dir)

readAllHanes <- function() {
  hanes <- sasxport.get("BMX_G.XPT")
  demo <- sasxport.get("DEMO_G.XPT")

  hanes <- merge(hanes, demo, by = "seqn")

  # select adult men
  hanes <- subset(hanes, !is.na(bmxht) & !is.na(bmxwt) & ridageyr > 18 & riagendr == 1)

  # convert to pounds and inches
  hanes$bmxwt = hanes$bmxwt * 2.20462
  hanes$bmxht = hanes$bmxht * 0.393701

  hanes
}

readHanes <- function(seed, n) {
  hanes <- readAllHanes()

  set.seed(seed)
  hanes <- hanes[sample(nrow(hanes), n),]

  hanes
}

hanes <- readAllHanes()

head(hanes)
h.and.w <- data.frame(
    cbind(height = hanes$bmxht, 
          weight = hanes$bmxwt, 
          age = hanes$ridageyr,
          height.level = cut(hanes$bmxht, seq(50, 80, 1))))


h.and.w$age
avg.h <- subset(h.and.w, round(height) == round(mean(h.and.w$height)) & age < 60)

nrow(h.and.w)
nrow(avg.h)

set.seed(2)
tall <- subset(h.and.w, 71 < height & height < 72, select = c(height, weight))
tall <- tall[sample(nrow(tall), 10),]
tall <- round(tall, 2)
mean(tall$weight)

set.seed(6)
short <- subset(h.and.w, 65 < height & height < 66, select = c(height, weight))
short <- short[sample(nrow(short), 10),]
short <- round(short, 2)
mean(short$weight)
tex.table(short)

with(h.and.w, {
  c(
    mean(weight) - cor(height, weight) * sd(weight),
    cor(height, weight)
  )
})

h.and.w.s <- ddply(h.and.w, .(height.level), plyr::summarize, 
                   height = mean(height), weight = mean(weight))

plot <- ggplot(avg.h, aes(y = weight, x = age)) + 
  # geom_point(alpha = 1.0) +
  geom_point(alpha = 0.5) +
  # geom_rug(position = "jitter", size = 0.2) +
  # ylim(80, 400) +
  # stat_density2d() +
  stat_smooth(method = "lm") +
  # stat_function(fun = function(w) 0.02904927 * w + 63.058) +
  # stat_function(fun = function(x) 14.89597 * x - 832.924) +
  # stat_function(fun = function(x) 0.4327172 * 14.89597 * x - 254.929) +
  theme_ubb + 
  labs(y = "Weight", x = "Age") 

nrow(avg.h)
with(avg.h, cor(weight, age))
print(plot)
save.plot(plot, figure.dir, "height_vs_weight.pdf", height = 5, width = 6)

summary.all <- with(h.and.w, {
  df <- data.frame(h.m = mean(height), h.sd = sd(height),  
                   w.m = mean(weight), w.sd = sd(weight))
  round(df, 2)
})

68.46 - 0.02904927 * 185.96

summary.all
tex.table(summary.all)

summary.all

44.39/2.98 * 0.4
0.4327172 * 14.89597

0.85 * 39.95/6.16

c <- with(h.and.w, cor(height, weight))
c^2
df$w.sd/df$h.sd

c
c * 14.89597

exam <- function(m) {
  0.5 * m + 30
}

exam(30)

m <- 0.4 * .6 /80

2.6 - m * 550
0.003 * 650 + 0.95 

qnorm(0.95)

pnorm(1.28, mean = 550, s = 80)

1.644854 * 80 + 550

0.003 * 682 + 0.95

( 3.0 - 2.6 )/0.6

pnorm(0.66667)
