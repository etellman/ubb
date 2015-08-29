root.dir <- '~/Documents/U/ubb/asu/math205'

data.dir <- paste(root.dir, 'data', 'triola', sep = '/')

exam.dir <- paste(root.dir, 'part_one_exam', sep = '/')
figure.dir <- paste(exam.dir, 'figures', sep = '/')

setwd(data.dir)

1.5 * (649 - 172) 
(10 * 66 + 77)/11

649 + 715.5  
z(1150, mean = 1518, sd = 325)
z(16.5, mean = 21.1, sd = 4.8)
height.z

qnorm(0.90, mean = 1518, sd = 325)
qnorm(0.90, mean = 21.1, sd = 4.8)

health <- read.delim('health.csv', header = T, sep = ',', strip.white = T)

health

male <- subset(health, sex == 'M')
female <- subset(health, sex == 'F')

# 10
set.seed(15)
male.s <- male[sample(nrow(male), 10),]
female.s <- female[sample(nrow(female), 10),]
health.s <- rbind(male.s, female.s)

health.s

tex.table(subset(health.s, select = c(chol, sex)))
mean(male$chol)
s <- summary(female$chol)

str(female$chol)

?ddply

s.t <- table(s)
str(s.t)
f <- factor(s)
str(f)
df <- data.frame(stat = c("Min", "Q1", "Med", "Q3", "Max"), 
                 val = fivenum(female.s$chol))

tex.table(df)
2 + 3
sink()
str(df)
health.s$chol

plot <- ggplot(health.s, aes(x = chol)) + 
  geom_histogram(fill = 'lightblue', color = 'black', binwidth = 100) +
  theme_ubb +
  labs(x = 'Cholesterol', y = 'Subjects')
print(plot)

save.plot(plot, figure.dir, "cholesterol_histogram.pdf")

plot <- ggplot(health.s, aes(x = sex, y = chol)) + 
  geom_boxplot(color = 'black', fill = 'lightblue', 
               outlier.shape = 21, outlier.size = 1.5) +
  stat_summary(fun.y = 'mean', geom = 'point', shape = 23, 
               size = 2, fill = 'white') +
  theme_ubb + theme(axis.title.y = element_blank()) +
  scale_x_discrete(labels = c('M' = 'Male', 'F' = 'Female')) +
  labs(y = 'Cholesterol') +
  coord_flip() 
print(plot)

save.plot(plot, figure.dir, "cholesterol_box.pdf")


pnorm(79, mean = 69.9, sd = 2.8)
pnorm(3.25)
z(79, 69.9, 2.8)

z(16.5, 21.1, 4.8)

(0.3739 * 2.8 + 69.9)/12

qnorm(0.95)

649 + 1.5 * (649 - 172) 
1.6449 * 325 + 1518

9.231 * 5 + 115 
1.6449 * 4.8 + 21.1 
county <- c("Umatilla", "Morrow", "Gilliam", "Sherman", "Wasco", "Hood River", 
           "Portland", "Columbia", "Clatsop");
exposure <- c(2.49, 2.57, 3.41, 1.25, 1.62, 3.83, 11.64, 6.41, 8.34)
mortality <- c(147.1, 130.1, 129.9, 113.5, 137.5, 162.3, 207.5, 177.9, 210.3)

lm(mortality ~ exposure)
lm(exposure ~ mortality)
0.9296 * 5 - 10.01 

9.231 * 5 + 114.72
            0.09296 * 161 - 10.01
0.9263^2

?summary

cancer <- data.frame(county = county, exposure = exposure, mortality = mortality)

cor(mortality, exposure)
tex.table(cancer)
plot <- ggplot(cancer, aes(x = exposure, y = mortality)) + 
  geom_point() +
  stat_smooth(method = "lm") +
  theme_ubb +
  labs(x = "Index of Exposure", y = "Mortality") 
print(plot)
save.plot(plot, figure.dir, "cancer.pdf")

with(cancer, cor(exposure, mortality))

with(cancer, lm(exposure ~ mortality))
