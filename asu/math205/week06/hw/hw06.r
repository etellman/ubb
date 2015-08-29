root.dir <- '~/Documents/U/ubb/asu/math205'
data.dir <- paste(root.dir, 'data', 'bps', 'PC-Text', 'ch06', sep = '/')
setwd(data.dir)

week.dir <- paste(root.dir, 'week06', sep = '/')
figure.dir <- paste(week.dir, 'hw', 'figures', sep = '/')

# exercise 19
ex19 <- rbind(
  data.frame(treatment = "Lithium", subjects = 24, success = 6),
  data.frame(treatment = "Placebo", subjects = 24, success = 4),
  data.frame(treatment = "Desipramine", subjects = 24, success = 14)
)

ex19.rates <- ddply(ex19, "treatment", summarize, 
                    success.rate = success / subjects)

plot <- ggplot(ex19.rates, 
               aes(x = reorder(treatment, -success.rate), y = success.rate)) + 
  geom_bar(stat = "identity", fill="lightblue", color = "black") +
  theme_ubb + theme(axis.title = element_blank()) 
  labs("Success Rate")
print(plot)

save.plot(plot, figure.dir, "ex19.pdf")

sink(paste(hw.dir, 'r.tex', sep = '/'))
  xtable(ex19.rates, digits = 2)
sink()

# exercise 21-24
ta6.2 <- read.delim("ta06-02.dat", header = TRUE, sep = '\t')

ta6.2 <- rbind(
  data.frame(job.grade = 1 , marital.status = "single"   , employees = 58)   ,
  data.frame(job.grade = 1 , marital.status = "married"  , employees = 874)  ,
  data.frame(job.grade = 1 , marital.status = "divorced" , employees = 15)   ,
  data.frame(job.grade = 1 , marital.status = "widowed"  , employees = 8)    ,

  data.frame(job.grade = 2 , marital.status = "single"   , employees = 222)  ,
  data.frame(job.grade = 2 , marital.status = "married"  , employees = 3927) ,
  data.frame(job.grade = 2 , marital.status = "divorced" , employees = 70)   ,
  data.frame(job.grade = 2 , marital.status = "widowed"  , employees = 20)   ,

  data.frame(job.grade = 3 , marital.status = "single"   , employees = 50)   ,
  data.frame(job.grade = 3 , marital.status = "married"  , employees = 2396) ,
  data.frame(job.grade = 3 , marital.status = "divorced" , employees = 34)   ,
  data.frame(job.grade = 3 , marital.status = "widowed"  , employees = 10)   ,

  data.frame(job.grade = 4 , marital.status = "single"   , employees = 7)    ,
  data.frame(job.grade = 4 , marital.status = "married"  , employees = 533)  ,
  data.frame(job.grade = 4 , marital.status = "divorced" , employees = 7)    ,
  data.frame(job.grade = 4 , marital.status = "widowed"  , employees = 4)
)

by.grade <- ddply(ta6.2, "job.grade", summarize, 
      employees = round(sum(employees) / sum(ta6.2$employees), 2))

by.marital <- ddply(ta6.2, "marital.status", summarize, 
      employees = round(sum(employees) / sum(ta6.2$employees), 2))

ddply(ta6.2, c("marital.status", "job.grade"), summarize, 
      employees = round(sum(employees) / sum(ta6.2$employees), 2))

ex21 <- ddply(ta6.2, c("marital.status"), summarize, 
      count = sum(employees), 
      employees = round(sum(employees) / sum(ta6.2$employees), 2))

function(df) sum(subset(df$job.grade == 1, select = "employees")) / sum(employees)
ex21 <- ddply(ta6.2, c("marital.status"), 
      count = sum(employees), 
      employees = round(sum(employees) / sum(ta6.2$employees), 2))
ex21

ta62.m <- melt(ta6.2, id = c("marital.status", "job.grade"))
cast(ta62.m, marital.status ~ job.grade, function(x) sum(x), margins = T)

337/955


# percent of job grade for each marital status
by.job.grade <- ddply(ta6.2, "job.grade", summarise, marital.status = marital.status, 
             pct = 100 * round(employees / sum(employees), 3))
by.job.grade.c <- cast(by.job.grade, job.grade ~ marital.status, value = .(pct),
                     fun = sum, margins = "grand_col")

by.marital <- ddply(ta6.2, "marital.status", summarise, job.grade = job.grade, 
             employees = employees, pct = 100 * round(employees / sum(employees), 3))
by.marital.c <- cast(by.marital, marital.status ~ job.grade, value = .(pct), 
                     fun = sum, margins = "grand_col")

by.job.grade.c
by.marital.c

# exercise 27
ex27 <- read.delim("ex06-27.dat", header = TRUE, sep = '\t')

by.degree <- ddply(ex27, "Degree", summarise, Work = Work, 
                   pct = 100 * round(Count / sum(Count), 3))
by.degree.c <- cast(by.degree, Degree ~ Work, value = .(pct), 
                     fun = sum, margins = "grand_col")

# exercise 29
ex29 <- rbind(data.frame(degree = 'associates', sex = 'F', degrees = 447),
              data.frame(degree = 'associates', sex = 'M', degrees = 268),
              data.frame(degree = 'bachelors', sex = 'F', degrees = 945),
              data.frame(degree = 'bachelors', sex = 'M', degrees = 651),
              data.frame(degree = 'masters', sex = 'F', degrees = 397),
              data.frame(degree = 'masters', sex = 'M', degrees = 251),
              data.frame(degree = 'professional', sex = 'F', degrees = 49),
              data.frame(degree = 'professional', sex = 'M', degrees = 44),
              data.frame(degree = 'doctors', sex = 'F', degrees = 26),
              data.frame(degree = 'doctors', sex = 'M', degrees = 25)
)

by.degree <- ddply(ex29, "degree", summarize, sex = sex, 
                   pct = round(100 * degrees / sum(degrees)))
by.degree.c <- cast(by.degree, degree ~ sex, value = .(pct), fun = sum)

sink(paste(hw.dir, 'r.tex', sep = '/'))
  xtable( by.degree.c, digits = 0)
sink()

# exercise 30
ex30 <- read.delim("ex06-30.dat", header = TRUE, sep = '\t')

ex30

by.oil <- ddply(ex30, "Oil", summarize, Cancer = Cancer, 
                   pct = 100 * round(Count / sum(Count), 3))
by.oil.2way <- cast(by.oil, Oil ~ Cancer, value = .(pct), fun = sum)

by.cancer <- ddply(ex30, "Cancer", summarize, Oil = Oil, 
                   pct = 100 * round(Count / sum(Count), 3))
by.cancer.2way <- cast(by.cancer, Cancer ~ Oil, value = .(pct), fun = sum)

by.oil.2way
by.cancer.2way

by.cancer

sink(paste(hw.dir, 'r.tex', sep = '/'))
  xtable(by.oil.2way, digits = 1)
  xtable(by.cancer.2way, digits = 1)
sink()

# exercise 31
ex31 <- read.delim("ex06-31.dat", header = TRUE, sep = '\t')

ex31

by.anger <- ddply(ex31, "Anger", summarize, CHD = CHD, 
                   pct = 100 * round(Count / sum(Count), 3))
by.anger.c <- cast(by.anger, Anger ~ CHD, value = .(pct), fun = sum)

by.anger.c

sink(paste(hw.dir, 'r.tex', sep = '/'))
  xtable(by.anger.c, digits = 1)
sink()

