root.dir <- "~/Documents/U/ubb/sccc/math146"

data.dir <- paste(root.dir, "data/bps/PC-Text", sep = "/")
hw6.data.dir <- paste(data.dir, "ch06", sep = "/")

hw.dir <- paste(root.dir, "week06/hw", sep = "/")
figures.dir <- paste(hw.dir, "figures", sep = "/")

setwd(hw6.data.dir)

# exercise 19
ex19 <- rbind(
  data.frame(treatment = "lithium", subjects = 24, success = 6),
  data.frame(treatment = "placebo", subjects = 24, success = 4),
  data.frame(treatment = "desipramine", subjects = 24, success = 14)
)

ex19.rates <- ddply(ex19, "treatment", summarize, 
                    success.rate = success / subjects)

ex19
ex19.rates

plot <- ggplot(ex19.rates, 
               aes(x = reorder(treatment, -success.rate), y = success.rate)) + 
  geom_bar(stat = "identity", fill="lightblue", color = "black") +
  labs(x = "Treatment", y = "Success Rate") +
  ggtitle("Exercise 19")
print(plot)

ggsave(paste(figures.dir, "ex19.pdf", sep = '/'), width = 4, height = 2.5)

sink(paste(hw.dir, 'r.tex', sep = '/'))
  xtable(ex19.rates, digits = 2)
sink()

# exercise 30
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

ex30.by.grade <- ddply(ta6.2, "job.grade", summarize, 
      employees = round(sum(employees) / sum(ta6.2$employees), 2))

ex30.by.marital <- ddply(ta6.2, "marital.status", summarize, 
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

# exercise 30
ex30 <- read.delim("ex06-30.dat", header = TRUE, sep = '\t')

ex30

by.oil <- ddply(ex30, "Oil", summarize, Cancer = Cancer, 
                   pct = 100 * round(Count / sum(Count), 3))
by.oil.c <- cast(by.oil, Oil ~ Cancer, value = .(pct), fun = sum)

by.cancer <- ddply(ex30, "Cancer", summarize, Oil = Oil, 
                   pct = 100 * round(Count / sum(Count), 3))
by.cancer.c <- cast(by.cancer, Cancer ~ Oil, value = .(pct), fun = sum)

by.oil.c
by.cancer.c

sink(paste(hw.dir, 'r.tex', sep = '/'))
  xtable(by.oil.c, digits = 2)
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

