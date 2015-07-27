
root.dir <- '~/Documents/U/ubb/sccc/math146'
notes.dir <- paste(root.dir, 'week06/notes', sep = '/')

setwd(notes.dir)

# exercises 1 and 3
ex1 <- rbind(data.frame(consumer = "buyer", higher = 20, same = 7, lower = 9),
             data.frame(consumer = "nonbuyer", higher = 29, same = 25, lower = 43))

ex1.m <- melt(ex1, id = "consumer")

ex1.margins <- cast(ex1.m, consumer ~ variable, margins = T, fun = sum)

same.or.higher <- sum(subset(ex1.m, variable %in% c("higher", 'same'), select = "value"))

same.or.higher
sum(ex1.m$value)

ex1.m

?rename

ex1.m <- rename(ex1.m, c(variable = "inclination"))
ex1.m

by.consumer <- ddply(ex1.m, .(consumer), summarize, inclination = inclination, 
                value = value / sum(value))
by.consumer.c <- cast(by.consumer, consumer ~ inclination)

by.inclination <- ddply(ex1.m, .(inclination), summarize, 
                        consumer = consumer, 
                        value = value / sum(value))
by.inclination
by.inclination.c <- cast(by.inclination, inclination ~ consumer )

by.inclination.c

sink(paste(notes.dir, 'r.tex', sep = '/'))
  xtable(by.consumer.c, digits = 2)
  xtable(by.inclination.c, digits = 2)
sink()

# exercise 2 and 4

ex2 <- rbind(data.frame(age = "15to17", female = 116, male = 61),
             data.frame(age = "18to24", female = 5470, male = 4691),
             data.frame(age = "25to34", female = 1319, male = 824),
             data.frame(age = "over34", female = 1075, male = 616))

ex2.m <- melt(ex2, id = "age")

ex2.margins <- cast(ex2.m, age ~ variable, margins = T, fun = sum)

ex2.m

cast(ex2.m, sex ~ ., margins = T, fun = sum)

by.age <- ddply(ex2.m, .(age), summarize, sex = variable, 
                value = value / sum(value))
by.age.c <- cast(by.age, age ~ sex)

by.sex <- ddply(ex2.m, .(variable), summarize, sex = variable, age = age, 
                value = value / sum(value))
by.sex.c <- cast(by.sex, sex ~ age)

sink(paste(notes.dir, 'r.tex', sep = '/'))
  xtable(by.age.c, digits = 2)
  xtable(by.sex.c, digits = 2)
sink()
