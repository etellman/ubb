
root.dir <- '~/Documents/U/ubb/sccc/math146'
notes.dir <- paste(root.dir, 'week06/notes', sep = '/')

setwd(notes.dir)

ex1 <- rbind(data.frame(consumer = "buyer", higher = 20, same = 7, lower = 9),
             data.frame(consumer = "nonbuyer", higher = 29, same = 25, lower = 43))

ex1.m <- melt(ex1, id = "consumer")

ex1.margins <- cast(ex1.m, consumer ~ variable, margins = T, fun = sum)


ex1.m

same.or.higher <- sum(subset(ex1.m, variable %in% c("higher", 'same'), select = "value"))

same.or.higher
sum(ex1.m$value)


# exercise 2

ex2 <- rbind(data.frame(age = "15to17", female = 116, male = 61),
             data.frame(age = "18to24", female = 5470, male = 4691),
             data.frame(age = "25to34", female = 1319, male = 824),
             data.frame(age = "over34", female = 1075, male = 616))

ex2.m <- melt(ex2, id = "age")

ex2.margins <- cast(ex2.m, age ~ variable, margins = T, fun = sum)

sink(paste(notes.dir, 'r.tex', sep = '/'))
  xtable(ex2.margins, digits = 0)
sink()
