root.dir <- '~/Documents/U/ubb/asu/math205'
notes.dir <- paste(root.dir, 'chapter17/notes', sep = '/')

set.seed(15)
easy <- round(rnorm(20, mean = 8, sd = 1), 1)
hard <- round(rnorm(20, mean = 6, sd = 1), 1)

t.test(easy, mu = 8)
t.test(hard, mu = 6)

mean(easy) - mean(hard)

easy.t

easy.t['conf.int']
hard.t['conf.int']

t.test(easy - hard, mu = 2)

easy - hard

easy <- easy[order(-easy)]
hard <- hard[order(-hard)]

t <- t.test(easy, hard, paired = T, mu = 2)

attr(t$conf.int, "conf.level")

conf.level(t$conf.int)

df <- data.frame(judge1 = easy, judge2 = hard, delta = easy - hard)

?atomic

tex.table(df, digits = 1)

?t.test

df
df.m <- melt(df, measure.vars = c("judge1", "judge2"))

tex.table(ddply(df.m, .(variable), summarize, x.bar = mean(value)))

(1.2 - 2.23)/2
