root.dir <- '~/Documents/U/ubb/asu/math205'
notes.dir <- paste(root.dir, 'chapter15/notes', sep = '/')

range <- seq(-3, 6, 0.05)
sd <- 1.5

p <- ggdistribution(dnorm, range, mean = 0, sd = sd, colour = 'blue')
p <- ggdistribution(dnorm, range, mean = 3, sd = sd, p = p, colour = 'red')

print(p)

save.plot(p, dir = notes.dir, name = 'power_sd_075.pdf')

1 - 0.1894

1 - pnorm(mean = 0.8, sd = 1/sqrt(10), q = 0.52)

??install

install.packages('pwr')
?pwr.p.test

power.t.test(delta = 0.8, sd = 1, n = 10, sig.level = 0.05, type = "one.sample", 
             alternative = "one.sided")

pwr.p.test(h = 0.8, n = 10, sig.level = 0.05, alternative = "greater")

