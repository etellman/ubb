# *** population histograms ***
population.histogram <- function(df, target.year, bw) {
  plot <- ggplot(data = subset(df, year == target.year), 
                 aes(x = prison.population / 1000)) + 
    geom_histogram(fill = 'lightblue', color = 'black', binwidth = bw) +
    labs(x = 'Population/1000', y = 'Number of States') +
    ggtitle(paste(target.year, 'Prison Population'))
  print(plot)

  plot
}

population.histogram(pbs, 1980, 2)
save.plot(plot, figure.dir, 'population_histogram_1980.pdf')

population.histogram(pbs, 2010, 10)
save.plot(plot, figure.dir, 'population_histogram_2010.pdf')

# all years doesn't make much sense
plot <- ggplot(pbs, aes(x = prison.population / 1000)) + 
  geom_histogram(fill = 'lightblue', color = 'black', binwidth = 10) +
  labs(x = 'Population/1000', y = 'Number of States') +
  ggtitle('1978 - 2012 Prison Population')
print(plot)
save.plot(plot, figure.dir, 'population_histogram_1978-2010.pdf')

# *** incarceration rates ***
# returns a new data frame with an incarceration rate column added and rows
# without a rate removed
add.rates <- function(df) {
  with.rates <- transform(df, rate = round( prison.population / state.population * 100000))
  subset(with.rates, !is.na(rate))
}

rate.histogram <- function(df, target.year, bw) {
  plot <- ggplot(data = subset(add.rates(df), year == target.year), aes(x = rate)) + 
    geom_histogram(fill = "lightblue", color = "black", binwidth = bw) +
    labs(x = "Prisoners per 100,000", y = "Number of States") +
    ggtitle(paste(target.year, "Incarceration Rate"))
  print(plot)

  plot
}

rate.histogram(pbs, 1980, 20)
save.plot(plot, figure.dir, 'rate_histogram_1980.pdf')

rate.histogram(pbs, 2010, 100)
save.plot(plot, figure.dir, 'rate_histogram_2010.pdf')

