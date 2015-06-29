# us_population.r

# plot prison population
plot <- ggplot(us, aes(x = year, y = population)) + 
  geom_line() +
  theme(axis.title.x = element_blank()) +
  # coord_cartesian(ylim = c(0, 330)) +
  ylab('Population (millions)') 

print(plot)
save.plot(plot, figure.dir, 'us_population_200_origin.pdf')

# plot US population
total.by.year <- ddply(pbs, .(year), summarize, year = first(year),
                       prison.population = sum(prison.population))
total.by.year <- merge(total.by.year, us)

plot <- ggplot(total.by.year, aes(x = year, y = prison.population / 1000)) + 
  geom_line() +
  ylab("Population/1000") +
  coord_cartesian(ylim = c(0, 1250)) +
  theme(axis.title.x = element_blank()) 

print(plot)
save.plot(plot, figure.dir, 'prison_population_0_origin.pdf')

# plot growth of incarceration rate
total.by.year$rate <- total.by.year$prison.population / (10 * total.by.year$population)

plot <- ggplot(total.by.year, aes(x = year, y = rate)) + 
  geom_line() +
  ylab('Rate') +
  coord_cartesian(ylim = c(0, 430)) +
  theme(axis.title.x = element_blank()) 

print(plot)
save.plot(plot, figure.dir, 'state_incarceration_rate_growth.pdf')
