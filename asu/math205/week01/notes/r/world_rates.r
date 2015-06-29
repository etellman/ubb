# world_rates.r

# regional rates
regional.rates <- add.rates(dcast(pbs.m, year + region ~ variable, sum))

plot <- ggplot(subset(with.rates, year == 2010), 
               aes(x = reorder(region, -rate), y = rate)) + 
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +
  labs(x = "Region", y = "Rate") +
  ggtitle("2010 Regional Rates")
print(plot)

save.plot(plot, figure.dir, 'regional_rates_2010.pdf')

# US rate
with.rates <- add.rates(dcast(pbs.m, year ~ variable, sum))

with.rates

plot <- ggplot(with.rates, aes(x = year, y = rate)) + 
  # geom_line() +
  # geom_point() +
  geom_bar(stat = 'identity', color = 'black', fill = 'lightblue') +
  labs(x = 'Year', y = 'Rate') +
  ggtitle('US Incarceration Rate')

print(plot)
save.plot(plot, figure.dir, 'us_rate_over_time.pdf')

# WA rate
plot <- ggplot(subset(add.rates(pbs), state == "Washington"), aes(x = year, y = rate)) + 
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +
  labs(x = "Year", y = "Rate") +
  ggtitle("WA Incarceration Rate")

print(plot)
save.plot(plot, figure.dir, 'wa_rate_over_time.pdf')

# world incarceration rates
 
plot <- ggplot(data = world.rates, aes(x = rate)) + 
  geom_histogram(fill = "lightblue", color = "black", binwidth = 100) +
  labs(x = "Incarceration Rate", y = "Number Countries") +
  ggtitle("World Incarceration Rates")
print(plot)

save.plot(plot, figure.dir, 'world_histogram.pdf')

world.rates

sample.rates <- subset(world.rates, 
                       country %in% c('United States', 'Seychelles', 'Cuba',
                                      'Russia', 'Iran', 'England and Wales',
                                      'France', 'Japan', 'India', 'South Africa', 
                                      'San Marino', 'Iceland'))

plot <- ggplot(sample.rates, aes(x = reorder(country, rate), y = rate)) + 
  geom_bar(stat = 'identity', fill = 'lightblue', color = 'black') +
  ylab('Prisoners per 100,000') +
  theme(axis.title.y = element_blank()) +
  coord_flip()

print(plot)
save.plot(plot, figure.dir, 'sample_world_rates.pdf', height = 4)

