# us_population.r

us <- read.csv('us_population.csv', header = T, comment.char = '#', strip.white = T)
us <- subset(us, year >= 1978)
us <- us[order(us$year),]

us$population[[1]]

plot <- ggplot(us, aes(x = year, y = population)) + 
  geom_line() +
  # coord_cartesian(ylim = c(0, 330)) +
  labs(x = "Year", y = "Population (millions)") 
print(plot)

save.plot(plot, figure.dir, 'us_population_200_origin.pdf')

plot <- ggplot(total.by.year, aes(x = year, y = prison.population / 1000)) + 
  geom_line() +
  ylab("Population/1000") +
  # coord_cartesian(ylim = c(0, 1250)) 
  theme(axis.title.x = element_blank()) 
print(plot)

save.plot(plot, figure.dir, 'prison_population_250_origin.pdf')

