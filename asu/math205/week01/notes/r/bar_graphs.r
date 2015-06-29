# bar_graphs.r

# plots the top five states by prison population for a single year
#
# df - data frame with all the states
# target.year - the target year
top.five.plot <- function(df, target.year) {
  df.for.year <- subset(df, year == target.year)
  df.for.year <- df.for.year[order(df.for.year$prison.population, decreasing = TRUE),]

  plot <- ggplot(df.for.year[1:5,], aes(x = reorder(state, -prison.population), 
                                y = prison.population / 1000)) + 
    geom_bar(stat = "identity", fill = "lightblue", color = "black") +
    labs(y = "Prison Population/1000") +
    theme(axis.title.x = element_blank()) 

  print(plot)
  plot
}

plot <- top.five.plot(pbs, 1980)
save.plot(plot, figure.dir, 'top_five_1980.pdf')

plot <- top.five.plot(pbs, 2010)
save.plot(plot, figure.dir, 'top_five_2010.pdf')

# regional change over time
by.region <- subset(dcast(pbs.m, year + region ~ variable, sum), year %% 10 == 0)
by.region$region <- reorder(by.region$region, -by.region$prison.population)

plot <- ggplot(by.region, 
               aes(x = year, y = prison.population / 1000, 
                   fill = region, order = region)) + 
  geom_bar(stat = 'identity', color = 'black') +
  ylab('Population/1000') +
  theme(axis.title.x = element_blank()) +
  guides(fill = guide_legend(reverse = T))

print(plot)
save.plot(plot, figure.dir, 'regions_over_time.pdf')

# 2010 by region
by.region.2010 <- subset(dcast(pbs.m, year + region ~ variable, sum), year == 2010)
plot <- ggplot(by.region.2010, aes(x = reorder(region, -prison.population), 
                                   y = prison.population / 1000)) + 
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +
  ylab('Population/1000') +
  theme(axis.title.x = element_blank()) 

print(plot)
save.plot(plot, figure.dir, 'regions_2010.pdf')


# plots the populations for states in a region
#
# df - data frame
# target.region - target region
# target.year - target year
region.by.state <- function(df, target.region, target.year) {
  by.state <- subset(dcast(pbs.m, year + region + state ~ variable, sum), 
                     year == target.year & region == target.region)

  plot <- ggplot(by.state, aes(x = reorder(state, prison.population), 
                               y = prison.population / 1000)) + 
    geom_bar(stat = "identity", fill = "lightblue", color = "black") +
    ylab("Population/1000") +
    theme(axis.title.y = element_blank()) +
    coord_flip()

  print(plot)

  plot
}

plot <- region.by.state(pbs, "South", 2010)
save.plot(plot, figure.dir, 'south_by_state_2010.pdf', height = 4)

plot <- region.by.state(pbs, "West", 2010)
save.plot(plot, figure.dir, 'west_by_state_2010.pdf', height = 4)
