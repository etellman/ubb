library(ggplot2) 
library(reshape)
library(xtable)
library(plyr) 

# data
pbs <- read.delim("prisoners_by_state.txt", header = TRUE, sep = '\t')
pbs.m <- melt(pbs, id = c("state", "region", "sub.region", "year"))

# data for a year ordered by prison population
for.year <- function(df, target.year) {
  data <- subset(df, year == target.year)
  data <- data[order(data$prison.population, decreasing = TRUE),]

  data
}

# *** time plots ***

# us population

total.by.year <- cast(pbs.m, year ~ variable, sum)

plot <- ggplot(subset(total.by.year, year %% 10 == 0), aes(x = year, y = state.population / 10^6)) + 
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +
  labs(x = "Year", y = "Population (millions)") +
  ggtitle("US Population")
print(plot)
ggsave("figures/us_population.eps", width = 5, height = 3)

# chop off bottom part of range
plot <- ggplot(subset(total.by.year, year %% 10 == 0), aes(x = year, y = state.population / 10^6)) + 
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +
  labs(x = "Year", y = "Population (millions)") +
  coord_cartesian(ylim = c(200, 320)) +
  ggtitle("US population with truncated y-axis")
print(plot)
ggsave("figures/us_population_limited_range.eps", width = 5, height = 3)

# US 
plot <- ggplot(total.by.year, aes(x = year, y = prison.population / 1000)) + 
  geom_line() +
  labs(x = "Year", y = "Population (thousands)") +
  ggtitle("US Prison Population")
print(plot)
ggsave("figures/us_prison_population.eps", width = 5, height = 3)

# WA
plot <- ggplot(subset(pbs, state == "Washington" & year %% 10 == 0), aes(x = year, y = state.population / 10^6)) + 
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +
  labs(x = "Year", y = "Population (millions)") +
  ggtitle("WA Population")
print(plot)
ggsave("figures/wa_population.eps", width = 5, height = 3)

plot <- ggplot(subset(pbs, state == "Washington"), aes(x = year, y = prison.population)) + 
  geom_line() +
  labs(x = "Year", y = "Population (thousands)") +
  ggtitle("WA Prison Population")
print(plot)
ggsave("figures/wa_prison_population.eps", width = 5, height = 3)

# *** Bar Graphs ***

# top five states

# plot for the top five
top.five.plot <- function(df, year) {
  df.for.year <- for.year(df, year)

  ggplot(df.for.year[1:5,], aes(x = reorder(state, -prison.population), y = prison.population / 1000)) + 
    geom_bar(stat = "identity", fill = "lightblue", color = "black") +
    labs(x = "State", y = "Population (thousands)") +
    ggtitle(paste(year, "Top Five"))
}

print(top.five.plot(pbs, 1980))
ggsave("figures/top_five_1980.eps", width = 5, height = 3)

print(top.five.plot(pbs, 2010))
ggsave("figures/top_five_2010.eps", width = 5, height = 3)

# regional change over time
by.region <- subset(cast(pbs.m, year + region ~ variable, sum), year %% 10 == 0)
by.region$region <- reorder(by.region$region, -by.region$prison.population)

plot <- ggplot(by.region, 
               aes(x = year, y = prison.population / 1000, 
                   fill = region, order = region)) + 
  geom_bar(stat = "identity", color = "black") +
  labs(x = "Region", y = "Population (thousands)") +
  guides(fill = guide_legend(reverse = TRUE)) +
  ggtitle("Regions over Time")
print(plot)

ggsave("figures/regions_over_time.eps", width = 5, height = 3)


with.percentages <- ddply(pbs, .(year, region), transform, 
      percent.of.region = prison.population / sum(prison.population) * 100) 

subset(with.percentages, year == 2010, select = c(state, region, percent.of.region))

plot <- ggplot(percentages, 
               aes(x = year, y = percent, fill = region, order = region)) + 
  geom_bar(stat = "identity", color = "black") +
  labs(x = "Region", y = "Percentage") +
  guides(fill = guide_legend(reverse = TRUE)) +
  ggtitle("Regions over Time With Percentages") 
  # scale_fill_grey() 
print(plot)

ggsave("figures/regions_over_time_percentages.eps", width = 5, height = 3)


# 2010 by region
by.region.2010 <- subset(cast(pbs.m, year + region ~ variable, sum), year == 2010)
plot <- ggplot(by.region.2010, aes(x = reorder(region, -prison.population), y = prison.population / 1000)) + 
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +
  ggtitle("2010 by Region")
print(plot)

ggsave("figures/regions_2010.eps", width = 5, height = 3)


# plots the populations for states in a region
region.by.state <- function(df, target.region, target.year) {
  by.state <- subset(cast(pbs.m, year + region + state ~ variable, sum), 
                     year == target.year & region == target.region)

  plot <- ggplot(by.state, aes(x = reorder(state, prison.population), 
                               y = prison.population / 1000)) + 
    geom_bar(stat = "identity", fill = "lightblue", color = "black") +
    labs(x = "State", y = "Population (thousands)") +
    coord_flip() +
    ggtitle(paste(target.region, "by State"))
  print(plot)

  plot
}

plot <- region.by.state(pbs, "West", 2010)
ggsave("figures/west_by_state.eps", width = 5, height = 3)

# *** population histograms ***
population.histogram <- function(df, target.year, bw) {
  plot <- ggplot(data = subset(df, year == target.year), 
                 aes(x = prison.population / 1000)) + 
    geom_histogram(fill = "lightblue", color = "black", binwidth = bw) +
    labs(x = "Population (thousands)", y = "Number of States") +
    ggtitle(paste(target.year, "Prison Population"))
  print(plot)

  plot
}

population.histogram(pbs, 1980, 2)
ggsave("figures/population_histogram_1980.eps", width = 5, height = 3)

population.histogram(pbs, 2010, 1)
ggsave("figures/population_histogram_2010_small_bins.eps", width = 5, height = 3)

# all years doesn't make much sense
plot <- ggplot(pbs, aes(x = prison.population / 1000)) + 
  geom_histogram(fill = "lightblue", color = "black", binwidth = 10) +
  labs(x = "Population (thousands)", y = "Number of States") +
  ggtitle("1978 - 2012 Prison Population")
print(plot)

# *** incarceration rates ***
rate.histogram <- function(df, target.year, bw) {
  plot <- ggplot(data = subset(df, year == target.year), aes(x = state.rate)) + 
    geom_histogram(fill = "lightblue", color = "black", binwidth = bw) +
    labs(x = "Rate", y = "Number of States") +
    ggtitle(paste(target.year, "Incarceration Rate"))
  print(plot)

  plot
}

rate.histogram(pbs, 1980, 20)
ggsave("figures/rate_histogram_1980.eps", width = 5, height = 3)

rate.histogram(pbs, 2010, 100)
ggsave("figures/rate_histogram_2010.eps", width = 5, height = 3)

with.rate.ranks <- ddply(pbs, .(year), transform, 
                         rank = rank(-state.rate, ties.method = "first"))


state.rates <- function(df, target.year) {
  plot <- ggplot(subset(df, year == target.year), 
                 aes(x = reorder(state, -rank), y = state.rate)) + 
    geom_point() +
    # geom_bar(stat = "identity", fill = "lightblue", color = "black") +
    labs(x = "State", y = "Rate") +
    coord_flip() +
    ggtitle(paste(target.year, "Rates"))
  print(plot)
  plot
}

state.rates(with.rate.ranks, 1980)
ggsave("figures/state_rates_1980.eps", width = 5, height = 7)

state.rates(with.rate.ranks, 2010)
ggsave("figures/state_rates_2010.eps", width = 5, height = 7)

# regional rates
with.rates <- transform(cast(pbs.m, year + region ~ variable, sum), 
                        rate = 100000 * prison.population / state.population) 

with.rates <- subset(with.rates, !is.na(rate), select = c(year, region, rate))

plot <- ggplot(subset(with.rates, year == 2010), 
               aes(x = reorder(region, -rate), y = rate)) + 
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +
  labs(x = "Region", y = "Rate") +
  ggtitle("2010 Regional Rates")
print(plot)

ggsave("figures/regional_rates_2010.eps", width = 5, height = 3)

# US rate
with.rates <- transform(cast(pbs.m, year ~ variable, sum), 
                        rate = 100000 * prison.population / state.population) 

with.rates <- subset(with.rates, !is.na(rate), select = c(year, rate))

plot <- ggplot(with.rates, aes(x = year, y = rate)) + 
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +
  labs(x = "Year", y = "Rate") +
  ggtitle("US Incarceration Rate")
print(plot)
ggsave("figures/us_rate_over_time.eps", width = 5, height = 3)


# WA rate
plot <- ggplot(subset(pbs, state == "Washington" & !is.na(state.rate)), 
               aes(x = year, y = state.rate)) + 
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +
  labs(x = "Year", y = "Rate") +
  ggtitle("WA Incarceration Rate")
print(plot)

ggsave("figures/wa_rate_over_time.eps", width = 5, height = 3)

# world incarceration rates
world.rates <- read.delim("data/world_incarceration_rates.txt", header = TRUE, 
                          strip.white = TRUE, sep = '\t')
 
plot <- ggplot(data = world.rates, aes(x = rate)) + 
  geom_histogram(fill = "lightblue", color = "black", binwidth = 100) +
  labs(x = "Incarceration Rate", y = "Number Countries") +
  ggtitle("World Incarceration Rates")
print(plot)

ggsave("figures/wold_histogram.eps", width = 5, height = 3)


sample.rates <- subset(world.rates, 
                       country == "United States" | 
                       country == "Cuba" | 
                       country == "South Africa" | 
                       country == "England and Wales" | 
                       country == "France" | 
                       country == "Japan" |
                       country == "India" | 
                       country == "South Africa") 

plot <- ggplot(sample.rates, aes(x = reorder(country, rate), y = rate)) + 
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +
  labs(x = "Country", y = "Rate") +
  coord_flip() +
  ggtitle("World Incarceration Rates")
print(plot)
ggsave("figures/sample_world_rates.eps", width = 5, height = 3)

