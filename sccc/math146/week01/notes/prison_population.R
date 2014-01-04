library(ggplot2) 
library(reshape)
library(xtable)

# data
pbs <- read.delim("prisoners_by_state.txt", header = TRUE, sep = '\t')
pbs.m <- melt(pbs, id = c("state", "region", "sub.region", "year"))
total.by.year <- cast(pbs.m, year ~ variable, sum)

?xtable
xtable(subset(pbs, year == 2010))

# time plots

# us population
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

pbs.decade <- subset(pbs, year %% 10 == 0)
pbs.decade.m <- melt(pbs.decade, id = c("state", "year", "region", "sub.region"))

by.year <- cast(pbs.decade.m, year ~ variable, sum)

total.rate <- with(by.year, rate(prison.population, state.population))

rate.by.year <- data.frame(year = by.year$year, value = total.rate)

pbs2010 <- subset(pbs, year == 2010)
pbs2010 <- pbs2010[order(pbs2010$prison.population, decreasing = TRUE),]
pbs2010[1,]

washington <- subset(pbs.decade.m, state == "Texas" & variable == "state.rate", select=c(state, year, value))
plot <- ggplot(rate.by.year, aes(x = year, y = value)) + 
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +
  geom_text(aes(label = value, vjust = 1.5)) +
  # geom_point() + coord_flip() +
  labs(x = "Year", y = "Incarceration Rate") +
  ggtitle("US State Incarceration Rate")
print(plot)

ggsave("us_rate.eps", width = 5, height = 3)

str(pbs2010)

plot <- ggplot(data = subset(pbs, year == 1980), aes(x = state.rate)) + 
  geom_histogram(fill = "lightblue", color = "black", binwidth = 50) +
  labs(x = "Incarceration Rate", y = "Number of States") +
  ggtitle("1980 Incarceration Rate")
print(plot)

pbs2010 <- subset(pbs, year == 2010, select = c(state, prison.population, state.rate))
pbs2010 <- pbs2010[order(pbs2010$state.rate, decreasing = TRUE),]
pbs2010[1:5,]
pbs2010[45:50,]
summary(subset(pbs1980, select = state.rate))

ggsave("1980_rate_histogram.eps", width = 5, height = 3)

?read.delim

world.rates <- read.delim("data/world_incarceration_rates.txt", header = TRUE, strip.white = TRUE, sep = '\t')
 
sample.rates <- subset(world.rates, country == "United States" | country == "Cuba" | country == "Russia" | country == "England and Wales" | country == "France" | country == "Japan"| country == "India" | country == "South Africa")

plot <- ggplot(pbs, aes(x = reorder(state, state.rate), y = state.rate)) + 
  # geom_bar(stat = "identity", fill = "lightblue", color = "black") +
  geom_point() +
  coord_flip() +
  labs(x = "State", y = "Incarceration Rate") +
  ggtitle("2010 State Incarceration Rates")
print(plot)

plot <- ggplot(pbs2010, aes(x = reorder(state, state.rate), y = state.rate)) + 
  # geom_bar(stat = "identity", fill = "lightblue", color = "black") +
  geom_point() +
  coord_flip() +
  labs(x = "State", y = "Incarceration Rate") +
  ggtitle("2010 State Incarceration Rates")
print(plot)

ggsave("rate_by_state_2010.eps", width = 5, height = 7)

plot <- ggplot(data = world.rates, aes(x = rate)) + 
  geom_histogram(fill = "lightblue", color = "black", binwidth = 50) +
  labs(x = "Incarceration Rate", y = "Number Countries") +
  ggtitle("World Incarceration Rates")
print(plot)

ggsave("rate_by_region.eps", width = 5, height = 3)

median(world.rates$rate)

str(total.by.year)

total.by.year

rates <- with(pbs, rate(prison.population, state.population))
str(rates)

pbs <- edit(cbind(pbs, state.rate = rates))


pbs.m <- subset(pbs2010.m, variable != "year" & variable != "X")

by.state <- cast(pbs.m, year + state ~ variable, sum)

str(by.state)

rate <- function(prison.population, total.population) {
  round(100000 * prison.population / total.population)
}

rate.by.region <- rate(population.by.region$prison.population, population.by.region$state.population)

rate.by.region.f <- data.frame(region = population.by.region$region, rate = rate.by.region)

rate.by.region

as.data.frame(as.list(by(pbs2010, pbs2010$region, function(df) { 100000 * sum(df$prison.population, na.rm = TRUE) / sum(df$state.population, na.rm = TRUE) })))

