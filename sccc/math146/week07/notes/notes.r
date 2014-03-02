
root.dir <- '~/Documents/U/ubb/sccc/math146'
data.dir <- paste(root.dir, 'data', sep = '/')
notes.dir <- paste(root.dir, 'week07/notes', sep = '/')
figures.dir <- paste(notes.dir, 'figures', sep = '/')

prison.data.dir <- paste(data.dir, 'prison_data', sep = '/')
setwd(prison.data.dir)

# time series
pbs <- read.delim("prisoners_by_state.txt", header = TRUE, sep = '\t')
pbs.m <- melt(pbs, id = c("state", "region", "year"))

us.population <- ddply(subset(pbs, !is.na(state.population)), 
                             .(year), summarize, 
                             population = round(sum(state.population) / 10^6))

ggplot(us.population, aes(x = year, y = population)) + 
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +
  labs(x = "Year", y = "Population (millions)") +
  ggtitle("US Population")

# prison population by year
prison.population <- ddply(pbs, .(year), summarize, 
                           population = round(sum(prison.population) / 10^3))

# bar plot
plot <- ggplot(prison.population, aes(x = year, y = population)) + 
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +
  labs(x = "Year", y = "Population (thousands)") +
  ggtitle("Prison Population")
print(plot)

# line plot
plot <- ggplot(prison.population, aes(x = year, y = population)) + 
  geom_line() +
  labs(x = "Year", y = "Population (thousands)") +
  ggtitle("Prison Population")
print(plot)

# prison population by state for 2010

# top 10 in alphabetical order
pbs.2010 <- subset(pbs, year == 2010, select = c(state, prison.population))
top.10 <- pbs.2010[rank(-pbs.2010$prison.population) < 10,]
top.10

# top 10 ordered by prison population, largest first
top.10[order(-top.10$prison.population),]

# bar plot
plot <- ggplot(subset(pbs, year == 2010),
               aes(x = reorder(state, prison.population), y = prison.population / 10^3)) + 
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +
  labs(x = "Year", y = "Population (thousands)") +
  ggtitle("2010 Prison Population") +
  coord_flip()
print(plot)

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
population.histogram(pbs, 2010, 10)

str(pbs)

# incarceration rates by state and year
incarceration.rates <- ddply(subset(pbs, !is.na(state.population)), 
  .(year, state), summarize, 
  rate = round(10^5 * sum(prison.population) / sum(state.population)))

incarceration.rates$year = factor(incarceration.rates$year)

plot <- ggplot(incarceration.rates, aes(x = year, y = rate)) + 
  geom_boxplot(color = "black", fill = "lightblue", outlier.shape = 21, outlier.size = 1.5) +
  stat_summary(fun.y = "mean", geom = "point", shape = 23, size = 2, fill = "white") +
  labs(x = "Year", y = "Rate") +
  coord_flip() +
  ggtitle("Incarceration Rates")
print(plot)

subset(incarceration.rates, rate > 700)
# print out the numbers
ddply(incarceration.rates, .(year), function(df) summary(df$rate))

# top 10 by rate
rates.2010 <- subset(incarceration.rates, year == 2010, select = c(state, rate))
top.10.rates <- rates.2010[rank(-rates.2010$rate) < 10,]
top.10.rates

# top 10 ordered by prison population, largest first
top.10.rates[order(-top.10.rates$rate),]

ucb <- rbind(
  data.frame(department = 'A', sex = 'M', applicants = 825, admissions = 511),
  data.frame(department = 'B', sex = 'M', applicants = 560, admissions = 353),
  data.frame(department = 'C', sex = 'M', applicants = 325, admissions = 120),
  data.frame(department = 'D', sex = 'M', applicants = 417, admissions = 139),
  data.frame(department = 'E', sex = 'M', applicants = 191, admissions = 53),
  data.frame(department = 'F', sex = 'M', applicants = 272, admissions = 16),

  data.frame(department = 'A', sex = 'F', applicants = 108, admissions = 89),
  data.frame(department = 'B', sex = 'F', applicants = 25, admissions = 17),
  data.frame(department = 'C', sex = 'F', applicants = 593, admissions = 202),
  data.frame(department = 'D', sex = 'F', applicants = 375, admissions = 131),
  data.frame(department = 'E', sex = 'F', applicants = 393, admissions = 94),
  data.frame(department = 'F', sex = 'F', applicants = 341, admissions = 24),

  # data.frame(department = 'G', sex = 'M', applicants = 307, admissions = 307),
  # data.frame(department = 'G', sex = 'F', applicants = 214, admissions = 214),

  data.frame(department = 'G', sex = 'M', applicants = 16, admissions = 5),
  data.frame(department = 'G', sex = 'F', applicants = 6, admissions = 1),

  data.frame(department = 'other', sex = 'M', applicants = 2017, 
             admissions = round(0.4 * 2017)),
  data.frame(department = 'other', sex = 'F', applicants = 1507, 
             admissions = round(0.4 * 1507))
)

ucb.m <- melt(ucb, id = c('department', 'sex'))



# overall
overall <- with(ucb, data.frame(admissions = sum(admissions), applicants = sum(applicants)))
with(ucb, round(sum(admissions) / sum(applicants), 2))

sink(paste(notes.dir, 'r.tex', sep = '/'))
  xtable(overall, digits = 0)
sink()

# by sex
by.sex <- ddply(ucb, .(sex), summarize, 
                applicants = sum(applicants), 
                admissions = sum(admissions), 
                proportion = round(sum(admissions) / sum(applicants), 2))

by.sex

# by department and sex
by.department.sex <- ddply(ucb, .(department, sex), summarize, 
                       applicants = applicants, 
                       admissions = admissions, 
                       proportion = round(sum(admissions) / sum(applicants), 2))
by.department.sex.c <- cast(by.department.sex, department ~ sex, 
                            value = .(proportion), fun = sum)


# by department
by.department <- ddply(ucb, .(department), summarize, 
                       applicants = sum(applicants), 
                       admissions = sum(admissions), 
                       proportion = round(sum(admissions) / sum(applicants), 2))


