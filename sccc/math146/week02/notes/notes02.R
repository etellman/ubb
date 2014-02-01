
# select a random sample of the subjects
select.sample <- function(df, size) {
  df[sample(1:nrow(df), size),]
}

# summary with the count and standard deviation also included 
extended.summary <- function(x) {
  c(Count = length(x), "Std Dev" = round(sd(x)), summary(x))
}

plot <- ggplot(subset(wa, wkw <= 2 & wagp > 0), aes(y = 100 * (..count..)/sum(..count..), x = wagp / 1000)) + 
        geom_histogram(fill = "lightblue", color = "black", binwidth = 10) +
        labs(x = "Wage (thousands)", y = "Percentage") +
        ggtitle("WA Wages")
print(plot)

wa.fulltime$field

ggsave("~/Documents/U/ubb/sccc/math146/week02/notes/figures/wa_wage_histogram.eps", 
       width = 6, height = 4)



df <- ddply(wa.fulltime, c("field.of.study"), function(df) c(field.median.wage = median(df$wagp)))
df <- join(wa.fulltime, df
df <- subset(df, !is.na(field.of.study))

selected.fields <- c("Electrical Engineering", "Mechanical Engineering", "Computer Science", "Elementary Education", "General Business")

df <- ddply(wa.fulltime, c("race"), function(df) c(race.median.education = median(df$schl)))
df <- join(wa.fulltime, df, "race")
df <- subset(df, !is.na(race))

plot <- ggplot(df, 
               # aes(x = 1, y = wagp / 1000)) + 
               aes(x = reorder(race, race.median.education), y = schl)) + 
  geom_boxplot(color = "black", fill = "lightblue", 
               outlier.shape = 21, outlier.size = 0) +
  stat_summary(fun.y = "mean", geom = "point", shape = 23, size = 2, fill = "white") +
  labs(x = "", y = "School (years)") +
  scale_y_continuous(limits = c(10, 25)) +
  coord_flip() +
  ggtitle("WA Education by Race")
print(plot)

ggsave("~/Documents/U/ubb/sccc/math146/week02/notes/figures/wa_education_by_race.eps", 
       width = 6, height = 6)

education.stats <- ddply(wa.fulltime, c("education"), function(df) c(education.median.wage = median(df$wagp)))
df <- join(wa.fulltime, education.stats)


plot <- ggplot(wa.fulltime, aes(x = 1, y = wagp / 1000)) + 
  geom_boxplot(color = "black", fill = "lightblue", outlier.size = 1.5) +
  stat_summary(fun.y = "mean", geom = "point", shape = 23, size = 2, fill = "white") +
  labs(x = "", y = "Wage (thousands)") +
  scale_y_continuous(limits = c(0, 225)) +
  coord_flip() +
  ggtitle("WA Wages")
print(plot)

ggsave("~/Documents/U/ubb/sccc/math146/week02/notes/figures/wa_wage.eps", width = 6, height = 2)

sink("~/Documents/U/ubb/sccc/math146/week02/notes/r.tex")
  education.by.race <- ddply(wa.fulltime, "race", function(df) summary(df$schl))
  education.by.race
  xtable(education.by.race, digits = 0)
sink()

ggsave("~/Documents/U/ubb/sccc/math146/week02/notes/figures/wa_age_by_sex.eps", 
       width = 6, height = 5)

df <- subset(wa.fulltime, wagp != 373000)
df <- ddply(df, "field.of.study", function(df) extended.summary(df$wagp))
df <- subset(df, !is.na(field.of.study) & Count > 100)
df <- df[order(-df$Median),]

sink("~/Documents/U/ubb/sccc/math146/week02/notes/r.tex")
xtable(df, digits = 0)
sink()

df <- join(wa.fulltime, top.fields, by = "field.of.study")
df <- subset(df, !is.na(field.of.study))

df <- ddply(wa.fulltime, c("race"), 
            function(df) c(degree.count = 100 * nrow(subset(df, schl >= 20)) / nrow(df))) 
df <- subset(df, !is.na(race))

plot <- ggplot(df, aes(x = reorder(race, degree.count), y = degree.count)) + 
  geom_bar(stat = "identity", color = "black", fill = "lightblue") +
  labs(x = "Education", y = "Percentage") +
  coord_flip() +
  ggtitle("WA Percentage with Associate's or Higher Degree")
print(plot)


plot <- ggplot(subset(wa.fulltime, field.of.study == "Computer Science" & wagp > 0), 
               aes(x = wagp / 1000, y = 100 * (..count..) / sum(..count..))) + 
  geom_histogram(binwidth = 20, color = "black", fill = "lightblue") +
  labs(x = "Wage (thousands)", y = "Percentage") +
  ggtitle("CS Wages")
print(plot)

ggsave("~/Documents/U/ubb/sccc/math146/week02/notes/figures/wa_cs_wages.eps", 
       width = 6, height = 4)

s <- sample(20, 15, replace = TRUE)
s
sd(c(0, 20, 40, 50, 60, 80, 100))
sd(c(0, 48, 49, 50, 51, 52, 100))
sd(c(0, 1, 2, 50, 98, 99, 100))
sd(c(1, 1, 16))

sink("~/Documents/U/ubb/sccc/math146/week02/notes/r.tex")
  xtable(t(as.matrix(summary(s))))
sink()
