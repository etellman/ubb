
root.dir <- '~/Documents/U/ubb/sccc/math146'

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
  data.frame(department = 'F', sex = 'F', applicants = 341, admissions = 24)
)

ucb

# demonstrate cast and melt
# cast/melt is good at building multi-dimensional tables, but not for
# calculating percentages
ucb.m <- melt(ucb, id = c('department', 'sex'))
ucb.m
cast(ucb.m, department + sex ~ variable, fun = sum, margins = T)

# overall
overall <- with(ucb, data.frame(admissions = sum(admissions), applicants = sum(applicants)))
with(ucb, round(sum(admissions) / sum(applicants), 2))

# by sex
by.sex <- ddply(ucb, .(sex), summarize, 
                applicants = sum(applicants), 
                admissions = sum(admissions), 
                proportion = round(sum(admissions) / sum(applicants), 2))
by.sex

# by department and sex
by.department.sex <- ddply(ucb, .(department, sex),
                           summarize, 
                           applicants = applicants, 
                           admissions = admissions, 
                           proportion = round(sum(admissions) / sum(applicants), 2))
by.department.sex

by.department.sex.c <- cast(by.department.sex, department ~ sex, value = .(proportion))
by.department.sex.c

# admission rates
plot <- ggplot(by.department.sex, aes(x = department, y = proportion, fill = sex)) + 
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(x = "Department", y = "Rate") +
  ggtitle("Admission Rates")
plot

# applicants
plot <- ggplot(by.department.sex, aes(x = department, y = applicants, fill = sex)) + 
  geom_bar(stat = "identity", position = "stack", color = "black") +
  labs(x = "Department", y = "Rate") +
  ggtitle("Applicants")

plot

# department 
by.department <- ddply(ucb, .(department), summarize, 
                       applicants = sum(applicants), 
                       admissions = sum(admissions), 
                       proportion = round(sum(admissions) / sum(applicants), 2))
by.department

plot <- ggplot(by.department, aes(x = department, y = proportion)) + 
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +
  labs(x = "Department", y = "Rate") +
  ggtitle("Admission Rates")
plot
