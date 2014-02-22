library('plyr')
library('ggplot2')
library('reshape')

root.dir <- '~/Documents/U/ubb/sccc/math146'
notes.dir <- paste(root.dir, 'week06/notes', sep = '/')
figures.dir <- paste(notes.dir, 'figures', sep = '/')

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
with(ucb, round(sum(admissions) / sum(applicants), 2))

# by sex
by.sex <- ddply(ucb, .(sex), summarize, 
                applicants = sum(applicants), 
                admissions = sum(admissions), 
                proportion = round(sum(admissions) / sum(applicants), 2))

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

sink(paste(notes.dir, 'r.tex', sep = '/'))
  xtable(by.department, digits = 2)
sink()

