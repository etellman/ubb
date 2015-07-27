root.dir <- '~/Documents/U/ubb/sccc/math146'
notes.dir <- paste(root.dir, 'week06/notes', sep = '/')

ucb <- rbind(
  data.frame(department = 'A', sex = 'M', applicants = 825, admitted = 511),
  data.frame(department = 'B', sex = 'M', applicants = 560, admitted = 353),
  data.frame(department = 'C', sex = 'M', applicants = 325, admitted = 120),
  data.frame(department = 'D', sex = 'M', applicants = 417, admitted = 139),
  data.frame(department = 'E', sex = 'M', applicants = 191, admitted = 53),
  data.frame(department = 'F', sex = 'M', applicants = 272, admitted = 16),

  data.frame(department = 'A', sex = 'F', applicants = 108, admitted = 89),
  data.frame(department = 'B', sex = 'F', applicants = 25, admitted = 17),
  data.frame(department = 'C', sex = 'F', applicants = 593, admitted = 202),
  data.frame(department = 'D', sex = 'F', applicants = 375, admitted = 131),
  data.frame(department = 'E', sex = 'F', applicants = 393, admitted = 94),
  data.frame(department = 'F', sex = 'F', applicants = 341, admitted = 24)

  # data.frame(department = 'G', sex = 'M', applicants = 307, admitted = 307),
  # data.frame(department = 'G', sex = 'F', applicants = 214, admitted = 214),

  # data.frame(department = 'G', sex = 'M', applicants = 16, admitted = 5),
  # data.frame(department = 'G', sex = 'F', applicants = 6, admitted = 1)

  # data.frame(department = 'other', sex = 'M', applicants = 2017, 
  #            admitted = round(0.4 * 2017)),
  # data.frame(department = 'other', sex = 'F', applicants = 1507, 
  #            admitted = round(0.4 * 1507))
)

ucb$rejected
ucb$rejected = with(ucb, applicants - admitted)

ucb.m <- melt(ucb, id = c('department', 'sex'))

# overall
overall <- with(ucb, data.frame(admitted = sum(admitted), 
                                rejected = sum(rejected), 
                                total = sum(admitted, rejected)))
with(ucb, round(sum(admitted) / sum(applicants), 2))

with(ucb, round(sum(admitted) / sum(applicants), 2))

# by sex
by.sex <- cast(ucb.m, sex ~ variable, subset = variable != "applicants", 
               fun = sum, margins = T)

by.sex.p <- ddply(ucb, .(sex), summarize, 
                admitted = round(sum(admitted) / sum(applicants), 2),
                rejected = round(sum(rejected) / sum(applicants), 2)
                )

by.department.and.sex <- cast(ucb.m, department + sex ~ variable, 
                              subset = variable != "applicants", fun = sum, margins = T)


sink(paste(notes.dir, 'r.tex', sep = '/'))
  xtable(by.department.and.sex, digits = 0)
sink()


# by department and sex
by.department.sex.p <- ddply(ucb, .(department, sex), summarize, 
                       proportion = round(sum(admitted) / sum(applicants), 2))
by.department.sex.c <- cast(by.department.sex.p, department ~ sex, 
                            value = .(proportion), fun = sum)

by.department.p <- ddply(ucb, .(department), summarize, 
                       proportion = round(sum(admitted) / sum(applicants), 2))
by.department.c <- cast(by.department.sex.p, department ~ sex, 
                            value = .(proportion), fun = sum)

ucb.m

women <- subset(ucb, sex == 'F')
men <- subset(ucb, sex == 'M')

women.by.department.p <- ddply(women, .(department), summarize, 
       proportion = round(sum(applicants) / sum(women$applicants), 2))

men.by.department.p <- ddply(men, .(department), summarize, 
       proportion = round(sum(applicants) / sum(men$applicants), 2))

by.department.sex.c <- cast(by.department.sex.p, department ~ sex, 
                            value = .(proportion), fun = sum)
women.by.department.p
men.by.department.p


by.department.and.sex <- cast(ucb.m, department + sex ~ variable, 
                              subset = variable != "applicants", fun = sum, margins = T)

sink(paste(notes.dir, 'r.tex', sep = '/'))
  xtable(women.by.department.p, digits = 2)
  xtable(men.by.department.p, digits = 2)
sink()

