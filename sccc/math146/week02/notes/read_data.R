# reads the data and adds education, state, race, field.of.study columns

wa <- read.delim("ss12pwa.csv", header = TRUE, sep = ',', strip.white = TRUE)

# remove suspiciously large numbers of 373000s
wa <- subset(wa, wagp != 373000)

wa <- add.columns(wa)
wa.fulltime <- subset(wa, is.fulltime(wagp, wkw, esr))

# returns TRUE if the subject worked at least 48 weeks over the year and made some money
is.fulltime <- function(wagp, wkw, esr) {
  !is.na(wagp) & !is.na(wkw) & wkw <= 2 & wagp > 0 & esr %in% c(1, 2, 4, 5)
}

# adds extra education, state, etc. columns to the original data
add.columns <- function(df) {
  df.augmented <- add.education(df)
  df.augmented <- add.state(df.augmented)
  df.augmented <- add.race(df.augmented)
  df.augmented <- add.sex(df.augmented)
  df.augmented <- add.field.of.study(df.augmented)

  df.augmented
}

# education
add.education <- function(df) {
  education <- vector()
  education[1:15] <- "No HS Diploma"
  education[16:17] <- "HS or GED"
  education[18:19] <- "Some College"
  education[20] <- "Associates Degree"
  education[21] <- "Bachelor's Degree"
  education[22] <- "Master's Degree"
  education[23] <- "Professional Degree"
  education[24] <- "Doctoral Degree"
  education[NA] <- NA

  cbind(df, education = education[df$schl])
}

# state
add.state <- function(df) {
  state <- vector()
  state[36] <- "NY"
  state[53] <- "WA"
  state[26] <- "MI"

  cbind(df, state = state[df$st])
}

# race
add.race <- function(df) {
  race <- vector()
  race[1] <- "White"
  race[2] <- "Black"
  race[3:5] <- "Native American"
  race[7] <- "Native American"
  race[6] <- "Asian"

  # use NA for anything else
  # race[8:9] <- "Other"

  cbind(df, race = race[df$rac1p])
}

# sex
add.sex <- function(df) {
  sex <- vector()
  sex[1] <- "Male"
  sex[2] <- "Female"

  df.sex <- sex[df$sex]
  without.sex <- subset(df, select = -sex)

  cbind(without.sex, sex = df.sex)
}

# give the field a name instead of a number
add.field.of.study <- function(df) {
  field.of.study <- vector()
  field.of.study[2408] <- "Electrical Engineering"
  field.of.study[2414] <- "Mechanical Engineering"
  field.of.study[2102] <- "Computer Science"
  field.of.study[2406] <- "Civil Engineering"
  field.of.study[2400] <- "General Engineering"
  field.of.study[3700] <- "Mathematics"
  field.of.study[6207] <- "Finance"
  field.of.study[5501] <- "Economics"
  field.of.study[6201] <- "Accounting"
  field.of.study[6203] <- "Business Management"
  field.of.study[6200] <- "General Business"
  field.of.study[6107] <- "Nursing"
  field.of.study[5506] <- "Political Science/Government"
  field.of.study[3600] <- "Biology"
  field.of.study[6206] <- "Marketing"
  field.of.study[5301] <- "Criminal Justice"
  field.of.study[6402] <- "History"
  field.of.study[2300] <- "General Education"
  field.of.study[1901] <- "Communications"
  field.of.study[5200] <- "Psychology"
  field.of.study[5507] <- "Sociology"
  field.of.study[3401] <- "Liberal Arts"
  field.of.study[2304] <- "Elementary Education"
  field.of.study[3301] <- "English"

  cbind(df, field.of.study = field.of.study[df$fod1p])
}

# adds an age category
add.age.category <- function(df) {
  categorize.by.age <- function(agep) {
    age.category <- vector()
    age.category[1:19] <- "Under 20"
    age.category[20:29] <- "Twenties"
    age.category[30:39] <- "Thirties"
    age.category[40:49] <- "Forties"
    age.category[50:59] <- "Fifties"
    age.category[60:69] <- "Sixties"
    age.category[70:79] <- "Seventies"
    age.category[80:200] <- "Eighty and Over"

    if (agep < 1) {
      NA
    } else {
      age.category[agep]
    }
  }

  cbind(df, age.category = sapply(df$agep, categorize.by.age))
}


# rm(race, state, sex, education, field.of.study)

