This README describes a number of larger or more complex data sets
provided for teachers who make use of software.  It is often useful to
extract (or have students extract) subsets of the data for analysis.
For example, ``all subjects with positive income'' might be studied.
----------------------------------------
DATA: bliss.dat
Data from Orit Hetzroni, The Effects of Active Versus Passive
Computer-Assisted Instruction on the Acquisition, Retention, and
Generalization of Blissymbols While Using Elements for Teaching
Compounds, Ph.D. thesis, Purdue University, 1995.
The thesis concerns computer-aided instruction (CAI) in the use of
``Blissymbols'' for communication.  These symbols are used in teaching
special groups of students.  Normal-ability school children were
randomly assigned to treatment groups.  The part of the data in the
file concerns four groups in a randomized factorial 2 by 2 design:
              Learning Style
            Active   Passive
Before        I        II
During       III       IV
The two factors are the learning style (active or passive) of the CAI
lesson and the placement of the material to be learned (Blissymbol
elements before compounds or elements and compounds during a joint
presentation).  We expect active learning to be more successful than
passive.  We have no expectation about the effect of placement.  Each
subject was tested immediately after the lesson, one week later, and
three weeks later.  There are thus three repeated measures for each
subject.
variables and coding:
Group I, II, III, IV as above
Style Act=Active, Pas=Passive
Place Bef=Before, Dur=During
Obs  Subject identification number (shows how the randomization worked)
S1   Score (count of symbols correctly recognized, out of 24)
     after lesson
S2   Score on a similar test one week after lesson
S3   Score on a similar test three weeks after lesson
---------------------------------------
DATA: call.dat
Data provided by Larry Brown and Haipeng Shen, from Haipeng Shen,
Nonparametric Regression for Problems Involving Lognormal
Distributions, Ph.D. thesis, University of Pennsylvania, 2003.
These are talk times (seconds) for the 31,492 customer calls handled by
the customer service call center of a small bank in a month.  The
distribution of service times is strongly right-skewed.  Data analysis
shows interesting effects.  There are quite a few very short service
times (say, less than 10 seconds).  Short calls are mostly due to
agents hanging up on the customer in order to reduce their average talk
time and increase the count of calls handled -- these are (flawed)
measures of the agents' performance.  The extreme right tail shows
13 times over an hour, one almost 8 hours.  The reason is not
known, though equipment malfunction is possible.
----------------------------------------
DATA: class.dat
Data provided by students (n=270) in a freshman-level course.
It is of course even better to survey your own students.
variables and coding:
Sex    0=male, 1=female 
Hand   0=right-handed, 1=left-handed
Height in inches
Study  Time spent studying on a typical week night (minutes)
       Beware outliers!
Coins  How much money in coins (not bills) are you carrying?
Income Guess the income of a "typical American family"
       (At the time of the survey, the median family income was
        approximately $30,000.)
------------------------------------------------------
DATA: concept.dat
Data from Darlene Gordon, The Relationships Among Academic
Self-Concept, Academic Achievement, and Persistence with
Self-Attribution, Study Habits, and Perceived School Environment, Ph.D.
thesis, Purdue University, 1997.
The data describe 78 seventh-grade students in a rural midwestern
school.  The research concerned the relationship between the students'
``self-concept'' and their academic performance.
variables and coding:
Obs Subject identification number
GPA Grade point average
IQ  IQ test score
Age in years
Sex 1=Female, 2=Male
SC  Overall score on the Piers-Harris Children's Self-Concept Scale
C1 to C6, ``cluster scores'' for specific aspects of
self-concept: C1 = behavior, C2 = school status, C3 = physical appearance,
C4 = anxiety, C5 = popularity, and C6 = happiness
-------------------------------------------
DATA: education.dat  
Years of education a child can expect to receive in 177 countries.
That is, the data reflect enrollment patterns for children rather than
actual education attainment for current adults.  For men, women, and
overall.  From the United Nations Statistics Division, at unstats.un.org.
------------------------------------
DATA: floridavote.dat
Voting data for Florida counties in the 2000 presidential election.
The elction was famously close.  Palm Beach County had a confusing
ballot arrangement that led some voters to choose Buchanan in error.
Data analysis will disclose the effects of this.  See the Statistical
Thinking intoduction for students for a scatterplot.
variables and coding:
County   County name
Gore Bush Buchanan Nader  Vote counts for these candidates
GOREpct BUSHpct BUCHpct   Percent of votes for these candidates
-----------------------------------
DATA: foodsafety.dat
Data from Huey Chern Boo, Consumers' Perceptions and Concerns About
Safety and Healthfulness of Food Served at Fairs and Festivals, M.S.
thesis, Purdue University, 1997.
Random samples of people attending outdoor fairs and festivals in the
midwest.  Some of these data are discussed in the Complement Chapter 
25 on Nonparametric Tests.
variables and coding:
Subject   Subject identification number
In terms of nutrient content (eg: fat, cholesterol, fiber,
sodium, sugar), how healthful do you think is food prepared at these
locations?
        1=unhealthful
        2=somewhat unhealthful
        3=neither unhealthful nor healthful
        4=somewhat healthful
        5=healthful
Hfair response for outdoor fairs and festivals
How often do you think people become sick because of the
food they consume prepared at one of these locations?
        1=very rarely
        2=once in a while
        3=often
        4=more often than not
        5=always
Sfair response for outdoor fairs and festivals
Sfast response for fast-food restaurants
Srest response for ``sit-down'' restaurants
Sex      1=Female, 2=Male
---------------------------------------------------
DATA: income.dat     
Personal income and demographic data from the March, 2008 supplement to
the Current Population Survey.  Data on all 82,249 respondents aged 25
to 64 years who were currently in the labor force and who listed their
race as Asian, black, or white.  This is a random sample from all such
residents of the United States.
variables and coding:
Sex	1=male, 2=female
Income  Total personal income, dollars
Race    Person's race, 1=white, 2=black, 4=Asian
Age     Person's age in years
Educ    Educational attainment, 
		1=less than high school
                2=some high school but no diploma
                3=high school graduate
                4=some college but less than bachelor's degree
                5=bachelor's degree
                6=master's, professional, or doctoral degree
        (Educational attainment is condensed from 16 levels in
         the CPS data.)
--------------------------------------------------------
DATA: literacy.dat  
Adult literacy rates (percent) for 139 countries.  For men, women, and
overall.  A person is literate if he or she can both read and write
simple statements.   From the United Nations Statistics Division, at
unstats.un.org.
--------------------------------------------------------
DATA: newt.dat
Data provided by Drina Iglesia, Purdue University.  The data are part
of a larger study reported in D. D. S. Iglesia, E. J. Cragoe, Jr., and
J. W. Vanable, ``Electric field strength and epithelization in the newt
(Notophthalmus viridescens),'' Journal of Experimental Zoology, 274
(1996), pp.\ 56--62.
Data on healing of cuts in newts under several conditions of applied
electric field.  See Exercise 24.33 for a description of the
experiment.  Table 24.5 contains partial data.  Here are the full
data.  
variables and coding:
Field   Intended electrical field strength as a multiple of the 
        natural strength, which is Field=1.  This distinguishes
        the 5 groups of subjects.
FieldExp and FieldCtrl   Measured field strength in the experimental 
        (field applied) and control (natural) limbs.  This is a check
        on how well the experimenters succeeded in imposing the 
        conditions they aimed for in groups other than 1.  In group 1, 
        we see natural limb-to-limb variation.
RateExp and RateCtrl   Healing rates (micrometers per hour) in the
        experimental and control limbs.  This is the response variable.
----------------------------------------------------------
DATA: reading.dat
Table 1 from James T. Fleming, ``The measurement of children's
perception of difficulty in reading materials,'' Research in the
teaching of English, 1 (1967), pp. 136--156.  The data describe 60
5th-graders randomly sampled from one elementary school.
variables and coding:
Obs    Subject identification number
Sex    F=Female, M=Male
Lss    Median grade level of student's selection of "best for me to
        read" (based on 8 trials, each with 4 choices at grade levels
        3, 5, 7, 9)
IQ     IQ test score
Read   Score on reading subtest of the Metropolitan Achievement Test
Est    Student's own estimate of his/her reading ability, scale 1 to 5,
        reversed from the paper so that 1 = lowest, 5 = highest
Some issues: relationship between measured and self-estimated
reading ability; IQ and reading ability; sex differences; LSS
and measured reading ability (can children choose appropriate
level material?)
----------------------------------------------------
DATA: verizon.dat
Local telephone companies must provide repair service for customers
of other telephone companies who lease their lines.  The local company
is called the ILEC (Incumbent Local Exchange Carrier).  Competing
companies are CLECs (Competing Local Exchange Carriers).  The data
are a random sample of 1687 repair times (hours) from areas in
which Verizon is the ILEC.  Of these, 1664 are Verizon's own
customers (ILEC) and 23 are customers of other companies (CLEC).
The distributions of repair times are strongly skewed.
variables and coding:
Time   Repair time in hours
Group  ILEC (Verizon) customer or CLEC customer
Data courtesy of Tim Hesterberg, Insightful Corporation.
--------------------------------------
