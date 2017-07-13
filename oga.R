library(dplyr)

datas <- read.csv('/Users/abc/Downloads/data.csv')

#=============== Basic Information =======================
go <- datas%>%filter(Have.you.participated.in.UW.study.abroad.programs.before.=='Yes')

# not.go <- datas%>%filter(Have.you.participated.in.UW.study.abroad.programs.before.=='No')

no.consider <- datas%>%filter(Are.you.considering.joining.a.UW.study.abroad.program. == "No")

not.planning <- datas%>%filter(Have.you.taken.any.actions.on.planning.for.studying.abroad...Such.as.seeing.an.adviser..looking.up.programs..etc.... == 'No')

planning <- datas%>%filter(Have.you.taken.any.actions.on.planning.for.studying.abroad...Such.as.seeing.an.adviser..looking.up.programs..etc.... == 'Yes')

consider <- datas%>%filter(Are.you.considering.joining.a.UW.study.abroad.program. != "No")
# STEM Major
stem <- datas%>%filter(Department == 'STEM')

stem.go <- stem%>%filter(Have.you.participated.in.UW.study.abroad.programs.before.=='Yes')

stem.not.consider <- stem%>%filter(Are.you.considering.joining.a.UW.study.abroad.program. == "No")

stem.consider <- stem.not.consider <- stem%>%filter(Are.you.considering.joining.a.UW.study.abroad.program. != "No")


stem.not.planning <- stem%>%filter(Have.you.taken.any.actions.on.planning.for.studying.abroad...Such.as.seeing.an.adviser..looking.up.programs..etc.... == 'No')

stem.planning <-stem%>%filter(Have.you.taken.any.actions.on.planning.for.studying.abroad...Such.as.seeing.an.adviser..looking.up.programs..etc.... == 'Yes')

# Other Major
other <- datas%>%filter(Department != 'STEM')
other.go <- other%>%filter(Have.you.participated.in.UW.study.abroad.programs.before.=='Yes')
other.not.consider <- other%>%filter(Are.you.considering.joining.a.UW.study.abroad.program. == "No")
other.not.planning <- other%>%filter(Have.you.taken.any.actions.on.planning.for.studying.abroad...Such.as.seeing.an.adviser..looking.up.programs..etc.... == 'No')
other.planning <-other%>%filter(Have.you.taken.any.actions.on.planning.for.studying.abroad...Such.as.seeing.an.adviser..looking.up.programs..etc.... == 'Yes')
other.consider <- other.not.consider <- other%>%filter(Are.you.considering.joining.a.UW.study.abroad.program. != "No")

#==================== considering ==============================

consider.time<- filter(consider, grepl("Time", NOTES2, ignore.case = TRUE))







#==================== not planning/ NOTES2 ============================
# factors
## Time
time<- filter(datas, grepl("Time", NOTES2, ignore.case = TRUE))
stem.time <- filter(stem, grepl("Time", NOTES2, ignore.case = TRUE))
other.time <- filter(other, grepl("Time", NOTES2, ignore.case = TRUE))
### time subfactors
#---overall---
time.graduate <-filter(time, grepl("graduate", NOTES2, ignore.case = TRUE))
time.major <- filter(time, grepl("major", NOTES2, ignore.case = TRUE))  
#time.personal <- filter(time, grepl("personal", NOTES2, ignore.case = TRUE))
time.schedule <- filter(time, grepl("schedule", NOTES2, ignore.case = TRUE))
time.job <- filter(time, grepl("job", NOTES2, ignore.case = TRUE))
time.work <-filter(time, grepl("work", NOTES2, ignore.case = TRUE))

#---STEM---
stem.time.graduate <-filter(stem.time, grepl("graduate", NOTES2, ignore.case = TRUE))
stem.time.major <- filter(stem.time, grepl("major", NOTES2, ignore.case = TRUE))  
#stem.time.personal <- filter(stem.time, grepl("personal", NOTES2, ignore.case = TRUE))
stem.time.schedule <- filter(stem.time, grepl("schedule", NOTES2, ignore.case = TRUE))
stem.time.job <- filter(stem.time, grepl("job", NOTES2, ignore.case = TRUE))
stem.time.work <-filter(stem.time, grepl("work", NOTES2, ignore.case = TRUE))
#---other---
other.time.graduate <-filter(other.time, grepl("graduate", NOTES2, ignore.case = TRUE))
other.time.major <- filter(other.time, grepl("major", NOTES2, ignore.case = TRUE))  
other.time.personal <- filter(stem.time, grepl("personal", NOTES2, ignore.case = TRUE))
other.time.schedule <- filter(stem.time, grepl("schedule", NOTES2, ignore.case = TRUE))
other.time.job <- filter(stem.time, grepl("job", NOTES2, ignore.case = TRUE))
other.time.work <-filter(stem.time, grepl("work", NOTES2, ignore.case = TRUE))


## Finance
finance <- filter(datas, grepl("finance", NOTES2, ignore.case = TRUE))
stem.finance <- filter(stem, grepl("finance", NOTES2, ignore.case = TRUE))
other.finance <- filter(other, grepl("finance", NOTES2, ignore.case = TRUE))
#---sub----
finance.expensive <- filter(finance, grepl("expensive", NOTES2, ignore.case = TRUE))
finance.scholarship <-  filter(finance, grepl("scholarship", NOTES2, ignore.case = TRUE))
finance.dependent <- filter(finance, grepl("dependent", NOTES2, ignore.case = TRUE))

## Uncertainty
uncertainty <- filter(datas, grepl("uncertainty", NOTES2, ignore.case = TRUE))
stem.uncertainty <- filter(stem, grepl("uncertainty", NOTES2, ignore.case = TRUE))
other.uncertainty <- filter(other, grepl("uncertainty", NOTES2, ignore.case = TRUE))
### subfactors
uncertainty.when <- filter(uncertainty, grepl("when", NOTES2, ignore.case = TRUE))
uncertainty.location <- filter(uncertainty, grepl("location", NOTES2, ignore.case = TRUE))
uncertainty.program <- filter(uncertainty, grepl("program", NOTES2, ignore.case = TRUE))
uncertainty.resource <- filter(uncertainty, grepl("resource", NOTES2, ignore.case = TRUE))
uncertainty.language <- filter(uncertainty, grepl("language", NOTES2, ignore.case = TRUE))
uncertainty.culture <- filter(uncertainty, grepl("culture", NOTES2, ignore.case = TRUE))
#---STEM---
s.uncertainty.when <- filter(stem.uncertainty, grepl("when", NOTES2, ignore.case = TRUE))
s.uncertainty.location <- filter(stem.uncertainty, grepl("location", NOTES2, ignore.case = TRUE))
s.uncertainty.program <- filter(stem.uncertainty, grepl("program", NOTES2, ignore.case = TRUE))
s.uncertainty.resource <- filter(stem.uncertainty, grepl("resource", NOTES2, ignore.case = TRUE))
s.uncertainty.language <- filter(stem.uncertainty, grepl("language", NOTES2, ignore.case = TRUE))
s.uncertainty.culture <- filter(stem.uncertainty, grepl("culture", NOTES2, ignore.case = TRUE))
#---OTHER---
o.uncertainty.when <- filter(other.uncertainty, grepl("when", NOTES2, ignore.case = TRUE))
o.uncertainty.location <- filter(other.uncertainty, grepl("location", NOTES2, ignore.case = TRUE))
o.uncertainty.program <- filter(other.uncertainty, grepl("program", NOTES2, ignore.case = TRUE))
o.uncertainty.resource <- filter(other.uncertainty, grepl("resource", NOTES2, ignore.case = TRUE))
o.uncertainty.language <- filter(other.uncertainty, grepl("language", NOTES2, ignore.case = TRUE))
o.uncertainty.culture <- filter(other.uncertainty, grepl("culture", NOTES2, ignore.case = TRUE))


## credit
credit <- filter(datas, grepl("credit", NOTES2, ignore.case = TRUE))
stem.credit <- filter(stem, grepl("credit", NOTES2, ignore.case = TRUE))
other.credit <- filter(other, grepl("credit", NOTES2, ignore.case = TRUE))

## application/Complexity
application <- filter(datas, grepl("application", NOTES2, ignore.case = TRUE))
stem.application <- filter(stem, grepl("application", NOTES2, ignore.case = TRUE))
other.application <- filter(other, grepl("application", NOTES2, ignore.case = TRUE))

#===========================problem when planning/NOTE3=========================================
# factors
## Time
time3<- filter(datas, grepl("Time", NOTES3, ignore.case = TRUE))
stem.time3 <- filter(stem, grepl("Time", NOTES3, ignore.case = TRUE))
other.time3 <- filter(other, grepl("Time", NOTES3, ignore.case = TRUE))

### time subfactors
#---overall---
time.graduate3 <-filter(time3, grepl("graduate", NOTES3, ignore.case = TRUE))
time.major3 <- filter(time3, grepl("major", NOTES3, ignore.case = TRUE))  
time.personal3 <- filter(time3, grepl("personal", NOTES3, ignore.case = TRUE))
time.schedule3 <- filter(time3, grepl("schedule", NOTES3, ignore.case = TRUE))
time.job3 <- filter(time3, grepl("job", NOTES3, ignore.case = TRUE))
time.work3 <-filter(time3, grepl("work", NOTES3, ignore.case = TRUE))

#---STEM---
stem.time.graduate3 <-filter(stem.time3, grepl("graduate", NOTES3, ignore.case = TRUE))
stem.time.major3 <- filter(stem.time3, grepl("major", NOTES3, ignore.case = TRUE))  
stem.time.personal3 <- filter(stem.time3, grepl("personal", NOTES3, ignore.case = TRUE))
stem.time.schedule3 <- filter(stem.time3, grepl("schedule", NOTES3, ignore.case = TRUE))
stem.time.job3 <- filter(stem.time3, grepl("job", NOTES3, ignore.case = TRUE))
stem.time.work3 <-filter(stem.time3, grepl("work", NOTES3, ignore.case = TRUE))
#---other---
other.time.graduate3 <-filter(other.time3, grepl("graduate", NOTES3, ignore.case = TRUE))
other.time.major3 <- filter(other.time3, grepl("major", NOTES3, ignore.case = TRUE))  
other.time.personal3 <- filter(stem.time3, grepl("personal", NOTES3, ignore.case = TRUE))
other.time.schedule3 <- filter(stem.time3, grepl("schedule", NOTES3, ignore.case = TRUE))
other.time.job3 <- filter(stem.time3, grepl("job", NOTES3, ignore.case = TRUE))
other.time.work3 <-filter(stem.time3, grepl("work", NOTES3, ignore.case = TRUE))


## Finance
finance3 <- filter(datas, grepl("finance", NOTES3, ignore.case = TRUE))
stem.finance3 <- filter(stem, grepl("finance", NOTES3, ignore.case = TRUE))
other.finance3 <- filter(other, grepl("finance", NOTES3, ignore.case = TRUE))

#---sub----
finance.expensive3 <- filter(finance3, grepl("expensive", NOTES3, ignore.case = TRUE))
finance.scholarship3 <-  filter(finance3, grepl("scholarship", NOTES3, ignore.case = TRUE))
finance.dependent3 <- filter(finance3, grepl("dependent", NOTES3, ignore.case = TRUE))

## Uncertainty
uncertainty3 <- filter(datas, grepl("uncertainty", NOTES3, ignore.case = TRUE))
stem.uncertainty3 <- filter(stem, grepl("uncertainty", NOTES3, ignore.case = TRUE))
other.uncertainty3 <- filter(other, grepl("uncertainty", NOTES3, ignore.case = TRUE))
### subfactors
uncertainty.when3 <- filter(uncertainty3, grepl("when", NOTES3, ignore.case = TRUE))
uncertainty.location3 <- filter(uncertainty3, grepl("location", NOTES3, ignore.case = TRUE))
uncertainty.program3 <- filter(uncertainty3, grepl("program", NOTES3, ignore.case = TRUE))
uncertainty.resource3 <- filter(uncertainty3, grepl("resource", NOTES3, ignore.case = TRUE))
uncertainty.language3 <- filter(uncertainty3, grepl("language", NOTES3, ignore.case = TRUE))
uncertainty.culture3 <- filter(uncertainty3, grepl("culture", NOTES3, ignore.case = TRUE))
#---STEM---
s.uncertainty.when3 <- filter(stem.uncertainty3, grepl("when", NOTES3, ignore.case = TRUE))
s.uncertainty.location3 <- filter(stem.uncertainty3, grepl("location", NOTES3, ignore.case = TRUE))
s.uncertainty.program3 <- filter(stem.uncertainty3, grepl("program", NOTES3, ignore.case = TRUE))
s.uncertainty.resource3 <- filter(stem.uncertainty3, grepl("resource", NOTES3, ignore.case = TRUE))
s.uncertainty.language3 <- filter(stem.uncertainty3, grepl("language", NOTES3, ignore.case = TRUE))
s.uncertainty.culture3 <- filter(stem.uncertainty3, grepl("culture", NOTES3, ignore.case = TRUE))

o.uncertainty.culture3 <- filter(other.uncertainty3, grepl("culture", NOTES3, ignore.case = TRUE))


## credit
credit3 <- filter(datas, grepl("credit", NOTES3, ignore.case = TRUE))
stem.credit3 <- filter(stem, grepl("credit", NOTES3, ignore.case = TRUE))
other.credit3 <- filter(other, grepl("credit", NOTES3, ignore.case = TRUE))

## application/Complexity
application3 <- filter(datas, grepl("application", NOTES3, ignore.case = TRUE))
stem.application3 <- filter(stem, grepl("application", NOTES3, ignore.case = TRUE))
other.application3 <- filter(other, grepl("application", NOTES3, ignore.case = TRUE))



