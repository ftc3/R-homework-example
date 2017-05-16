rm(list=ls())
# install.packages('RSQLite')
#install.packages('ggplot2')
setwd("~/PycharmProjects/hw8")

library(DBI)
library(RSQLite)
library(data.table)
library(ggplot2)
drive = dbDriver("SQLite")
connect  = dbConnect(drive, db = "~/PycharmProjects/hw8/vehicles.db")
# call on the table created in the db file vdb
vtb = dbGetQuery(connect, "select * from vdb")
head(vtb,10)

# it looked like the following weren't in the right 'class' (data type)
#class(vtb$year)
vtb$year <- as.numeric(vtb$year)
vtb$cylinders <- as.numeric(vtb$cylinders)
vtb$displ <- as.numeric(vtb$displ)
vtb$city08 <- as.numeric(vtb$city08)
vtb$highway08 <- as.numeric(vtb$highway08)
vtb$comb08 <- as.numeric(vtb$comb08)
summary(vtb)

# convert make VClass cylinders trany into factors
vtb$make <- as.factor(vtb$make)
vtb$VClass <- as.factor(vtb$VClass)
vtb$cylinders <- as.factor(vtb$cylinders)
vtb$trany <- as.factor(vtb$trany)
class(vtb$make)
summary(vtb)

DT <- data.table(vtb)
#DT the data table looks good
DT[, class_n:= .N, by = VClass]
?subset
sub <- subset(DT, class_n > 40)
sub <- data.table(sub)

# tabulation showing frequency of each VClass
#table(DT$class_n, DT$VClass)

# mean mpg x class, make, year
sub[, MPG_per_year:= mean(comb08), by= .(VClass, year, make)]
sub[, MPG_all_years:= mean(comb08), by= .(VClass, make)]
class(sub$VClass)

?aggregate
?subset
#max <- aggregate(MPG_all_years, by= .(VClass, make), data= sub, FUN= max)
vec <- unique(sub$VClass)
#vec
#ggplot(sub, aes(x=year, y= MPG_per_year, colour= make)) + stat_summary(fun.y= "max", geom="line") + guides(col= guide_legend(ncol=2)) + xlab("Year")+ylab("Mean Combined MPG") + ggtitle('i')
#ggplot(sub, aes(x=make, y=MPG_all_years)) + stat_summary(fun.y= "max") + geom_bar(stat='identity', alpha=.6) + xlab("Make") + ylab("Mean Combined MPG in All Years") + theme(axis.text.x= element_text(angle=90, hjust=1)) + ggtitle('i')
for (i in vec) {
  ssub <- subset(sub, VClass==i)
  # graph MPG_per_year
  print(ggplot(ssub, aes(x=year, y= MPG_per_year, colour= make)) + stat_summary(fun.y= "max", geom="line") + guides(col= guide_legend(ncol=2)) + xlab("Year")+ylab("Mean Combined MPG") + ggtitle(i))
  # need to get one observation per make from ssub now
  #interact <- interaction(ssub$VClass, ssub$make, drop = TRUE)
  ssub <-subset(ssub, !duplicated(make))
  #merge(aggregate(ssub$MPG_all_years, by= list(ssub$make), max), ssub)
  
  print(ggplot(ssub, aes(x=reorder(make, -MPG_all_years), y=MPG_all_years)) + stat_summary(fun.y= "max") + geom_bar(stat='identity', alpha=.6) + xlab("Make") + ylab("Mean Combined MPG in All Years") + theme(axis.text.x= element_text(angle=90, hjust=1)) + ggtitle(i))
}


