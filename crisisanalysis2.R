
library(readxl)
library(ggfortify)
library(tidyverse)
library(data.table)
library(scales)
library(lubridate)
library(zoo)
library(rworldmap)
library(leaflet)
library(grid)
library(gridExtra)

##reading in west african crisi data##
w <- read_xlsx("westafrica2.xlsx")

wac <- w %>% 
  select(COUNTRY,YEAR,ACTOR1,EVENT_DATE,EVENT_TYPE,LATITUDE,LONGITUDE,NOTES)
#write.csv(wac,"wac.csv")

###selecting the country,year,actor1,event_date for all the countries###
w1 <- w %>% 
  select(COUNTRY,YEAR,ACTOR1,EVENT_DATE) %>%
  group_by(EVENT_DATE,COUNTRY) %>% 
  summarise(number = n())
#write.csv(w1,"wa1.csv")

##getting distinct country names to plot###

w3 <- w %>% 
  select(COUNTRY,YEAR,ACTOR1,EVENT_DATE) %>%
  group_by(COUNTRY) %>% 
  summarise(number = n())
#write.csv(w3,"wa.csv")

malMap <- joinCountryData2Map(w3, joinCode = "NAME",
                              nameJoinColumn = "COUNTRY")

mapCountryData(malMap, nameColumnToPlot="COUNTRY", catMethod = "categorical",
               missingCountryCol = gray(.8))

##interactive map with leaflet##
w5 <- w %>% 
  select(COUNTRY,LATITUDE,LONGITUDE,YEAR) %>%
  group_by(COUNTRY,LATITUDE,LONGITUDE,YEAR) %>% 
  summarize(number = n())

####function for getting specific years###
w2 <- w %>% 
  select(COUNTRY,YEAR,ACTOR1,EVENT_DATE) %>%
  group_by(YEAR,COUNTRY) %>% 
  summarise(number = n())


####BARPLOT ON YEARLY CRISIS ACROSS THE 15 COUNTRIES##
#jpeg('Barplot.jpg')
t <- ggplot(w2, aes(fill=COUNTRY, y=number, x=YEAR)) +
  geom_bar(colour="black",position="dodge", stat="identity")+ 
  facet_wrap(~COUNTRY)
t

###Extracting the individual countries###

NIG1 <- w1 %>% filter(COUNTRY == 'Nigeria')
SE1 <- w1 %>% filter(COUNTRY == 'Sierra Leone')
NIGR1 <- w1 %>% filter(COUNTRY == 'Niger')
LB1 <- w1 %>% filter(COUNTRY == 'Liberia')
GUI1 <- w1 %>% filter(COUNTRY == 'Guinea')
SEN1 <- w1 %>% filter(COUNTRY == 'Senegal')
IV1 <- w1 %>% filter(COUNTRY == 'Ivory Coast')
MAU1 <- w1 %>% filter(COUNTRY == 'Mauritania')
GB1 <- w1 %>% filter(COUNTRY == 'Guinea-Bissau')
BF1 <- w1 %>% filter(COUNTRY == 'Burkina Faso')
GAM1 <- w1 %>% filter(COUNTRY == 'Gambia')
GHA1 <- w1 %>% filter(COUNTRY == 'Ghana')
TOG1 <- w1 %>% filter(COUNTRY == 'Togo')
BEN1 <- w1 %>% filter(COUNTRY == 'Benin')
MAL1 <- w1 %>% filter(COUNTRY == 'Mali')


###nig2##
nig2 <- NIG1 %>% separate(EVENT_DATE, into = c("year", "month", "day"), sep="-") %>% 
  mutate(year = as.numeric(year)) %>% 
  mutate(month = as.numeric(month))%>% 
  group_by(year,month) %>% 
  summarize(numb=sum(number))

##se1##
se2 <- SE1 %>% separate(EVENT_DATE, into = c("year", "month", "day"), sep="-") %>% 
  mutate(year = as.numeric(year)) %>% 
  mutate(month = as.numeric(month))%>% 
  group_by(year,month) %>% 
  summarize(numb=sum(number))

##nigr1##
nigr2 <- NIGR1 %>% separate(EVENT_DATE, into = c("year", "month", "day"), sep="-") %>% 
  mutate(year = as.numeric(year)) %>% 
  mutate(month = as.numeric(month))%>% 
  group_by(year,month) %>% 
  summarize(numb=sum(number))

##lb2##
lb2 <- LB1 %>% separate(EVENT_DATE, into = c("year", "month", "day"), sep="-") %>% 
  mutate(year = as.numeric(year)) %>% 
  mutate(month = as.numeric(month))%>% 
  group_by(year,month) %>% 
  summarize(numb=sum(number))

##gui2###
gui2 <- GUI1 %>% separate(EVENT_DATE, into = c("year", "month", "day"), sep="-") %>% 
  mutate(year = as.numeric(year)) %>% 
  mutate(month = as.numeric(month))%>% 
  group_by(year,month) %>% 
  summarize(numb=sum(number))

##sen2##
sen2 <- SEN1 %>% separate(EVENT_DATE, into = c("year", "month", "day"), sep="-") %>% 
  mutate(year = as.numeric(year)) %>% 
  mutate(month = as.numeric(month))%>% 
  group_by(year,month) %>% 
  summarize(numb=sum(number))

##iv2##
iv2 <- IV1 %>% separate(EVENT_DATE, into = c("year", "month", "day"), sep="-") %>% 
  mutate(year = as.numeric(year)) %>% 
  mutate(month = as.numeric(month))%>% 
  group_by(year,month) %>% 
  summarize(numb=sum(number))

##mau2##
mau2 <- MAU1 %>% separate(EVENT_DATE, into = c("year", "month", "day"), sep="-") %>% 
  mutate(year = as.numeric(year)) %>% 
  mutate(month = as.numeric(month))%>% 
  group_by(year,month) %>% 
  summarize(numb=sum(number))

##gb2##
gb2 <- GB1 %>% separate(EVENT_DATE, into = c("year", "month", "day"), sep="-") %>% 
  mutate(year = as.numeric(year)) %>% 
  mutate(month = as.numeric(month))%>% 
  group_by(year,month) %>% 
  summarize(numb=sum(number))

##bf2##
bf2 <- BF1 %>% separate(EVENT_DATE, into = c("year", "month", "day"), sep="-") %>% 
  mutate(year = as.numeric(year)) %>% 
  mutate(month = as.numeric(month))%>% 
  group_by(year,month) %>% 
  summarize(numb=sum(number))

##gam2##
gam2 <- GAM1 %>% separate(EVENT_DATE, into = c("year", "month", "day"), sep="-") %>% 
  mutate(year = as.numeric(year)) %>% 
  mutate(month = as.numeric(month))%>% 
  group_by(year,month) %>% 
  summarize(numb=sum(number))

##gha2##
gha2 <- GHA1 %>% separate(EVENT_DATE, into = c("year", "month", "day"), sep="-") %>% 
  mutate(year = as.numeric(year)) %>% 
  mutate(month = as.numeric(month))%>% 
  group_by(year,month) %>% 
  summarize(numb=sum(number))

##tog2##
tog2 <- TOG1 %>% separate(EVENT_DATE, into = c("year", "month", "day"), sep="-") %>% 
  mutate(year = as.numeric(year)) %>% 
  mutate(month = as.numeric(month))%>% 
  group_by(year,month) %>% 
  summarize(numb=sum(number))

##ben2##
ben2 <- BEN1 %>% separate(EVENT_DATE, into = c("year", "month", "day"), sep="-") %>% 
  mutate(year = as.numeric(year)) %>% 
  mutate(month = as.numeric(month))%>% 
  group_by(year,month) %>% 
  summarize(numb=sum(number))

##mal2##
mal2 <- MAL1 %>% separate(EVENT_DATE, into = c("year", "month", "day"), sep="-") %>% 
  mutate(year = as.numeric(year)) %>% 
  mutate(month = as.numeric(month))%>% 
  group_by(year,month) %>% 
  summarize(numb=sum(number))

cols <- c("red","blue","darkgreen","orange","darkblue","blue4","blueviolet","brown",
          "chartreuse1","chocolate4","black","chartreuse",
          "cadetblue","coral","burlywood2")

##Getting individual time-series plots######

Plot1 <-autoplot(ts(nig2$numb,
                    start = c(1997,1), frequency = 12))
Plot2 <-autoplot(ts(se2$numb,
                    start = c(1997,1), frequency = 12))
Plot3 <-autoplot(ts(nigr2$numb,
                    start = c(1997,1), frequency = 12))
Plot4 <-autoplot(ts(lb2$numb,
                    start = c(1997,1), frequency = 12))
Plot5 <-autoplot(ts(gui2$numb,
                    start = c(1997,1), frequency = 12))
Plot6 <-autoplot(ts(sen2$numb,
                    start = c(1997,1), frequency = 12))
Plot7 <-autoplot(ts(iv2$numb,
                    start = c(1997,1), frequency = 12))
Plot8 <-autoplot(ts(mau2$numb,
                    start = c(1997,1), frequency = 12))
Plot9 <-autoplot(ts(gb2$numb,
                    start = c(1997,1), frequency = 12))
Plot10 <-autoplot(ts(bf2$numb,
                    start = c(1997,1), frequency = 12))
Plot11 <-autoplot(ts(gam2$numb,
                    start = c(1997,1), frequency = 12))
Plot12 <-autoplot(ts(gha2$numb,
                    start = c(1997,1), frequency = 12))
Plot13 <-autoplot(ts(tog2$numb,
                     start = c(1997,1), frequency = 12))
Plot14 <-autoplot(ts(ben2$numb,
                     start = c(1997,1), frequency = 12))
Plot15 <-autoplot(ts(mal2$numb,
                     start = c(1997,1), frequency = 12))
myPlotList <- list(Plot1,Plot2,Plot3,Plot4,Plot5,Plot6,Plot7,Plot8,Plot9,Plot10,Plot11,Plot12,
                   Plot13,Plot14,Plot15)

do.call(grid.arrange, c(myPlotList, list(ncol = 2)))


grid.arrange(Plot1,Plot2,Plot3,Plot4,Plot5,Plot6,Plot7,Plot8,Plot9,Plot10,Plot11,Plot12,
             Plot13,Plot14,Plot15)




##Time series on the 15 countries##
p <- autoplot(ts(cbind(nig2$numb, se2$numb,nigr2$numb,
                   lb2$numb,gui2$numb,sen2$numb,
                   iv2$numb,mau2$numb,gb2$numb,
                   bf2$numb,gam2$numb,gha2$numb,
                   tog2$numb,ben2$numb,mal2$numb),
             start = c(1997,1), frequency = 12),
             ts.colour = 'variable',
         facets = FALSE)


p1 <- p + xlab('Year') + ylab('Crisis Count')+scale_colour_manual(values = cols,
                                                           labels = c('Nigeria',
                                                                      'Sierra Leone',
                                                                      'Niger',
                                                                      'Liberia',
                                                                      'Guinea',
                                                                      'Senegal',
                                                                      'Ivory Coast',
                                                                      'Mauritania',
                                                                      'Guinea-Bissau',
                                                                      'Burkina Faso',
                                                                      'Gambia',
                                                                      'Ghana',
                                                                      'Togo',
                                                                      'Benin',
                                                                      'Mali'))
p1
####Comparing monthly crisis within each year###
MC <- w %>% 
  separate(EVENT_DATE,c("y","MONTH","DATE")) %>% 
  select(YEAR,COUNTRY,EVENT_TYPE,ACTOR1,LOCATION,MONTH) %>%
  group_by(YEAR,MONTH,COUNTRY) %>% 
  summarise(number = n())

MC1 <-  MC %>% 
  spread(MONTH,number)

###rename the columns to months###
MC1 <- rename(MC1, January = '01',
              Feburary = '02',
              March = '03',
              April = '04',
              May = '05',
              June = '06',
              July = '07',
              August = '08',
              September = '09',
              October = '10',
              November = '11',
              December = '12')
##converting na's to zero##
MC1[ ,3:14][is.na(MC1[ ,3:14] ) ] = 0

###monthly time series representation of the monthly comparison###
##which month has had the largest crisis in each year###

myfunction <- function(x){
  query = MC1 %>% filter(YEAR == x)
  return(query)
}

MC1997 <- myfunction(1997)
MC1998 <- myfunction(1998)
MC1999 <- myfunction(1999)
MC2000 <- myfunction(2000)
MC2001 <- myfunction(2001)
MC2002 <- myfunction(2002)
MC2003 <- myfunction(2003)
MC2004 <- myfunction(2004)
MC2005 <- myfunction(2005)
MC2006 <- myfunction(2006)
MC2007 <- myfunction(2007)
MC2008 <- myfunction(2008)
MC2009 <- myfunction(2009)
MC2010 <- myfunction(2010)
MC2011 <- myfunction(2011)
MC2012 <- myfunction(2012)
MC2013 <- myfunction(2013)
MC2014 <- myfunction(2014)
MC2015 <- myfunction(2015)
MC2016 <- myfunction(2016)
MC2017 <- myfunction(2017)
MC2018 <- myfunction(2018)

##Preparing data for CHI-square test##
MC1997t <-  subset(MC1997, select= -c(YEAR,COUNTRY)) 
MC1997a <- rbind(MC1997t , colSums(MC1997t))
rownames(MC1997a) <- c(rownames(MC1997a)[-length(rownames(MC1997a))], "Total")
MC1997a$Country <- (MC1997$COUNTRY %>% append("Total"))
#######summing columns for 1998 crisis##
MC1998t <-  subset(MC1998, select= -c(YEAR,COUNTRY)) 
MC1998a <- rbind(MC1998t , colSums(MC1998t))
rownames(MC1998a) <- c(rownames(MC1998a)[-length(rownames(MC1998a))], "Total")
MC1998a$Country <- (MC1998$COUNTRY %>% append("Total"))
#######summing columns for 1999 crisis##
MC1999t <-  subset(MC1999, select= -c(YEAR,COUNTRY)) 
MC1999a <- rbind(MC1999t , colSums(MC1999t))
rownames(MC1999a) <- c(rownames(MC1999a)[-length(rownames(MC1999a))], "Total")
MC1999a$Country <- (MC1999$COUNTRY %>% append("Total"))
##summing the columns year 2000##
MC2000t <-  subset(MC2000, select= -c(YEAR,COUNTRY)) 
MC2000a <- rbind(MC2000t , colSums(MC2000t))
rownames(MC2000a) <- c(rownames(MC2000a)[-length(rownames(MC2000a))], "Total")
MC2000a$Country <- (MC2000a$COUNTRY %>% append("Total"))
##summing the columns year 2001##
MC2001t <-  subset(MC2001, select= -c(YEAR,COUNTRY)) 
MC2001a <- rbind(MC2001t , colSums(MC2001t))
rownames(MC2001a) <- c(rownames(MC2001a)[-length(rownames(MC2001a))], "Total")
MC2001a$Country <- (MC2001a$COUNTRY %>% append("Total"))
##summing the columns year 2002##
MC2002t <-  subset(MC2002, select= -c(YEAR,COUNTRY)) 
MC2002a <- rbind(MC2002t , colSums(MC2002t))
rownames(MC2002a) <- c(rownames(MC2002a)[-length(rownames(MC2002a))], "Total")
MC2002a$Country <- (MC2002a$COUNTRY %>% append("Total"))
##summing the columns year 2003##
MC2003t <-  subset(MC2003, select= -c(YEAR,COUNTRY)) 
MC2003a <- rbind(MC2003t , colSums(MC2003t))
rownames(MC2003a) <- c(rownames(MC2003a)[-length(rownames(MC2003a))], "Total")
MC2003a$Country <- (MC2003a$COUNTRY %>% append("Total"))
##summing the columns year 2004##
MC2004t <-  subset(MC2004, select= -c(YEAR,COUNTRY)) 
MC2004a <- rbind(MC2004t , colSums(MC2004t))
rownames(MC2004a) <- c(rownames(MC2004a)[-length(rownames(MC2004a))], "Total")
MC2004a$Country <- (MC2004a$COUNTRY %>% append("Total"))
###summing the columns year 2005###
MC2005t <-  subset(MC2005, select= -c(YEAR,COUNTRY)) 
MC2005a <- rbind(MC2005t , colSums(MC2005t))
rownames(MC2005a) <- c(rownames(MC2005a)[-length(rownames(MC2004a))], "Total")
MC2005a$Country <- (MC2005a$COUNTRY %>% append("Total"))
###summing the columns year 2006###
MC2006t <-  subset(MC2006, select= -c(YEAR,COUNTRY)) 
MC2006a <- rbind(MC2006t , colSums(MC2006t))
rownames(MC2006a) <- c(rownames(MC2006a)[-length(rownames(MC2006a))], "Total")
MC2006a$Country <- (MC2006a$COUNTRY %>% append("Total"))
###summing the columns year 2007###
MC2007t <-  subset(MC2007, select= -c(YEAR,COUNTRY)) 
MC2007a <- rbind(MC2007t , colSums(MC2007t))
rownames(MC2007a) <- c(rownames(MC2007a)[-length(rownames(MC2007a))], "Total")
MC2007a$Country <- (MC2007a$COUNTRY %>% append("Total"))
###summing the columns year 2008###
MC2008t <-  subset(MC2008, select= -c(YEAR,COUNTRY)) 
MC2008a <- rbind(MC2008t , colSums(MC2008t))
rownames(MC2008a) <- c(rownames(MC2008a)[-length(rownames(MC2008a))], "Total")
MC2008a$Country <- (MC2008a$COUNTRY %>% append("Total"))
###summing the columns year 2009###
MC2009t <-  subset(MC2009, select= -c(YEAR,COUNTRY)) 
MC2009a <- rbind(MC2009t , colSums(MC2009t))
rownames(MC2009a) <- c(rownames(MC2009a)[-length(rownames(MC2009a))], "Total")
MC2009a$Country <- (MC2009a$COUNTRY %>% append("Total"))
###summing the columns year 2010###
MC2010t <-  subset(MC2010, select= -c(YEAR,COUNTRY)) 
MC2010a <- rbind(MC2010t , colSums(MC2010t))
rownames(MC2010a) <- c(rownames(MC2010a)[-length(rownames(MC2010a))], "Total")
MC2010a$Country <- (MC2010a$COUNTRY %>% append("Total"))
###summing the columns year 2011###
MC2011t <-  subset(MC2011, select= -c(YEAR,COUNTRY)) 
MC2011a <- rbind(MC2011t , colSums(MC2011t))
rownames(MC2011a) <- c(rownames(MC2011a)[-length(rownames(MC2011a))], "Total")
MC2011a$Country <- (MC2011a$COUNTRY %>% append("Total"))
###summing the columns year 2012###
MC2012t <-  subset(MC2012, select= -c(YEAR,COUNTRY)) 
MC2012a <- rbind(MC2012t , colSums(MC2012t))
rownames(MC2012a) <- c(rownames(MC2012a)[-length(rownames(MC2012a))], "Total")
MC2012a$Country <- (MC2012a$COUNTRY %>% append("Total"))
###summing the columns year 2013###
MC2013t <-  subset(MC2013, select= -c(YEAR,COUNTRY)) 
MC2013a <- rbind(MC2013t , colSums(MC2013t))
rownames(MC2013a) <- c(rownames(MC2013a)[-length(rownames(MC2013a))], "Total")
MC2013a$Country <- (MC2013a$COUNTRY %>% append("Total"))
###summing the columns year 2014###
MC2014t <-  subset(MC2014, select= -c(YEAR,COUNTRY)) 
MC2014a <- rbind(MC2014t , colSums(MC2014t))
rownames(MC2014a) <- c(rownames(MC2014a)[-length(rownames(MC2014a))], "Total")
MC2014a$Country <- (MC2014a$COUNTRY %>% append("Total"))
###summing columns in year 2015###
MC2015t <-  subset(MC2015, select= -c(YEAR,COUNTRY)) 
MC2015a <- rbind(MC2015t , colSums(MC2015t))
rownames(MC2015a) <- c(rownames(MC2015a)[-length(rownames(MC2015a))], "Total")
MC2015a$Country <- (MC2015a$COUNTRY %>% append("Total"))
###summing columns in year 2016###
MC2016t <-  subset(MC2016, select= -c(YEAR,COUNTRY)) 
MC2016a <- rbind(MC2016t , colSums(MC2016t))
rownames(MC2016a) <- c(rownames(MC2016a)[-length(rownames(MC2016a))], "Total")
MC2016a$Country <- (MC2016a$COUNTRY %>% append("Total"))
###summing columns in year 2017###
MC2017t <-  subset(MC2017, select= -c(YEAR,COUNTRY)) 
MC2017a <- rbind(MC2017t , colSums(MC2017t))
rownames(MC2017a) <- c(rownames(MC2017a)[-length(rownames(MC2017a))], "Total")
MC2017a$Country <- (MC2017a$COUNTRY %>% append("Total"))
###summing columns in year 2018###
MC2018t <-  subset(MC2018, select= -c(YEAR,COUNTRY)) 
MC2018a <- rbind(MC2018t , colSums(MC2018t))
rownames(MC2018a) <- c(rownames(MC2018a)[-length(rownames(MC2018a))], "Total")
MC2018a$Country <- (MC2018a$COUNTRY %>% append("Total"))

###chi-square test###
sum3y <- MC1997a[16,1:12]+MC1998a[16,1:12]+MC1999a[16,1:12]+MC2000a[16,1:12]+MC2001a[16,1:12]+
  MC2002a[16,1:12]+MC2003a[16,1:12]+MC2004a[16,1:12]+MC2005a[16,1:12]+MC2006a[16,1:12]+MC2007a[16,1:12]+
  MC2008a[16,1:12]+MC2009a[16,1:12]+MC2010a[16,1:12]+MC2011a[16,1:12]+MC2012a[16,1:12]+MC2013a[16,1:12]+
  MC2014a[16,1:12]+MC2015a[16,1:12]+MC2016a[16,1:12]+MC2017a[16,1:12]+MC2018a[16,1:12]

###summing all years###
total <-  sum(sum3y)

col_dat <- as.tibble( t(rbind(colnames(sum3y),sum3y)))
col_dat <- mutate(col_dat, V1=as.factor(V1))
col_dat <- mutate(col_dat, V2=as.numeric(V2))
write_csv(col_dat,"col_dat.csv")

g <- ggplot(col_dat, aes(x = V1, y=V2)) + 
    geom_col() +labs(x="Month",y="Count") +
    theme(axis.text.x = element_text(angle = 60, hjust = 1))
g



######################Chi-Square test ##################### 
observed <- as.vector(sum3y[1,]/total)

expected <- rep(1/12,12)


Chi <- chisq.test(x = observed,
           p = expected)
Chi



