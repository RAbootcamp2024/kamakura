#install.packages("tidyverse")
#install.packages("haven")
library(tidyverse)
library(haven)

df1<-read_dta("reprecation_data/Albouy(2016) replication package/ipums2000h_wacw_albouy.dta")
df2 <-read_dta("reprecation_data/Albouy(2016) replication package/ipums2000p_wacw_albouy.dta")
df3 <-read_dta("reprecation_data/Albouy(2016) replication package/match_puma2000_pmsa99.dta")#wage housing differentials use
df4 <-read_dta("reprecation_data/Albouy(2016) replication package/stcmsa2000_wacw_albouy.dta")#

colnames(df1)
colnames(df2)
colnames(df3)
colnames(df4)
class(df1)
class(df2)
class(df3)
class(df4)
data44<-subset(df2, statefip==44)
data44_year<-subset(data44, age>=25&age<=55)
data44_year<- mutate(data44_year, expo=age-educ-5)
data44_year<- mutate(data44_year, immig_dummy=if_else(yrimmig!=0,1,0))
data44_year<- mutate(data44_year, eng_no=if_else(speakeng==1,1,0))
data44_year<- mutate(data44_year, eng_bad=if_else(speakeng==5,1,0))
data44_year<- mutate(data44_year, eng_good=if_else(speakeng==6,1,0))

max(data44_year$ind1950)
data44_year$ind1950<-round(data44_year$ind1950/100,digits = 0)
view(data44_year)
data44_year$occ1950<-round(data44_year$occ1950/100,digits = 0)
data44_year<- mutate(data44_year, vetage = if_else(vetstat == 2, age, 0))

