#install.packages("tidyverse")
#install.packages("haven")
library(tidyverse)
library(haven)

df1<-read_dta("reprecation_data/Albouy(2016) replication package/ipums2000h_wacw_albouy.dta")
df2 <-read_dta("reprecation_data/Albouy(2016) replication package/ipums2000p_wacw_albouy.dta")
df3 <-read_dta("reprecation_data/Albouy(2016) replication package/match_puma2000_pmsa99.dta")#wage housing differentials use
df4 <-read_dta("reprecation_data/Albouy(2016) replication package/stcmsa2000_wacw_albouy.dta")#


data44_year<-subset(df2, age>=25&age<=55)
data44_year<- mutate(data44_year, immig_dummy=if_else(yrimmig!=0,1,0))
data44_year<- mutate(data44_year, eng_no=if_else(speakeng==1,1,0))
data44_year<- mutate(data44_year, eng_bad=if_else(speakeng==5,1,0))
data44_year<- mutate(data44_year, eng_good=if_else(speakeng==6,1,0))

data44_year$ind1950<-round(data44_year$ind1950/100,digits = 0)
data44_year$occ1950<-round(data44_year$occ1950/100,digits = 0)
data44_year<- mutate(data44_year, vetage = if_else(vetstat == 2, age, 0))

data44_year <- data44_year %>%
  mutate(black = if_else(race==2,1,0)) %>%
  mutate(native = if_else(race==3,1,0)) %>%
  mutate(asia = if_else(4<=race&race<=6,1,0)) %>%
  mutate(other = if_else(7<=race&race<=10,1,0)) %>%
  mutate(hispan = if_else(1<=hispan&hispan<=3,1,0))
#IMMIGRATION STATUS INTERACTED WITH TIME IN COUNTRY AND MINORITY STATUS
data44_year<-data44_year%>%
  mutate(im_usyrs=immig_dummy*(year-yrimmig))%>%
  mutate(im_hisp=immig_dummy*hispan)%>%
  mutate(im_black=immig_dummy*black)%>%
  mutate(im_asia=immig_dummy*asia)%>%
  mutate(im_other=immig_dummy*other)
#AGE FROM 25 TO 55, NON-FARM, NON-GROUP QUARTERS
data44_year<-data44_year%>%
  filter(farm!=2, gq<3 | gq>4) %>%
  filter(classwkrd>=22 & classwkrd <=28)%>%
  filter(empstat==1)%>%
  filter(uhrswork >=30 & uhrswork <=99)%>%
  filter(wkswork1 >=26 & wkswork1<=52)

data44_year<-data44_year%>%
  
  mutate(lhrwage = log(incwage/(wkswork1*uhrswork))) %>%
  group_by(statefip)%>%
  mutate(stmaxwage = max(incwage)/2000) %>% 
#極端な値を消す
  filter(lhrwage >= log(2)) %>% 
  filter(lhrwage <= log(2*max(incwage)))
#教育に関するダミー変数を追加
data44_year<-data44_year %>% 
  mutate(schyrs =　case_when(
    educ == 0 ~ 0,
    educ== 1 ~ 2.5,
    educ == 2 ~ 6.5,
    educ == 3 ~ 9,
    educ == 4 ~ 10,
    educ == 5 ~ 11,
    educ == 6 ~ 12,
    educ == 7 ~ 13,
    educ >= 8 & educ <= 9~ 14,
    educ == 10 ~ 16,
    educ == 11 ~ 18
  ))  %>% 
  mutate(potexp = age - schyrs -5)
 #215行目から 

df0<-inner_join(data44_year,df3,by=c("statefip","puma"))

data_joined <- df0 %>% 
  mutate(afact = pop/pumapop) %>% 
  mutate(oldperwt = perwt) %>% 
  mutate(perwt = perwt*afact) %>% 
  filter(afact != 0) %>% 
  mutate(female = if_else(sex == 2, 1, 0))

library(plm)
fixed <- plm(lhrwage ~ female, data=data_joined, index=c("cmsa"), model="within", weights = perwt)
summary(fixed)
fixed_effects<-fixef(fixed)
