library(tidyverse)
library(haven)
# install.packages("ggrepel")
# install.packages("gt")
library(ggrepel)
library(gt)
df4 <-read_dta("reprecation_data/Albouy(2016) replication package/stcmsa2000_wacw_albouy.dta")#成形済みのデータ
#
df4 <- df4 %>% 
  mutate(pop5m = case_when(
    pop > 5000000 ~ 0,
    pop > 1500000 & pop <= 5000000 ~ 1, 
    pop > 500000 & pop <= 1500000 ~ 2,
    pop < 500000 ~ 3 
  )) %>%
  mutate(pop5m = if_else(cmsa>10000,4,pop5m))
#PとWについて散布図を作成

df4$pop5m <- as.factor(df4$pop5m)

df4 %>% 
  ggplot(aes(x=w,y=p,label=shortname,colour = pop5m))+
  geom_point(shape=21, fill="white")+
  geom_text_repel()+
  geom_abline(aes(intercept = 0,slope = 1.534))

#table1作成
df4 %>% 
  mutate(w  = round(as.numeric(w),digits = 2)) %>%
  mutate(p  = round(as.numeric(p),digits = 2)) %>%
  mutate(r  = 4.29*p-2.64*w) %>% 
  mutate(ax = 0.11*p+0.76*w) %>% 
  mutate(q  = 0.32*p-0.48*w) %>% 
  mutate(om = 0.39*p+0.01*w) %>% 
  group_by(cmsaname)%>% 
  summarize(population=sum(pop),
          housingcosts=mean(p),
          wage = mean(w),
          land_rent =mean(r),
          trade_product=mean(ax),
          quality_of_life=mean(q),
          total_amenity_value=mean(om)
          )%>% 
  arrange(desc(housingcosts)) %>%
  gt()
    