library(tidyverse)
library(haven)
# install.packages("ggrepel")
# install.packages("gt")
library(ggrepel)
library(gt)
df4 <-read_dta("reprecation_data/Albouy(2016) replication package/stcmsa2000_wacw_albouy.dta")#成形済みのデータ
#
df4 <- df4 %>% 
  mutate(type_of_city = case_when(
    pop > 5000000 ~ "Pop.5.0 > Million",
    pop > 1500000 & pop <= 5000000 ~ "Pop. 1.5-5.0 Million", 
    pop > 500000 & pop <= 1500000 ~ "Pop. 0.5-1.5 Million",
    pop < 500000 ~ "Pop.0.5 < Million" 
  )) %>%
  mutate(type_of_city = if_else(cmsa>10000,"Non-Metro Areas",type_of_city))
#PとWについて散布図を作成

df4$type_of_city <- as.factor(df4$type_of_city)

df4 %>% 
  ggplot(aes(x=w,y=p,label=shortname,colour = type_of_city))+
  geom_point(shape=21, fill="white")+
  geom_text_repel()+
  geom_abline(aes(intercept = 0,slope = 1.534))+
  labs(x = "Log Wage Differential", y = "Log Housing-Cost Differential")

#table1作成
df4 %>% 
  mutate(w  = round(as.numeric(w),digits = 2)) %>%
  mutate(p  = round(as.numeric(p),digits = 2)) %>%
  mutate(pop  = round(as.numeric(pop),digits = 2)) %>%
  mutate(r  = 4.29*p-2.64*w) %>% 
  mutate(ax = 0.11*p+0.76*w) %>% 
  mutate(q  = 0.32*p-0.48*w) %>% 
  mutate(om = 0.39*p+0.01*w) %>% 
  mutate(r = round(as.numeric(r),digits = 2)) %>%
  mutate(ax  = round(as.numeric(ax),digits = 2)) %>%
  mutate(q  = round(as.numeric(q),digits = 2)) %>%
  mutate(om  = round(as.numeric(om),digits = 2)) %>%
  group_by(cmsaname)%>% 
  summarize(population        = sum(pop),
          housingcosts        = mean(p),
          wage                = mean(w),
          land_rent           = sum(r*pop)/sum(pop),
          trade_product       = sum(ax*pop)/sum(pop),
          quality_of_life     = sum(q*pop)/sum(pop),
          total_amenity_value = sum(om*pop)/sum(pop)
          )%>% 
  arrange(desc(housingcosts)) %>%
  gt()

#地域ごとの表
df4 <- df4 %>%
  mutate(division = as.numeric(division)) %>%
  mutate(division = case_when(
    division == 9 ~ "Pacific",
    division == 1 ~ "New England",
    division == 2 ~ "Middle Atlantic",
    division == 8 ~ "Mountain",
    division == 5 ~ "South Atlantic",
    division == 3 ~ "East North Central",
    division == 4 ~ "West North Central",
    division == 6 ~ "East South Central",
    division == 7 ~ "West South Central"
  ))

df4 %>% 
  mutate(w  = round(as.numeric(w),digits = 2)) %>%
  mutate(p  = round(as.numeric(p),digits = 2)) %>%
  group_by(division)%>% 
  mutate(r  = 4.29*p-2.64*w) %>% 
  mutate(ax = 0.11*p+0.76*w) %>% 
  mutate(q  = 0.32*p-0.48*w) %>% 
  mutate(om = 0.39*p+0.01*w) %>% 
  mutate(r = round(as.numeric(r),digits = 2)) %>%
  mutate(ax  = round(as.numeric(ax),digits = 2)) %>%
  mutate(q  = round(as.numeric(q),digits = 2)) %>%
  mutate(om  = round(as.numeric(om),digits = 2)) %>%
  summarize(population        = sum(pop),
            housingcosts        = round(sum(p*pop)/sum(pop),digits = 2),
            wage                = round(sum(w*pop)/sum(pop),digits = 2),
            land_rent           = round(sum(r*pop)/sum(pop),digits = 2),
            trade_product       = round(sum(ax*pop)/sum(pop),digits = 2),
            quality_of_life     = round(sum(q*pop)/sum(pop),digits = 2),
            total_amenity_value = round(sum(om*pop)/sum(pop),digits = 2)
  )%>% 
  arrange(desc(housingcosts)) %>%
  gt()
