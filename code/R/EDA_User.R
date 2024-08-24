rm(list=ls())

library(tidyverse)
library(lubridate)

#데이터 불러오기
user=read.csv('C:/Users/82102/OneDrive - dongguk.edu/바탕 화면/2022 빅콘/data/user_spec.csv', header=T, fileEncoding = 'utf-8')

user2=read.csv('C:/Users/82102/OneDrive - dongguk.edu/바탕 화면/2022 빅콘/data/user_spec.csv', header=T, fileEncoding = 'utf-8')

con=read.csv('C:/Users/82102/OneDrive - dongguk.edu/바탕 화면/2022 빅콘/data/loan_result.csv', header=T, fileEncoding = 'utf-8')

log=read.csv('C:/Users/82102/OneDrive - dongguk.edu/바탕 화면/2022 빅콘/data/log_data.csv', header=T, fileEncoding = 'utf-8')

str(user)
str(user2)

view(head(user))
view(head(con))
view(head(log))

colSums(is.na(user))
colSums(is.na(con))
colSums(is.na(log))

colnames(user)
#EDA of user
#[1] "application_id"                     
#[2] "user_id"                            
#[3] "birth_year"                         
#[4] "gender"                             
#[5] "insert_time"                        
#[6] "credit_score"                       
#[7] "yearly_income"                      
#[8] "income_type"                        
#[9] "company_enter_month"                
#[10] "employment_type"                    
#[11] "houseown_type"                      
#[12] "desired_amount"                     
#[13] "purpose"                            
#[14] "personal_rehabilitation_yn"         
#[15] "personal_rehabilitation_complete_yn"
#[16] "existing_loan_cnt"                  
#[17] "existing_loan_amt"

#1. 변수 별 분포 및 table
#1-1) application_id
length(unique(user[,'application_id']))
sum(is.na(user[,'application_id']))

#1-2) user_id
length(unique(user[,'user_id']))
sum(is.na(user[,'user_id']))

#1-3) birth_year
length(unique(user[,'birth_year']))
sum(is.na(user[,'birth_year']))

sort(unique(user[,'birth_year']))

# histogram, 나이별 분포 확인, 나이로 볼때는 2023-birth_year 하면 됨
ggplot(data=user)+
  geom_histogram(aes(2023-birth_year), 
                 col='white', 
                 fill='skyblue',
                 bins=82)

#1-4) gender
length(unique(user[,'gender']))
sum(is.na(user[,'gender']))
table(user[,'gender'])

#1-5) insert_time
length(unique(user[,'insert_time']))
sum(is.na(user[,'insert_time']))

user %>% 
  ungroup() %>% 
  select(insert_time) %>% 
  arrange(insert_time) %>% 
  head()

user %>% 
  ungroup() %>% 
  select(insert_time) %>% 
  arrange(insert_time) %>% 
  tail(10)


#1-6) credit_score
length(unique(user[,'credit_score']))
sort(unique(user[,'credit_score']))
sum(is.na(user[,'credit_score']))

ggplot(data=user)+
  geom_histogram(aes(credit_score), 
                 col='white', 
                 fill='skyblue',
                 bins=92)

#1-7) yearly_income
length(unique(user[,'yearly_income']))
sum(is.na(user[,'yearly_income']))
sort(unique(user[,'yearly_income']))


user %>% 
  select(yearly_income) %>% 
  filter(yearly_income =< 100000000) %>%
  unique() %>%
  arrange(yearly_income)

user %>% 
  select(yearly_income) %>% 
  filter(yearly_income > 100000000) %>%
  unique() %>%
  arrange(yearly_income)

# 연 수입 1억 초과에서의 각 사람 수 - 전체는 37647명으로 약 10퍼
user %>% 
  select(yearly_income) %>% 
  filter(yearly_income > 100000000) %>% 
  group_by(yearly_income) %>% 
  summarise(n=n()) %>% 
  select(n) %>% 
  colSums()
  

# 연 수입 1억 이하인 사람들의 각 사람 수, 수입이 없는 사람이 17459명
user %>% 
  select(yearly_income) %>% 
  filter(yearly_income <= 100000000) %>% 
  group_by(yearly_income) %>% 
  summarise(n=n()) %>% 
  arrange(yearly_income) 

# 연 수입 1억 이하인 사람들의 분포 그래프
user %>% 
  filter(yearly_income <= 100000000) %>% 
  ggplot()+
  geom_histogram(aes(yearly_income), 
                 col='white', 
                 fill='shkyblue',
                 bins=100)+
    scale_y_continuous(labels = scales::comma)+
    scale_x_continuous(labels = scales::comma)

# 연 수입 1억 이상인 사람들의 분포 그래프
user %>% 
  filter(yearly_income>=100000000) %>% 
  ggplot()+
  geom_histogram(aes(yearly_income), 
                 col='white', 
                 fill='skyblue',
                 bins=10)+
  scale_x_continuous(breaks = seq(1e+8,1.000e+10, 1e+9))
#  +scale_y_continuous(labels = scales::comma)+
#  scale_x_continuous(labels = scales::comma)

#1-8) income_type
length(unique(user[,'income_type']))
sum(is.na(user[,'income_type']))
sort(unique(user[,'income_type']))

user %>% 
  select(income_type) %>% 
  filter(income_type == '') %>% 
  summarise(n=n())

user %>% 
  select(income_type) %>% 
  table()

#1-9) company_enter_month
length(unique(user[,'company_enter_month']))
sum(is.na(user[,'company_enter_month']))
sort(unique(user[,'company_enter_month']))

user %>% 
  ungroup() %>% 
  filter(!is.na(company_enter_month)) %>% 
  select(company_enter_month) %>% 
  arrange(company_enter_month) %>% 
  tail()


user %>% 
  filter(company_enter_month==format(ym('2022-11'),'%Y-%m'))

format(ym('2022-11'),'%Y-%m')
ym('2022.11')

user2 %>% 
  ungroup() %>% 
  filter(!is.na(company_enter_month)) %>% 
  select(company_enter_month) %>% 
  arrange(company_enter_month) %>% 
  tail()


user %>% 
  filter(!is.na(company_enter_month)) %>% 
  select(company_enter_month) %>% 
  mutate(company_enter_month=ymd(ifelse(company_enter_month > 2000000,
                                        as.character(ymd(company_enter_month)),
                                        as.character(ym(company_enter_month))))) %>% 
  arrange(company_enter_month) %>% 
  head()

view(sort(user[,'company_enter_month']))


#2000년 이후로 입사한 사람들의 분포
user %>% 
  filter(!is.na(company_enter_month)) %>% 
  mutate(company_enter_month=ymd(ifelse(company_enter_month > 2000000, 
                                        as.character(ymd(company_enter_month)), 
                                        as.character(ym(company_enter_month))))) %>%
  filter(company_enter_month >= ymd(20000101)) %>% 
  ggplot()+
  geom_histogram(aes(company_enter_month),
                 col='white', 
                 fill='skyblue',
                 bins = 100)


#2000년 이전에 입사한 사람들의 분포
user %>% 
  filter(!is.na(company_enter_month)) %>% 
  mutate(company_enter_month=ymd(ifelse(company_enter_month > 2000000, 
                                        as.character(ymd(company_enter_month)), 
                                        as.character(ym(company_enter_month))))) %>%
  filter(company_enter_month < ymd(20000101)) %>% 
  ggplot()+
  geom_histogram(aes(company_enter_month), 
                 col='white', 
                 fill='skyblue',
                 bins = 100)





#############################################
length(unique(con[,'product_id']))
length(unique(con[,'application_id']))









min(user[,'existing_loan_cnt'], na.rm=TRUE)



