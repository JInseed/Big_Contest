rm(list=ls())

library(tidyverse)
library(lubridate)

#데이터 불러오기
user=read.csv('C:/Users/82102/OneDrive - dongguk.edu/바탕 화면/2022 빅콘/data/user_spec.csv', header=T, fileEncoding = 'utf-8')

con=read.csv('C:/Users/82102/OneDrive - dongguk.edu/바탕 화면/2022 빅콘/data/loan_result.csv', header=T, fileEncoding = 'utf-8')

app=con %>% 
  filter(!is.na(is_applied))

write.csv(app, file = "target제외.csv", row.names = F)

app=read.csv('target제외.csv', header=T)

length(unique(app[,'application_id']))


sum(!(unique(user[,'application_id'])) %in% unique(con[,'application_id']))
sum(!(unique(user[,'application_id'])) %in% unique(app[,'application_id']))

sum(!(con[,'application_id']) %in% user[,'application_id'])
sum(!(app[,'application_id']) %in% user[,'application_id'])


length(unique(user[,'application_id']))
length(unique(con[,'application_id']))



################################################################################
#user 신청서 기준으로 병합
tt=user %>% 
  inner_join(app, by='application_id')

app=con %>% 
  filter(is.na(is_applied)) %>% 
  filter(!is.na(loan_rate))

colSums(is.na(tt))






#1-1) application_id
length(unique(con[,'application_id']))
sum(is.na(con[,'application_id']))

#1-2) loanapply_insert_time
length(unique(con[,'loanapply_insert_time']))
sum(is.na(con[,'loanapply_insert_time']))

con %>% 
  select(loanapply_insert_time) %>% 
  mutate(loanapply_insert_time=ymd_hms(loanapply_insert_time)) %>%
  arrange(loanapply_insert_time) %>% 
  head()

con %>% 
  select(loanapply_insert_time) %>% 
  mutate(loanapply_insert_time=ymd_hms(loanapply_insert_time)) %>%
  arrange(loanapply_insert_time) %>% 
  tail()

#한도 조회 일시 분포 확인

con %>% 
  select(loanapply_insert_time) %>% 
  mutate(loanapply_insert_time=mday(loanapply_insert_time)) %>% 
  mutate(loanapply_insert_time=as.factor(loanapply_insert_time)) %>% 
  ggplot()+
  geom_bar(aes(loanapply_insert_time), 
                 col='white', 
                 fill='skyblue')+
  scale_y_continuous(labels = scales::comma)


con %>% 
  select(loanapply_insert_time) %>% 
  mutate(loanapply_insert_time=hour(loanapply_insert_time)) %>% 
  mutate(loanapply_insert_time=as.factor(loanapply_insert_time)) %>% 
  ggplot()+
  geom_bar(aes(loanapply_insert_time), 
                 col='white', 
                 fill='skyblue')+
  scale_y_continuous(labels = scales::comma)



#1-3) bank_id

length(unique(con[,'bank_id']))
sum(is.na(con[,'bank_id']))
sort(unique(con[,'bank_id']))


# 금융사 번호 분포 확인
con %>% 
  mutate(bank_id=as.factor(bank_id)) %>% 
  select(bank_id) %>% 
  ggplot()+
  geom_bar(aes(bank_id),
           fill='skyblue',
           col='white')+
  scale_y_continuous(labels = scales::comma)+
  theme(axis.text.x=element_text(angle=90, hjust=1))

#많이 사용하는 순서
con %>% 
  mutate(bank_id=as.factor(bank_id)) %>% 
  select(bank_id) %>%
  group_by(bank_id) %>% 
  summarise(count=n()) %>% 
  ggplot()+
  geom_bar(aes(fct_reorder(bank_id, -count), count),
           fill='skyblue',
           col='white',
           stat="identity" )+
  scale_y_continuous(labels = scales::comma)+
  theme(axis.text.x=element_text(angle=90, hjust=1))



#1-4) product_id

length(unique(con[,'product_id']))
sum(is.na(con[,'product_id']))
sort(unique(con[,'product_id']))

# 상품 번호 분포 확인

con %>% 
  mutate(product_id=as.factor(product_id)) %>% 
  select(product_id) %>% 
  ggplot()+
  geom_bar(aes(product_id),
           fill='skyblue',
           col='white')+
  scale_y_continuous(labels = scales::comma)+
  theme(axis.text.x=element_text(angle=90, hjust=1))

#많이 사용하는 순서
con %>% 
  mutate(product_id=as.factor(product_id)) %>% 
  select(product_id) %>%
  group_by(product_id) %>% 
  summarise(count=n()) %>% 
  ggplot()+
  geom_bar(aes(fct_reorder(product_id, -count), count),
           fill='skyblue',
           col='white',
           stat="identity" )+
  scale_y_continuous(labels = scales::comma)+
  theme(axis.text.x=element_text(angle=90, hjust=1))


sum(!(unique(con[,'product_id']) %in% unique(app[,'product_id'])))





# 1-5) loan_limit

length(unique(con[,'loan_limit']))
sum(is.na(con[,'loan_limit']))

min(unique(con[,'loan_limit']), na.rm=TRUE)
max(unique(con[,'loan_limit']), na.rm=TRUE)

#승인한도 1억 초과인 사람 수, 전체 62034명
con %>% 
  select(loan_limit) %>% 
  filter(loan_limit > 100000000) %>% 
  group_by(loan_limit) %>% 
  summarise(n=n()) %>%
  arrange(-n) 

con %>% 
  select(loan_limit) %>% 
  filter(loan_limit > 100000000) %>% 
  group_by(loan_limit) %>% 
  summarise(n=n()) %>% 
  select(n) %>% 
  colSums()

#승인한도 1억 이하인 사람 수, 전체 13457834 명
con %>% 
  select(loan_limit) %>% 
  filter(loan_limit <= 100000000) %>% 
  group_by(loan_limit) %>% 
  summarise(n=n()) %>%
  arrange(loan_limit) %>% 
  view()

con %>% 
  select(loan_limit) %>% 
  filter(loan_limit <= 100000000) %>% 
  group_by(loan_limit) %>% 
  summarise(n=n()) %>% 
  select(n) %>% 
  colSums()

#승인한도 1억 이하인 사람 분포 그래프
con %>% 
  filter(loan_limit <= 100000000) %>% 
  ggplot()+
  geom_histogram(aes(loan_limit), 
                 col='white', 
                 fill='skyblue',
                 bins=20)+
  scale_y_continuous(labels = scales::comma)+
  scale_x_continuous(labels = scales::comma)

#승인한도 1억 초과인 사람 분포 그래프
con %>% 
  filter(loan_limit > 100000000) %>% 
  ggplot()+
  geom_histogram(aes(loan_limit), 
                 col='white', 
                 fill='skyblue',
                 bins=20)+
  scale_y_continuous(labels = scales::comma)+
  scale_x_continuous(labels = scales::comma)


# 1-6) loan_rate

length(unique(con[,'loan_rate']))
sum(is.na(con[,'loan_rate']))

min(unique(con[,'loan_rate']), na.rm=TRUE)
max(unique(con[,'loan_rate']), na.rm=TRUE)


# 승인금리 분포 그래프

con %>% 
  select(loan_rate) %>% 
  ggplot()+
  geom_histogram(aes(loan_rate),
                 fill='skyblue',
                 col='white',
                 binwidth = 0.5)+
  scale_y_continuous(labels = scales::comma)


con %>% 
  select(is_applied) %>% 
  table(useNA = 'ifany')

