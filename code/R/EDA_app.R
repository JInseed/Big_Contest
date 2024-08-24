rm(list=ls())

library(tidyverse)
library(lubridate)

#데이터 불러오기
app=read.csv('target제외.csv', header=T)


#1-1) application_id
length(unique(app[,'application_id']))
sum(is.na(app[,'application_id']))

#1-2) loanapply_insert_time
length(unique(app[,'loanapply_insert_time']))
sum(is.na(app[,'loanapply_insert_time']))

app %>% 
  select(loanapply_insert_time) %>% 
  mutate(loanapply_insert_time=ymd_hms(loanapply_insert_time)) %>%
  arrange(loanapply_insert_time) %>% 
  head()

app %>% 
  select(loanapply_insert_time) %>% 
  mutate(loanapply_insert_time=ymd_hms(loanapply_insert_time)) %>%
  arrange(loanapply_insert_time) %>% 
  tail()

#한도 조회 일시 분포 확인

#일
app %>% 
  select(loanapply_insert_time) %>% 
  mutate(loanapply_insert_time=mday(loanapply_insert_time)) %>% 
  mutate(loanapply_insert_time=as.factor(loanapply_insert_time)) %>% 
  ggplot()+
  geom_bar(aes(loanapply_insert_time), 
           col='white', 
           fill='skyblue')+
  scale_y_continuous(labels = scales::comma)

#시간
app %>% 
  select(loanapply_insert_time) %>% 
  mutate(loanapply_insert_time=hour(loanapply_insert_time)) %>% 
  mutate(loanapply_insert_time=as.factor(loanapply_insert_time)) %>% 
  ggplot()+
  geom_bar(aes(loanapply_insert_time), 
           col='white', 
           fill='skyblue')+
  scale_y_continuous(labels = scales::comma)



#1-3) bank_id

length(unique(app[,'bank_id']))
sum(is.na(app[,'bank_id']))
sort(unique(app[,'bank_id']))

con %>% 
  filter(bank_id==48) %>% 
  filter(is.na(is_applied)) %>% 
  select(application_id) %>% 
  unique() %>% 
  dim()


# 금융사 번호 분포 확인
app %>% 
  mutate(bank_id=as.factor(bank_id)) %>% 
  select(bank_id) %>% 
  ggplot()+
  geom_bar(aes(fct_reorder(bank_id)),
           fill='skyblue',
           col='white')+
  scale_y_continuous(labels = scales::comma)+
  theme(axis.text.x=element_text(angle=90, hjust=1))

#많이 사용하는 순서
app %>% 
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

length(unique(app[,'product_id']))
sum(is.na(app[,'product_id']))
sort(unique(app[,'product_id']))

# 상품 번호 분포 확인

app %>% 
  mutate(product_id=as.factor(product_id)) %>% 
  select(product_id) %>% 
  ggplot()+
  geom_bar(aes(product_id),
           fill='skyblue',
           col='white')+
  scale_y_continuous(labels = scales::comma)+
  theme(axis.text.x=element_text(angle=90, hjust=1))


#많이 사용하는 순서
app %>% 
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

#test 데이터에는 없고 target에만 있는 product_id
con %>% 
  mutate(product_id=as.factor(product_id)) %>% 
  select(product_id) %>%
  filter(!(product_id %in% unique(app[,'product_id']))) %>% 
  group_by(product_id) %>% 
  summarise(count=n()) %>% 
  ggplot()+
  geom_bar(aes(fct_reorder(product_id, -count), count),
           fill='skyblue',
           col='white',
           stat="identity" )+
  scale_y_continuous(labels = scales::comma)+
  theme(axis.text.x=element_text(angle=90, hjust=1))


con %>% 
  filter(!(product_id %in% unique(app[,'product_id']))) %>% 
  filter(is.na(is_applied)) %>% 
  select(application_id) %>% 
  unique() %>% 
  dim()


con %>% 
  filter(!(product_id %in% unique(app[,'product_id']))) %>% 
  filter(bank_id==48) %>% 
  filter(is.na(is_applied)) %>% 
  select(application_id) %>% 
  unique() %>% 
  dim()

# 1-5) loan_limit

length(unique(app[,'loan_limit']))
sum(is.na(app[,'loan_limit']))

min(unique(app[,'loan_limit']), na.rm=TRUE)
max(unique(app[,'loan_limit']), na.rm=TRUE)

#승인한도 1억 초과인 사람 수, 전체 8590명
app %>% 
  select(loan_limit) %>% 
  filter(loan_limit > 100000000) %>% 
  group_by(loan_limit) %>% 
  summarise(n=n()) %>%
  arrange(-n) 

app %>% 
  select(loan_limit) %>% 
  filter(loan_limit > 100000000) %>% 
  group_by(loan_limit) %>% 
  summarise(n=n()) %>% 
  select(n) %>% 
  colSums()

#승인한도 1억 이하인 사람 수, 전체 10255796 명
app %>% 
  select(loan_limit) %>% 
  filter(loan_limit <= 100000000) %>% 
  group_by(loan_limit) %>% 
  summarise(n=n()) %>%
  arrange(loan_limit) %>% 
  view()

app %>% 
  select(loan_limit) %>% 
  filter(loan_limit <= 100000000) %>% 
  group_by(loan_limit) %>% 
  summarise(n=n()) %>% 
  select(n) %>% 
  colSums()

#승인한도 1억 이하인 사람 분포 그래프
app %>% 
  filter(loan_limit <= 100000000) %>% 
  ggplot()+
  geom_histogram(aes(loan_limit), 
                 col='white', 
                 fill='skyblue',
                 bins=20)+
  scale_y_continuous(labels = scales::comma)+
  scale_x_continuous(labels = scales::comma)

#승인한도 1억 초과인 사람 분포 그래프
app %>% 
  filter(loan_limit > 100000000) %>% 
  ggplot()+
  geom_histogram(aes(loan_limit), 
                 col='white', 
                 fill='skyblue',
                 bins=20)+
  scale_y_continuous(labels = scales::comma)+
  scale_x_continuous(labels = scales::comma)


# 1-6) loan_rate

length(unique(app[,'loan_rate']))
sum(is.na(app[,'loan_rate']))

min(unique(app[,'loan_rate']), na.rm=TRUE)
max(unique(app[,'loan_rate']), na.rm=TRUE)


# 승인금리 분포 그래프

app %>% 
  select(loan_rate) %>% 
  ggplot()+
  geom_histogram(aes(loan_rate),
                 fill='skyblue',
                 col='white',
                 binwidth = 0.5)+
  scale_y_continuous(labels = scales::comma)


app %>% 
  select(is_applied) %>% 
  table(useNA = 'ifany')


eda %>% 
  mutate(is_applied=as.factor(is_applied)) %>% 
  select(is_applied,loan_rate) %>% 
  ggplot(aes(fill=is_applied))+
  geom_histogram(aes(loan_rate),
                 col='white',
                 position='fill',
                 binwidth = 0.5)+
  scale_y_continuous(labels = scales::comma)





df %>% 
  filter(!is.na(is_applied)) %>% 
  select(user_id, application_id, product_id, insert_time, loanapply_insert_time, 
         loan_rate, loan_limit, is_applied) %>% 
  group_by(user_id, application_id, product_id) %>% 
  filter(n()>1) %>% 
  view()
  



app %>% 
  arrange(application_id) %>% 
  head(100) %>% 
  view()







a=c(1,1,2,2,2,2,3,3,3)
b=c(10,20,30,40,40,50,5,5,5)

tt=data.frame(a,b)

tt %>% 
  group_by(a) %>% 
  mutate(test=length(unique(b)))












