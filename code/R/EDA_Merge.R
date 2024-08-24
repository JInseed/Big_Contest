rm(list=ls())
library(tidyverse)
library(lubridate)

#데이터 불러오기
user=read.csv('C:/Users/82102/OneDrive - dongguk.edu/바탕 화면/2022 빅콘/data/user_spec.csv', header=T, fileEncoding = 'utf-8')

con=read.csv('C:/Users/82102/OneDrive - dongguk.edu/바탕 화면/2022 빅콘/data/loan_result.csv', header=T, fileEncoding = 'utf-8')

app=read.csv('target제외.csv', header=T, fileEncoding = 'utf-8')

df=read.csv('user_con_병합_target제외.csv', header=T)


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

#user 신청서 기준으로 병합
df=user %>% 
  left_join(app, by='application_id')
  
  
write.csv(df, file = "user_con_병합_target제외.csv", row.names = F)





#1-1) user_id 당 신청서 개수 분포
df %>% 
  group_by(user_id) %>% 
  mutate(신청수 = n_distinct(application_id)) %>% 
  ungroup() %>% 
  select(신청수) %>% 
  arrange(신청수) %>% 
  table(useNA = 'ifany') %>% 
  view()

df %>% 
  filter(!is.na(is_applied)) %>% 
  mutate(is_applied=as.factor(is_applied)) %>% 
  group_by(user_id) %>% 
  summarise(신청수=n_distinct(application_id)) %>%
  group_by(신청수) %>% 
  summarise(count=n()) %>% 
  arrange(신청수) %>% 
  head()


#user 별 신청수
df %>% 
  filter(!is.na(is_applied)) %>% 
  mutate(is_applied=as.factor(is_applied)) %>%
  group_by(user_id) %>% 
  mutate(신청수 = n_distinct(application_id)) %>%
  filter(신청수 <= 50) %>% 
  ggplot(aes(fill=is_applied))+
  geom_histogram(aes(신청수),
                 col='white',
                 position='fill',
                 binwidth=1)
  
df %>% 
  filter(!is.na(is_applied)) %>% 
  mutate(is_applied=as.factor(is_applied)) %>%
  group_by(user_id) %>% 
  mutate(신청수 = n_distinct(application_id)) %>%
  filter(신청수 > 50) %>% 
  ggplot()+
    geom_histogram(aes(신청수),
                   col='white',
                   bins=20)
  
df %>% 
  filter(!is.na(is_applied)) %>% 
  mutate(is_applied=as.factor(is_applied)) %>% 
  group_by(is_applied,user_id) %>% 
  summarise(신청수=n_distinct(application_id)) %>%
  group_by(신청수,is_applied) %>% 
  summarise(n=n()) %>% 
  mutate(p=n/sum(n)) %>%
  filter(신청수<50) %>% 
  ggplot(aes(fill=p))+
  geom_histogram(aes(신청수),
                 col='white',
                 position='fill',
                 binwidth=1)



  

#####################################################################

#1-2) age변수 생성 후 나이에 따른 대출 여부 비율
df %>% 
  filter(!is.na(is_applied)) %>% 
  mutate(is_applied=as.factor(is_applied)) %>% 
  mutate(age=2023-birth_year) %>% 
  ggplot(aes(fill=is_applied))+
  geom_histogram(aes(age),
           col='white',
           position='fill',
           binwidth=1)

  
df %>%
  filter(!is.na(is_applied)) %>% 
  mutate(is_applied=as.factor(is_applied)) %>% 
  mutate(age=2023-birth_year) %>% 
  ggplot(aes(fill=is_applied))+
  geom_histogram(aes(age),
           col='white',
           position='stack',
           binwidth=1)


#1-3) gender 따른 대출 여부 비율
df %>% 
  filter(!is.na(is_applied)) %>% 
  group_by(gender,is_applied) %>% 
  summarise(n=n()) %>% 
  mutate(비율=n/sum(n)) %>% 
  select(gender, is_applied ,비율)


df %>% 
  filter(!is.na(is_applied)) %>% 
  mutate(is_applied=as.factor(is_applied)) %>% 
  ggplot(aes(fill=is_applied))+
  geom_bar(aes(gender),
           col='white',
           position='fill')

df %>% 
  filter(!is.na(is_applied)) %>% 
  mutate(is_applied=as.factor(is_applied)) %>% 
  ggplot(aes(fill=is_applied))+
  geom_bar(aes(gender),
           col='white')


#1-4) insert time 따른 대출 여부 비율

#일 별
df %>% 
  select(insert_time, is_applied) %>% 
  filter(!is.na(is_applied)) %>% 
  mutate(is_applied=as.factor(is_applied)) %>%
  mutate(insert_time=mday(insert_time)) %>% 
  ggplot(aes(fill=is_applied))+
  geom_histogram(aes(insert_time),
                 col='white',
                 binwidth = 1)+
  scale_y_continuous(labels = scales::comma)


df %>% 
  select(insert_time, is_applied) %>% 
  filter(!is.na(is_applied)) %>% 
  mutate(is_applied=as.factor(is_applied)) %>%
  mutate(insert_time=mday(insert_time)) %>% 
  ggplot(aes(fill=is_applied))+
  geom_histogram(aes(insert_time),
                 col='white',
                 position='fill',
                 binwidth = 1)

#시간별
df %>% 
  select(insert_time, is_applied) %>% 
  filter(!is.na(is_applied)) %>% 
  mutate(is_applied=as.factor(is_applied)) %>%
  mutate(insert_time=hour(insert_time)) %>% 
  ggplot(aes(fill=is_applied))+
  geom_histogram(aes(insert_time),
                 col='white',
                 binwidth = 1)+
  scale_y_continuous(labels = scales::comma)


df %>% 
  select(insert_time, is_applied) %>% 
  filter(!is.na(is_applied)) %>% 
  mutate(is_applied=as.factor(is_applied)) %>%
  mutate(insert_time=hour(insert_time)) %>% 
  ggplot(aes(fill=is_applied))+
  geom_histogram(aes(insert_time),
                 col='white',
                 position='fill',
                 binwidth = 1)


#1-5) 신용점수 따른 대출 신청 여부 비율

#신용점수 470점 초과

df %>% 
  filter(credit_score > 470) %>% 
  select(credit_score, is_applied) %>% 
  filter(!is.na(is_applied)) %>% 
  mutate(is_applied=as.factor(is_applied)) %>%
  ggplot(aes(fill=is_applied))+
  geom_histogram(aes(credit_score),
                 col='white',
                 binwidth = 10)+
  scale_y_continuous(labels = scales::comma)

df %>% 
  filter(credit_score > 470) %>% 
  select(credit_score, is_applied) %>% 
  filter(!is.na(is_applied)) %>% 
  mutate(is_applied=as.factor(is_applied)) %>%
  ggplot(aes(fill=is_applied))+
  geom_histogram(aes(credit_score),
                 col='white',
                 position = 'fill',
                 binwidth = 10)


#신용점수 470점 이하

df %>% 
  filter(credit_score <= 470) %>% 
  select(credit_score, is_applied) %>% 
  filter(!is.na(is_applied)) %>% 
  mutate(is_applied=as.factor(is_applied)) %>%
  ggplot(aes(fill=is_applied))+
  geom_histogram(aes(credit_score),
                 col='white',
                 binwidth = 10)+
  scale_y_continuous(labels = scales::comma)

df %>% 
  filter(credit_score <= 470) %>% 
  select(credit_score, is_applied) %>% 
  filter(!is.na(is_applied)) %>% 
  mutate(is_applied=as.factor(is_applied)) %>%
  ggplot(aes(fill=is_applied))+
  geom_histogram(aes(credit_score),
                 col='white',
                 position='fill',
                 binwidth = 10)



#1-6) 연 수입 따른 대출 신청 여부 비율

# 연 수입 1억 이하
df %>% 
  select(yearly_income, is_applied) %>% 
  filter(!is.na(is_applied)) %>% 
  filter(yearly_income <= 100000000) %>% 
  mutate(is_applied=as.factor(is_applied)) %>%
  ggplot(aes(fill=is_applied))+
  geom_histogram(aes(yearly_income),
                 col='white',
                 position = 'fill',
                 bins=20)+
  scale_y_continuous(labels = scales::comma)+
  scale_x_continuous(labels = scales::comma)


  
# 연 수입 1억 초과~3억 미만  
df %>% 
  select(yearly_income, is_applied) %>% 
  filter(!is.na(is_applied)) %>% 
  filter(yearly_income > 100000000 & yearly_income < 300000000) %>% 
  mutate(is_applied=as.factor(is_applied)) %>%
  ggplot(aes(fill=is_applied))+
  geom_histogram(aes(yearly_income),
                 col='white',
                 bins=20)+
  scale_y_continuous(labels = scales::comma)+
  scale_x_continuous(labels = scales::comma)

df %>% 
  select(yearly_income, is_applied) %>% 
  filter(!is.na(is_applied)) %>% 
  filter(yearly_income > 100000000 & yearly_income < 300000000) %>% 
  mutate(is_applied=as.factor(is_applied)) %>%
  ggplot(aes(fill=is_applied))+
  geom_histogram(aes(yearly_income),
                 col='white',
                 position = 'fill',
                 bins=20)+
  scale_y_continuous(labels = scales::comma)+
  scale_x_continuous(labels = scales::comma)


# 연수입 3억 이상  
df %>% 
  select(yearly_income, is_applied) %>% 
  filter(!is.na(is_applied)) %>% 
  filter(yearly_income > 300000000) %>% 
  mutate(is_applied=as.factor(is_applied)) %>%
  ggplot(aes(fill=is_applied))+
  geom_histogram(aes(yearly_income),
                 col='white',
                 position='fill',
                 bins=20)+
  scale_y_continuous(labels = scales::comma)+
  scale_x_continuous(labels = scales::comma)



#1-7) 근로 형태 따른 대출 신청 비율 확인

df %>%  
  select(income_type, is_applied) %>% 
  filter(!is.na(is_applied)) %>%
  mutate(is_applied=as.factor(is_applied)) %>%
  ggplot(aes(fill=is_applied))+
  geom_bar(aes(income_type),
           col='white')+
  theme(axis.text.x=element_text(angle=45, hjust=1))+
  scale_y_continuous(labels = scales::comma)
  
df %>%  
  select(income_type, is_applied) %>% 
  filter(!is.na(is_applied)) %>%
  mutate(is_applied=as.factor(is_applied)) %>%
  ggplot(aes(fill=is_applied))+
  geom_bar(aes(income_type),
           col='white',
           position = 'fill')+
  theme(axis.text.x=element_text(angle=45, hjust=1))


df %>%  
  select(income_type, is_applied) %>% 
  filter(!is.na(is_applied)) %>%
  mutate(is_applied=as.factor(is_applied)) %>%
  group_by(income_type,is_applied) %>% 
  summarise(n=n()) %>% 
  mutate(p=n/sum(n)) %>%
  arrange(income_type,-p) %>% 
  ggplot()+
  geom_bar(aes(income_type, p,fill=fct_reorder(is_applied,-p)),
           col='white',
           position = 'fill',
           stat='identity')+
  theme(axis.text.x=element_text(angle=45, hjust=1))


df %>%  
  select(income_type, is_applied) %>% 
  filter(!is.na(is_applied)) %>%
  mutate(is_applied=as.factor(is_applied),
         income_type=as.factor(income_type)) %>%
  group_by(income_type,is_applied) %>% 
  summarise(n=n()) %>% 
  mutate(p=n/sum(n)) %>%
  ggplot(fill=is_applied)+
  geom_bar(aes(income_type), p,
           col='white',
           position = 'fill',
           stat='identity')+
  theme(axis.text.x=element_text(angle=45, hjust=1))


#1-8) company_enter_month

#년 별
df %>% 
  filter(!is.na(company_enter_month)) %>% 
  mutate(company_enter_month=ymd(ifelse(company_enter_month > 2000000, 
                                        as.character(ymd(company_enter_month)), 
                                        as.character(ym(company_enter_month))))) %>%
  mutate(company_enter_month=year(company_enter_month)) %>% 
  ggplot(fill=is_applied)+
  geom_histogram(aes(company_enter_month),
                 col='white',
                 binwidth = 1)+
  scale_y_continuous(labels = scales::comma)

#월 별
df %>% 
  filter(!is.na(company_enter_month)) %>% 
  mutate(company_enter_month=ymd(ifelse(company_enter_month > 2000000, 
                                        as.character(ymd(company_enter_month)), 
                                        as.character(ym(company_enter_month))))) %>%
  mutate(company_enter_month=month(company_enter_month)) %>% 
  ggplot(fill=is_applied)+
  geom_histogram(aes(company_enter_month),
                 col='white',
                 binwidth = 1)+
  scale_y_continuous(labels = scales::comma)

#1-9) 고용 형태 따른 대출 신청 비율 확인

df %>%  
  select(employment_type, is_applied) %>% 
  filter(!is.na(is_applied)) %>%
  mutate(is_applied=as.factor(is_applied)) %>%
  ggplot(aes(fill=is_applied))+
  geom_bar(aes(employment_type),
           col='white')+
  theme(axis.text.x=element_text(angle=45, hjust=1))+
  scale_y_continuous(labels = scales::comma)

df %>%  
  select(employment_type, is_applied) %>% 
  filter(!is.na(is_applied)) %>%
  mutate(is_applied=as.factor(is_applied)) %>%
  ggplot(aes(fill=is_applied))+
  geom_bar(aes(employment_type),
           col='white',
           position = 'fill')+
  theme(axis.text.x=element_text(angle=45, hjust=1))
  

#1-10) 주거소유형태 따른 대출 신청 여부 확인

df %>%  
  select(houseown_type, is_applied) %>% 
  filter(!is.na(is_applied)) %>%
  mutate(is_applied=as.factor(is_applied)) %>%
  ggplot(aes(fill=is_applied))+
  geom_bar(aes(houseown_type),
           col='white')+
  theme(axis.text.x=element_text(angle=45, hjust=1))+
  scale_y_continuous(labels = scales::comma)

df %>%  
  select(houseown_type, is_applied) %>% 
  filter(!is.na(is_applied)) %>%
  mutate(is_applied=as.factor(is_applied)) %>%
  ggplot(aes(fill=is_applied))+
  geom_bar(aes(houseown_type),
           col='white',
           position = 'fill')+
  theme(axis.text.x=element_text(angle=45, hjust=1))


#1-11) 대출 희망 금액 따른 대출 신청 여부 확인

# 대출 희망 금액 1억 이하
df %>% 
  select(desired_amount, is_applied) %>% 
  filter(!is.na(is_applied)) %>% 
  filter(desired_amount <= 100000000) %>% 
  mutate(is_applied=as.factor(is_applied)) %>%
  ggplot(aes(fill=is_applied))+
  geom_histogram(aes(desired_amount),
                 col='white',
                 position = 'fill',
                 bins=20)+
  scale_y_continuous(labels = scales::comma)+
  scale_x_continuous(labels = scales::comma)
  
df %>% 
  select(desired_amount, is_applied) %>% 
  filter(!is.na(is_applied)) %>% 
  filter(desired_amount <= 100000000) %>% 
  mutate(is_applied=as.factor(is_applied)) %>%
  ggplot(aes(fill=is_applied))+
  geom_histogram(aes(desired_amount),
                 col='white',
                 bins=20)+
  scale_y_continuous(labels = scales::comma)+
  scale_x_continuous(labels = scales::comma)


# 대출 희망 금액 1억 초과 10억 미만
df %>% 
  select(desired_amount, is_applied) %>% 
  filter(!is.na(is_applied)) %>% 
  filter(desired_amount > 100000000 & desired_amount < 1000000000) %>% 
  mutate(is_applied=as.factor(is_applied)) %>%
  ggplot(aes(fill=is_applied))+
  geom_histogram(aes(desired_amount),
                 col='white',
                 position = 'fill',
                 bins=20)+
  scale_y_continuous(labels = scales::comma)+
  scale_x_continuous(labels = scales::comma)

df %>% 
  select(desired_amount, is_applied) %>% 
  filter(!is.na(is_applied)) %>% 
  filter(desired_amount > 100000000 & desired_amount < 1000000000) %>% 
  mutate(is_applied=as.factor(is_applied)) %>%
  ggplot(aes(fill=is_applied))+
  geom_histogram(aes(desired_amount),
                 col='white',
                 bins=20)+
  scale_y_continuous(labels = scales::comma)+
  scale_x_continuous(labels = scales::comma)

# 대출 희망 금액 10억 이상
df %>% 
  select(desired_amount, is_applied) %>% 
  filter(!is.na(is_applied)) %>% 
  filter(desired_amount >= 1000000000) %>% 
  mutate(is_applied=as.factor(is_applied)) %>%
  ggplot(aes(fill=is_applied))+
  geom_histogram(aes(desired_amount),
                 col='white',
                 position = 'fill',
                 bins=20)+
  scale_y_continuous(labels = scales::comma)+
  scale_x_continuous(labels = scales::comma)

df %>% 
  select(desired_amount, is_applied) %>% 
  filter(!is.na(is_applied)) %>% 
  filter(desired_amount >= 1000000000) %>% 
  mutate(is_applied=as.factor(is_applied)) %>%
  ggplot(aes(fill=is_applied))+
  geom_histogram(aes(desired_amount),
                 col='white',
                 bins=20)+
  scale_y_continuous(labels = scales::comma)+
  scale_x_continuous(labels = scales::comma)


#1-12) 대출 목적 따른 대출 신청 여부 확인

df %>%  
  select(purpose, is_applied) %>% 
  filter(!is.na(is_applied)) %>%
  mutate(is_applied=as.factor(is_applied)) %>%
  ggplot(aes(fill=is_applied))+
  geom_bar(aes(purpose),
           col='white')+
  theme(axis.text.x=element_text(angle=45, hjust=1))+
  scale_y_continuous(labels = scales::comma)

df %>%  
  select(purpose, is_applied) %>% 
  filter(!is.na(is_applied)) %>%
  mutate(is_applied=as.factor(is_applied)) %>%
  ggplot(aes(fill=is_applied))+
  geom_bar(aes(purpose),
           col='white',
           position = 'fill')+
  theme(axis.text.x=element_text(angle=45, hjust=1))



#1-13) 개인회생자 여부 따른 대출 신청 여부 확인

df %>%  
  select(personal_rehabilitation_yn, is_applied) %>% 
  filter(!is.na(is_applied)) %>%
  mutate(is_applied=as.factor(is_applied)) %>%
  ggplot(aes(fill=is_applied))+
  geom_bar(aes(personal_rehabilitation_yn),
           col='white')+
  theme(axis.text.x=element_text(angle=45, hjust=1))+
  scale_y_continuous(labels = scales::comma)

df %>%  
  select(personal_rehabilitation_yn, is_applied) %>% 
  filter(!is.na(is_applied)) %>%
  mutate(is_applied=as.factor(is_applied)) %>%
  ggplot(aes(fill=is_applied))+
  geom_bar(aes(personal_rehabilitation_yn),
           col='white',
           position = 'fill')+
  theme(axis.text.x=element_text(angle=45, hjust=1))


#1-14) 개인회생자 납입 완료 여부 따른 대출 신청 여부 확인

df %>%  
  select(personal_rehabilitation_complete_yn, is_applied) %>% 
  filter(!is.na(is_applied)) %>%
  mutate(is_applied=as.factor(is_applied)) %>%
  ggplot(aes(fill=is_applied))+
  geom_bar(aes(personal_rehabilitation_complete_yn),
           col='white')+
  theme(axis.text.x=element_text(angle=45, hjust=1))+
  scale_y_continuous(labels = scales::comma)

df %>%  
  select(personal_rehabilitation_complete_yn, is_applied) %>% 
  filter(!is.na(is_applied)) %>%
  mutate(is_applied=as.factor(is_applied)) %>%
  ggplot(aes(fill=is_applied))+
  geom_bar(aes(personal_rehabilitation_complete_yn),
           col='white',
           position = 'fill')+
  theme(axis.text.x=element_text(angle=45, hjust=1))



#1-15) 기대출수 따른 대출 신청 여부 확인

#기대출 수 15 미만
df %>% 
  filter(existing_loan_cnt < 15) %>% 
  select(existing_loan_cnt, is_applied) %>% 
  filter(!is.na(is_applied)) %>% 
  mutate(is_applied=as.factor(is_applied)) %>%
  ggplot(aes(fill=is_applied))+
  geom_histogram(aes(existing_loan_cnt),
                 col='white',
                 binwidth = 1)+
  scale_y_continuous(labels = scales::comma)

df %>% 
  filter(existing_loan_cnt < 15) %>% 
  select(existing_loan_cnt, is_applied) %>% 
  filter(!is.na(is_applied)) %>% 
  mutate(is_applied=as.factor(is_applied)) %>%
  ggplot(aes(fill=is_applied))+
  geom_histogram(aes(existing_loan_cnt),
                 col='white',
                 position = 'fill',
                 binwidth = 1)+
  scale_y_continuous(labels = scales::comma)


#기대출 수 15 이상
df %>% 
  filter(existing_loan_cnt >= 15) %>% 
  select(existing_loan_cnt, is_applied) %>% 
  filter(!is.na(is_applied)) %>% 
  mutate(is_applied=as.factor(is_applied)) %>%
  ggplot(aes(fill=is_applied))+
  geom_histogram(aes(existing_loan_cnt),
                 col='white',
                 binwidth = 1)+
  scale_y_continuous(labels = scales::comma)

df %>% 
  filter(existing_loan_cnt >= 15) %>% 
  select(existing_loan_cnt, is_applied) %>% 
  filter(!is.na(is_applied)) %>% 
  mutate(is_applied=as.factor(is_applied)) %>%
  ggplot(aes(fill=is_applied))+
  geom_histogram(aes(existing_loan_cnt),
                 col='white',
                 position = 'fill',
                 binwidth = 1)+
  scale_y_continuous(labels = scales::comma)


#1-16) 기대출 금액 따른 대출 신청 여부 확인







df %>% 
  select(loan_rate, is_applied) %>% 
  filter(!is.na(is_applied)) %>% 
  mutate(is_applied=as.factor(is_applied)) %>%
  ggplot(aes(fill=is_applied))+
  geom_histogram(aes(loan_rate),
                 col='white',
                 position = 'fill',
                 binwidth = 0.5)

df %>% 
  select(loan_rate, is_applied) %>% 
  filter(!is.na(is_applied)) %>% 
  mutate(is_applied=as.factor(is_applied)) %>%
  ggplot(aes(fill=is_applied))+
  geom_histogram(aes(loan_rate),
                 col='white',
                 binwidth = 0.5)



df %>% 
  select(loan_limit, is_applied) %>% 
  filter(loan_limit < 100000000) %>% 
  filter(!is.na(is_applied)) %>% 
  mutate(is_applied=as.factor(is_applied)) %>%
  ggplot(aes(fill=is_applied))+
  geom_histogram(aes(loan_limit),
                 col='white',
                 position = 'fill',
                 bins = 20)+
  scale_y_continuous(labels = scales::comma)+
  scale_x_continuous(labels = scales::comma)

df %>% 
  select(loan_limit, is_applied) %>% 
  filter(loan_limit < 100000000) %>% 
  filter(!is.na(is_applied)) %>% 
  mutate(is_applied=as.factor(is_applied)) %>%
  ggplot(aes(fill=is_applied))+
  geom_histogram(aes(loan_limit),
                 col='white',
                 bins =20)+
  scale_y_continuous(labels = scales::comma)+
  scale_x_continuous(labels = scales::comma)

df %>% 
  select(loan_limit, is_applied) %>% 
  filter(loan_limit < 5000000) %>% 
  filter(!is.na(is_applied)) %>% 
  mutate(is_applied=as.factor(is_applied)) %>%
  ggplot(aes(fill=is_applied))+
  geom_histogram(aes(loan_limit),
                 col='white',
                 bins =20)+
  scale_y_continuous(labels = scales::comma)+
  scale_x_continuous(labels = scales::comma)






]df %>% 
  select(loan_limit, is_applied, purpose) %>% 
  filter(loan_limit < 100000000) %>% 
  filter(!is.na(is_applied)) %>% 
  filter(is_applied==1) %>% 
  mutate(is_applied=as.factor(is_applied)) %>%
  ggplot(aes(fill=purpose))+
  geom_histogram(aes(loan_limit),
                 col='white',
                 bins =20)+
  scale_y_continuous(labels = scales::comma)+
  scale_x_continuous(labels = scales::comma)













