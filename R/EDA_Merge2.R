rm(list=ls())

library(tidyverse)
library(lubridate)


#데이터 불러오기
df=read.csv('user_con_병합_target제외.csv', header=T)


#대출 한 user와 대출 하지 않은 user 비율
df %>% 
  filter(!is.na(is_applied)) %>% 
  select(user_id, is_applied) %>% 
  group_by(user_id) %>% 
  mutate(대출여부= ifelse(sum(is_applied) > 0, '대출', '대출안함')) %>%
  ggplot() + 
  geom_bar(aes(대출여부))+
  scale_y_continuous(labels = scales::comma)


df %>% 
  filter(!is.na(is_applied)) %>% 
  select(user_id, is_applied) %>% 
  group_by(user_id) %>% 
  mutate(대출여부= sum(is_applied)) %>%
  filter(대출여부 < 40) %>%
  ggplot() + 
  geom_histogram(aes(대출여부),
                 fill='skyblue',
                 col='white',
                 binwidth=1)+
  scale_y_continuous(labels = scales::comma)



#신청서별_상품수
df %>%
  filter(!is.na(is_applied)) %>% 
  mutate(is_applied=as.factor(is_applied)) %>%
  group_by(user_id, application_id) %>% 
  mutate(신청서별_상품수=length(unique(product_id))) %>%
  ggplot() + 
  geom_histogram(aes(신청서별_상품수),
                 fill='skyblue',
                 col='white' ,
                 binwidth=1
  )+
  scale_y_continuous(labels = scales::comma)

df %>%
  filter(!is.na(is_applied)) %>% 
  mutate(is_applied=as.factor(is_applied)) %>%
  group_by(user_id, application_id) %>% 
  mutate(신청서별_상품수=length(unique(product_id))) %>%
  ggplot(aes(fill=is_applied)) + 
  geom_histogram(aes(신청서별_상품수),
                 col='white',
                 position='fill',
                 binwidth=1)+
  scale_y_continuous(labels = scales::comma)

df %>%
  filter(!is.na(is_applied)) %>% 
  mutate(is_applied=as.factor(is_applied)) %>%
  group_by(user_id, application_id) %>% 
  mutate(신청서별_상품수=length(unique(product_id))) %>%
  filter(신청서별_상품수 <= 20) %>% 
  ggplot(aes(fill=is_applied)) + 
  geom_histogram(aes(신청서별_상품수),
                 col='white',
                 position='stack',
                 binwidth=1)+
  scale_y_continuous(labels = scales::comma)


df %>% 
  filter(!is.na(is_applied)) %>% 
  filter(user_id==836762) %>% 
  select(application_id, is_applied) %>% 
  view()


##############################################################################################
#user 중심의 대출신청(user가 대출을 했느냐 않했느냐, 고로  User_spec을 중심으로 볼 것)

eda=df %>% 
  filter(!is.na(is_applied)) %>%
  group_by(user_id) %>% 
  mutate(대출여부= sum(is_applied)) %>%
  mutate(대출수=ifelse(대출여부==0, '대출안함',
                        ifelse(대출여부<=3,'대출신청(1-3)','대출신청다수')))


eda %>% 
  ggplot() + 
  geom_bar(aes(대출수,fill=대출수),
           col='white')+
  scale_y_continuous(labels = scales::comma)
  


#1-1) age변수 생성 후 나이에 따른 대출 여부 비율
eda %>% 
  mutate(age=2023-birth_year) %>% 
  ggplot(aes(fill=대출수))+
  geom_histogram(aes(age),
                 col='white',
                 position='fill',
                 binwidth=1)


eda %>%
  mutate(age=2023-birth_year) %>% 
  ggplot(aes(fill=대출수))+
  geom_histogram(aes(age),
                 col='white',
                 position='stack',
                 binwidth=1)


#1-2) gender 따른 대출 여부 비율
eda %>% 
  group_by(gender,대출수) %>% 
  summarise(n=n()) %>% 
  mutate(비율=n/sum(n)) %>% 
  select(gender, 대출수 ,비율)


eda %>% 
  mutate(대출수=as.factor(대출수)) %>% 
  ggplot(aes(fill=대출수))+
  geom_bar(aes(gender),
           col='white',
           position='fill')

eda %>% 
  ggplot(aes(fill=대출수))+
  geom_bar(aes(gender),
           col='white')


#1-3) insert time 따른 대출 여부 비율

#일 별
eda %>% 
  select(insert_time, 대출수) %>% 
  mutate(insert_time=mday(insert_time)) %>% 
  ggplot(aes(fill=대출수))+
  geom_histogram(aes(insert_time),
                 col='white',
                 binwidth = 1)+
  scale_y_continuous(labels = scales::comma)


eda %>% 
  select(insert_time, 대출수) %>% 
  mutate(insert_time=mday(insert_time)) %>% 
  ggplot(aes(fill=대출수))+
  geom_histogram(aes(insert_time),
                 col='white',
                 position='fill',
                 binwidth = 1)

#시간별
eda %>% 
  select(insert_time, 대출수) %>% 
  mutate(insert_time=hour(insert_time)) %>% 
  ggplot(aes(fill=대출수))+
  geom_histogram(aes(insert_time),
                 col='white',
                 binwidth = 1)+
  scale_y_continuous(labels = scales::comma)


eda %>% 
  select(insert_time, 대출수) %>% 
  mutate(insert_time=hour(insert_time)) %>% 
  ggplot(aes(fill=대출수))+
  geom_histogram(aes(insert_time),
                 col='white',
                 position='fill',
                 binwidth = 1)


#1-5) 신용점수 따른 대출 신청 여부 비율

#신용점수 470점 초과

eda %>% 
  filter(credit_score > 470) %>% 
  select(credit_score, 대출수) %>% 
  ggplot(aes(fill=대출수))+
  geom_histogram(aes(credit_score),
                 col='white',
                 binwidth = 10)+
  scale_y_continuous(labels = scales::comma)

eda %>% 
  filter(credit_score > 470) %>% 
  select(credit_score, 대출수) %>% 
  ggplot(aes(fill=대출수))+
  geom_histogram(aes(credit_score),
                 col='white',
                 position = 'fill',
                 binwidth = 10)


#신용점수 470점 이하

eda %>% 
  filter(credit_score <= 470) %>% 
  select(credit_score, 대출수) %>% 
  ggplot(aes(fill=대출수))+
  geom_histogram(aes(credit_score),
                 col='white',
                 binwidth = 10)+
  scale_y_continuous(labels = scales::comma)

eda %>% 
  filter(credit_score <= 470) %>% 
  select(credit_score, 대출수) %>% 
  ggplot(aes(fill=대출수))+
  geom_histogram(aes(credit_score),
                 col='white',
                 position='fill',
                 binwidth = 10)



#1-6) 연 수입 따른 대출 신청 여부 비율

# 연 수입 1억 이하
eda %>% 
  select(yearly_income, 대출수) %>% 
  filter(yearly_income <= 100000000) %>% 
  ggplot(aes(fill=대출수))+
  geom_histogram(aes(yearly_income),
                 col='white',
                 position = 'fill',
                 bins=20)+
  scale_y_continuous(labels = scales::comma)+
  scale_x_continuous(labels = scales::comma)

eda %>% 
  select(yearly_income, 대출수) %>% 
  filter(yearly_income <= 100000000) %>% 
  ggplot(aes(fill=대출수))+
  geom_histogram(aes(yearly_income),
                 col='white',
                 position = 'stack',
                 bins=20)+
  scale_y_continuous(labels = scales::comma)+
  scale_x_continuous(labels = scales::comma)



# 연 수입 1억 초과~3억 미만  
eda %>% 
  select(yearly_income, 대출수) %>% 
  filter(yearly_income > 100000000 & yearly_income < 300000000) %>% 
  ggplot(aes(fill=대출수))+
  geom_histogram(aes(yearly_income),
                 col='white',
                 bins=20)+
  scale_y_continuous(labels = scales::comma)+
  scale_x_continuous(labels = scales::comma)

eda %>% 
  select(yearly_income, 대출수) %>% 
  filter(yearly_income > 100000000 & yearly_income < 300000000) %>% 
  ggplot(aes(fill=대출수))+
  geom_histogram(aes(yearly_income),
                 col='white',
                 position = 'fill',
                 bins=20)+
  scale_y_continuous(labels = scales::comma)+
  scale_x_continuous(labels = scales::comma)


# 연수입 3억 이상  
eda %>% 
  select(yearly_income, 대출수) %>% 
  filter(yearly_income > 300000000) %>% 
  ggplot(aes(fill=대출수))+
  geom_histogram(aes(yearly_income),
                 col='white',
                 position='fill',
                 bins=20)+
  scale_y_continuous(labels = scales::comma)+
  scale_x_continuous(labels = scales::comma)



#1-7) 근로 형태 따른 대출 신청 비율 확인

eda %>%  
  select(income_type, 대출수) %>% 
  ggplot(aes(fill=대출수))+
  geom_bar(aes(income_type),
           col='white')+
  theme(axis.text.x=element_text(angle=45, hjust=1))+
  scale_y_continuous(labels = scales::comma)

eda %>%  
  select(income_type, 대출수) %>%
  filter(income_type!='EARNEDINCOME') %>% 
  ggplot(aes(fill=대출수))+
  geom_bar(aes(income_type),
           col='white')+
  theme(axis.text.x=element_text(angle=45, hjust=1))+
  scale_y_continuous(labels = scales::comma)



eda %>%  
  select(income_type, 대출수) %>% 
  ggplot(aes(fill=대출수))+
  geom_bar(aes(income_type),
           col='white',
           position = 'fill')+
  theme(axis.text.x=element_text(angle=45, hjust=1))




#1-8) company_enter_month
eda=eda %>% 
  mutate(company_enter_month=ymd(ifelse(company_enter_month > 2000000, 
                                        as.character(ymd(company_enter_month)), 
                                        as.character(ym(company_enter_month)))))



#년 별
eda %>% 
  filter(!is.na(company_enter_month)) %>% 
  mutate(company_enter_month=ymd(ifelse(company_enter_month > 2000000, 
                                        as.character(ymd(company_enter_month)), 
                                        as.character(ym(company_enter_month))))) %>%
  mutate(company_enter_month=year(company_enter_month)) %>%
  ggplot(aes(fill=대출수))+
  geom_histogram(aes(company_enter_month),
                 col='white',
                 position = 'fill',
                 binwidth = 1)+
  scale_y_continuous(labels = scales::comma)

#월 별
eda %>% 
  filter(!is.na(company_enter_month)) %>% 
  mutate(company_enter_month=ymd(ifelse(company_enter_month > 2000000, 
                                        as.character(ymd(company_enter_month)), 
                                        as.character(ym(company_enter_month))))) %>%
  mutate(company_enter_month=month(company_enter_month)) %>% 
  ggplot(fill=대출수)+
  geom_histogram(aes(company_enter_month),
                 col='white',
                 binwidth = 1)+
  scale_y_continuous(labels = scales::comma)

#1-9) 고용 형태 따른 대출 신청 비율 확인

eda %>%  
  select(employment_type, 대출수) %>% 
  ggplot(aes(fill=대출수))+
  geom_bar(aes(employment_type),
           col='white')+
  theme(axis.text.x=element_text(angle=45, hjust=1))+
  scale_y_continuous(labels = scales::comma)

eda %>%  
  select(employment_type, 대출수) %>% 
  filter(employment_type!='정규직') %>% 
  ggplot(aes(fill=대출수))+
  geom_bar(aes(employment_type),
           col='white')+
  theme(axis.text.x=element_text(angle=45, hjust=1))+
  scale_y_continuous(labels = scales::comma)



eda %>%  
  select(employment_type, 대출수) %>% 
  ggplot(aes(fill=대출수))+
  geom_bar(aes(employment_type),
           col='white',
           position = 'fill')+
  theme(axis.text.x=element_text(angle=45, hjust=1))


#1-10) 주거소유형태 따른 대출 신청 여부 확인

eda %>%  
  select(houseown_type, 대출수) %>% 
  ggplot(aes(fill=대출수))+
  geom_bar(aes(houseown_type),
           col='white')+
  theme(axis.text.x=element_text(angle=45, hjust=1))+
  scale_y_continuous(labels = scales::comma)

eda %>%  
  select(houseown_type, 대출수) %>% 
  ggplot(aes(fill=대출수))+
  geom_bar(aes(houseown_type),
           col='white',
           position = 'fill')+
  theme(axis.text.x=element_text(angle=45, hjust=1))


#1-11) 대출 희망 금액 따른 대출 신청 여부 확인

# 대출 희망 금액 1억 이하
eda %>% 
  select(desired_amount, 대출수) %>% 
  filter(desired_amount <= 100000000) %>% 
  ggplot(aes(fill=대출수))+
  geom_histogram(aes(desired_amount),
                 col='white',
                 position = 'fill',
                 bins=20)+
  scale_y_continuous(labels = scales::comma)+
  scale_x_continuous(labels = scales::comma)

eda %>% 
  select(desired_amount, 대출수) %>% 
  filter(desired_amount <= 100000000) %>% 
  ggplot(aes(fill=대출수))+
  geom_histogram(aes(desired_amount),
                 col='white',
                 bins=20)+
  scale_y_continuous(labels = scales::comma)+
  scale_x_continuous(labels = scales::comma)


# 대출 희망 금액 1억 초과 10억 미만
eda %>% 
  select(desired_amount, 대출수) %>% 
  filter(desired_amount > 100000000 & desired_amount < 1000000000) %>% 
  ggplot(aes(fill=대출수))+
  geom_histogram(aes(desired_amount),
                 col='white',
                 position = 'fill',
                 bins=20)+
  scale_y_continuous(labels = scales::comma)+
  scale_x_continuous(labels = scales::comma)

eda %>% 
  select(desired_amount, 대출수) %>% 
  filter(desired_amount > 100000000 & desired_amount < 1000000000) %>% 
  ggplot(aes(fill=대출수))+
  geom_histogram(aes(desired_amount),
                 col='white',
                 bins=20)+
  scale_y_continuous(labels = scales::comma)+
  scale_x_continuous(labels = scales::comma)

# 대출 희망 금액 10억 이상
eda %>% 
  select(desired_amount, 대출수) %>% 
  filter(desired_amount >= 1000000000) %>% 
  ggplot(aes(fill=대출수))+
  geom_histogram(aes(desired_amount),
                 col='white',
                 position = 'fill',
                 bins=20)+
  scale_y_continuous(labels = scales::comma)+
  scale_x_continuous(labels = scales::comma)

eda %>% 
  select(desired_amount, 대출수) %>% 
  filter(desired_amount >= 1000000000) %>% 
  ggplot(aes(fill=대출수))+
  geom_histogram(aes(desired_amount),
                 col='white',
                 bins=20)+
  scale_y_continuous(labels = scales::comma)+
  scale_x_continuous(labels = scales::comma)


#1-12) 대출 목적 따른 대출 신청 여부 확인

eda %>%  
  select(purpose, 대출수) %>% 
  ggplot(aes(fill=대출수))+
  geom_bar(aes(purpose),
           col='white')+
  theme(axis.text.x=element_text(angle=45, hjust=1))+
  scale_y_continuous(labels = scales::comma)

eda %>%  
  select(purpose, 대출수) %>% 
  filter(!(purpose %in% c('대환대출','생활비'))) %>% 
  ggplot(aes(fill=대출수))+
  geom_bar(aes(purpose),
           col='white')+
  theme(axis.text.x=element_text(angle=45, hjust=1))+
  scale_y_continuous(labels = scales::comma)



eda %>%  
  select(purpose, 대출수) %>% 
  ggplot(aes(fill=대출수))+
  geom_bar(aes(purpose),
           col='white',
           position = 'fill')+
  theme(axis.text.x=element_text(angle=45, hjust=1))



#1-13) 개인회생자 여부 따른 대출 신청 여부 확인

sum(is.na(eda[,'personal_rehabilitation_yn']))

eda %>%  
  select(personal_rehabilitation_yn, 대출수) %>% 
  ggplot(aes(fill=대출수))+
  geom_bar(aes(personal_rehabilitation_yn),
           col='white')+
  theme(axis.text.x=element_text(angle=45, hjust=1))+
  scale_y_continuous(labels = scales::comma)

eda %>%  
  select(personal_rehabilitation_yn, 대출수) %>% 
  ggplot(aes(fill=대출수))+
  geom_bar(aes(personal_rehabilitation_yn),
           col='white',
           position = 'fill')+
  theme(axis.text.x=element_text(angle=45, hjust=1))


#1-14) 개인회생자 납입 완료 여부 따른 대출 신청 여부 확인

eda %>%  
  select(personal_rehabilitation_complete_yn, 대출수) %>% 
  ggplot(aes(fill=대출수))+
  geom_bar(aes(personal_rehabilitation_complete_yn),
           col='white')+
  theme(axis.text.x=element_text(angle=45, hjust=1))+
  scale_y_continuous(labels = scales::comma)

eda %>%  
  select(personal_rehabilitation_complete_yn, 대출수) %>% 
  ggplot(aes(fill=대출수))+
  geom_bar(aes(personal_rehabilitation_complete_yn),
           col='white',
           position = 'fill')+
  theme(axis.text.x=element_text(angle=45, hjust=1))



#1-15) 기대출수 따른 대출 신청 여부 확인

#기대출 수 15 미만
eda %>% 
  filter(existing_loan_cnt < 15) %>% 
  select(existing_loan_cnt, 대출수) %>% 
  ggplot(aes(fill=대출수))+
  geom_histogram(aes(existing_loan_cnt),
                 col='white',
                 binwidth = 1)+
  scale_y_continuous(labels = scales::comma)

eda %>% 
  filter(existing_loan_cnt < 15) %>% 
  select(existing_loan_cnt, 대출수) %>% 
  ggplot(aes(fill=대출수))+
  geom_histogram(aes(existing_loan_cnt),
                 col='white',
                 position = 'fill',
                 binwidth = 1)+
  scale_y_continuous(labels = scales::comma)


#기대출 수 15 이상
eda %>% 
  filter(existing_loan_cnt >= 15 & existing_loan_cnt <= 50) %>% 
  select(existing_loan_cnt, 대출수) %>% 
  ggplot(aes(fill=대출수))+
  geom_histogram(aes(existing_loan_cnt),
                 col='white',
                 binwidth = 1)+
  scale_y_continuous(labels = scales::comma)

eda %>% 
  filter(existing_loan_cnt >= 15 & existing_loan_cnt <= 50) %>% 
  select(existing_loan_cnt, 대출수) %>% 
  ggplot(aes(fill=대출수))+
  geom_histogram(aes(existing_loan_cnt),
                 col='white',
                 position = 'fill',
                 binwidth = 1)+
  scale_y_continuous(labels = scales::comma)


#1-16) 기대출 금액 따른 대출 신청 여부 확인

#기대출액 1억 이하
eda %>% 
  filter(existing_loan_amt <= 100000000) %>% 
  select(existing_loan_amt, 대출수) %>% 
  ggplot(aes(fill=대출수))+
  geom_histogram(aes(existing_loan_amt),
                 col='white',
                 bins = 20)+
  scale_y_continuous(labels = scales::comma)+
  scale_x_continuous(labels = scales::comma)

eda %>% 
  filter(existing_loan_amt <= 100000000) %>% 
  select(existing_loan_amt, 대출수) %>% 
  ggplot(aes(fill=대출수))+
  geom_histogram(aes(existing_loan_amt),
                 col='white',
                 position = 'fill',
                 bins = 20)+
  scale_y_continuous(labels = scales::comma)+
  scale_x_continuous(labels = scales::comma)




#기대출액 1억 초과 10억 이하
eda %>% 
  filter(existing_loan_amt > 100000000 & existing_loan_amt < 1000000000) %>% 
  select(existing_loan_amt, 대출수) %>% 
  ggplot(aes(fill=대출수))+
  geom_histogram(aes(existing_loan_amt),
                 col='white',
                 bins = 20)+
  scale_y_continuous(labels = scales::comma)+
  scale_x_continuous(labels = scales::comma)

eda %>% 
  filter(existing_loan_amt > 100000000 & existing_loan_amt < 1000000000) %>% 
  select(existing_loan_amt, 대출수) %>% 
  ggplot(aes(fill=대출수))+
  geom_histogram(aes(existing_loan_amt),
                 col='white',
                 position = 'fill',
                 bins = 20)+
  scale_y_continuous(labels = scales::comma)+
  scale_x_continuous(labels = scales::comma)

#1-17) 한도조회 신청 시간

#일
eda %>% 
  select(loanapply_insert_time, 대출수) %>% 
  mutate(loanapply_insert_time=mday(loanapply_insert_time)) %>%
  ggplot(aes(fill=대출수))+
  geom_histogram(aes(loanapply_insert_time), 
           col='white', 
           position = 'fill',
           binwidth = 1)+
  scale_y_continuous(labels = scales::comma)

eda %>% 
  select(loanapply_insert_time, 대출수) %>% 
  mutate(loanapply_insert_time=mday(loanapply_insert_time)) %>% 
  mutate(loanapply_insert_time=as.factor(loanapply_insert_time)) %>% 
  ggplot(aes(fill=대출수))+
  geom_bar(aes(loanapply_insert_time), 
           col='white', 
           binwidth = 1)+
  scale_y_continuous(labels = scales::comma)


#시간
eda %>% 
  select(loanapply_insert_time, 대출수) %>% 
  mutate(loanapply_insert_time=hour(loanapply_insert_time)) %>% 
  ggplot(aes(fill=대출수))+
  geom_bar(aes(loanapply_insert_time), 
           col='white',
           position='fill',
           binwidth = 1)+
  scale_y_continuous(labels = scales::comma)

eda %>% 
  select(loanapply_insert_time, 대출수) %>% 
  mutate(loanapply_insert_time=hour(loanapply_insert_time)) %>% 
  ggplot(aes(fill=대출수))+
  geom_bar(aes(loanapply_insert_time), 
           col='white',
           binwidth = 1)+
  scale_y_continuous(labels = scales::comma)



#1-18) 은행별
eda %>% 
  mutate(bank_id=as.factor(bank_id)) %>% 
  select(bank_id, 대출수) %>% 
  ggplot(aes(fill=대출수))+
  geom_bar(aes(bank_id),
           col='white')+
  scale_y_continuous(labels = scales::comma)+
  theme(axis.text.x=element_text(angle=90, hjust=1))

eda %>% 
  mutate(bank_id=as.factor(bank_id)) %>% 
  select(bank_id, 대출수) %>% 
  ggplot(aes(fill=대출수))+
  geom_bar(aes(bank_id),
           col='white',
           position = 'fill')+
  scale_y_continuous(labels = scales::comma)+
  theme(axis.text.x=element_text(angle=90, hjust=1))


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




eda %>%
  select(bank_id, 대출수) %>% 
  ggplot(aes(fill=대출수))+
  geom_histogram(aes(bank_id),
           col='white',
           binwidth = 1,
           position='fill')+
  theme(axis.text.x=element_text(angle=90, hjust=1))+
  scale_y_continuous(labels = scales::comma)



#1-19) 상품별
eda %>% 
  mutate(product_id=as.factor(product_id)) %>% 
  select(product_id, 대출수) %>% 
  ggplot(aes(fill=대출수))+
  geom_bar(aes(product_id),
           col='white')+
  scale_y_continuous(labels = scales::comma)+
  theme(axis.text.x=element_text(angle=90, hjust=1))

eda %>% 
  mutate(product_id=as.factor(product_id)) %>% 
  select(product_id, 대출수) %>% 
  ggplot(aes(fill=대출수))+
  geom_bar(aes(product_id),
           col='white',
           position = 'fill')+
  scale_y_continuous(labels = scales::comma)+
  theme(axis.text.x=element_text(angle=90, hjust=1))

eda %>% 
  mutate(is_applied=as.factor(is_applied)) %>% 
  mutate(product_id=as.factor(product_id)) %>% 
  select(product_id, is_applied) %>% 
  ggplot(aes(fill=is_applied))+
  geom_bar(aes(product_id),
           col='white',
           position = 'fill')+
  scale_y_continuous(labels = scales::comma)+
  theme(axis.text.x=element_text(angle=90, hjust=1))




#1-20) 승인한도
memory.limit(size = 50000)  

#승인한도 1억 이하인 사람 분포 그래프
eda %>% 
  filter(loan_limit <= 100000000) %>% 
  ggplot(aes(fill=대출수))+
  geom_histogram(aes(loan_limit), 
                 col='white',
                 bins=20)+
  scale_y_continuous(labels = scales::comma)+
  scale_x_continuous(labels = scales::comma)

eda %>% 
  filter(loan_limit <= 100000000) %>% 
  ggplot(aes(fill=대출수))+
  geom_histogram(aes(loan_limit), 
                 col='white',
                 position='fill',
                 bins=20)+
  scale_y_continuous(labels = scales::comma)+
  scale_x_continuous(labels = scales::comma)



#1억 초과 10억 미만
eda %>% 
  filter(loan_limit > 100000000 & loan_limit <= 1000000000) %>% 
  ggplot(aes(fill=대출수))+
  geom_histogram(aes(loan_limit), 
                 col='white', 
                 bins=20)+
  scale_y_continuous(labels = scales::comma)+
  scale_x_continuous(labels = scales::comma)


eda %>% 
  filter(loan_limit > 100000000 & loan_limit <= 1000000000) %>% 
  ggplot(aes(fill=대출수))+
  geom_histogram(aes(loan_limit), 
                 col='white', 
                 position='fill',
                 bins=20)+
  scale_y_continuous(labels = scales::comma)+
  scale_x_continuous(labels = scales::comma)



#1-21) 금리
eda %>% 
  select(loan_rate, 대출수) %>% 
  ggplot(aes(fill=대출수))+
  geom_histogram(aes(loan_rate),
                 col='white',
                 binwidth = 0.5)+
  scale_y_continuous(labels = scales::comma)

eda %>% 
  select(loan_rate, 대출수) %>% 
  ggplot(aes(fill=대출수))+
  geom_histogram(aes(loan_rate),
                 col='white',
                 position = 'fill',
                 binwidth = 0.5)+
  scale_y_continuous(labels = scales::comma)




##############################################################################
eda %>% 
  filter(!is.na(personal_rehabilitation_yn)) %>% 
  select(user_id,personal_rehabilitation_yn, personal_rehabilitation_complete_yn) %>% 
  group_by(user_id) %>% 
  mutate(영우바보=length(unique(personal_rehabilitation_yn))) %>% 
  filter(영우바보 > 1) %>% 
  dim()



a=c(1,2,3,NA,3)
length(unique(a))


















