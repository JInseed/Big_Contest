rm(list=ls())
install.packages('Metrics')
install.packages('missForest')
library(tidyverse)
library(missForest)
library(Metrics)

df=read.csv('기대출제외_전처리.csv', fileEncoding = 'cp949',encoding = "UTF-8")
user_pr=read.csv('기대출제외_User전처리.csv', fileEncoding = 'cp949',encoding = "UTF-8")

head(user_pr)

mer=df %>%
  ungroup() %>% 
  select(application_id, 신청서별_상품수, 대출신청수_one,대출신청수_two) %>% 
  distinct() %>% 
  as.data.frame()

mer=df %>%
  ungroup() %>% 
  select(application_id, 신청서별_상품수, 대출신청수_one) %>% 
  distinct() %>% 
  as.data.frame()


user_pr2=user_pr %>% 
  left_join(mer, by='application_id')
  
colnames(user_pr)

user_pr2=user_pr2 %>% 
  filter(!is.na(existing_loan_amt)) %>% 
  select(c(yearly_income, houseown_type, desired_amount, 신청서별_상품수,rehabilitation,purpose, existing_loan_cnt, credit_score)) %>% 
  mutate(houseown_type=as.factor(houseown_type),
         rehabilitation=as.factor(rehabilitation),
         purpose=as.factor(purpose))
  
#1. 모든 변수 사용
user1=user_pr2


test1=user1 %>% 
  filter(!is.na(credit_score))

set.seed(2022)
train_index <- sample(nrow(test1), 7000)

test1=test1[train_index,]
real=test1$credit_score

set.seed(2022)
na_index=sample(nrow(test1), 580)
test1[na_index,'credit_score']=NA

miss1 = missForest(test1, maxiter = 20)

tt=as.data.frame(miss1$ximp)
predict=tt$credit_score

rmse(real,predict) #22

rmse(real[na_index],predict[na_index]) #75.9

#2.yearly_income 제외
user2=user_pr2 %>% 
  select(-yearly_income)

test2=user2 %>% 
  filter(!is.na(credit_score))

set.seed(2022)
train_index <- sample(nrow(test2), 7000)

test2=test2[train_index,]
real=test2$credit_score

set.seed(2022)
na_index=sample(nrow(test2), 580)
test2[na_index,'credit_score']=NA

miss2 = missForest(test2, maxiter = 20)

tt=as.data.frame(miss2$ximp)
predict=tt$credit_score


rmse(real[na_index],predict[na_index]) #79.2


#3.houseown_type 제외
user3=user_pr2 %>% 
  select(-houseown_type)

test3=user3 %>% 
  filter(!is.na(credit_score))

set.seed(2022)
train_index <- sample(nrow(test3), 7000)

test3=test3[train_index,]
real=test3$credit_score

set.seed(2022)
na_index=sample(nrow(test3), 580)
test3[na_index,'credit_score']=NA

miss3 = missForest(test3, maxiter = 20)

tt=as.data.frame(miss3$ximp)
predict=tt$credit_score


rmse(real[na_index],predict[na_index]) #76.2

#4.desired_amount 제외
user4=user_pr2 %>% 
  select(-desired_amount)

test4=user4 %>% 
  filter(!is.na(credit_score))

set.seed(2022)
train_index <- sample(nrow(test4), 7000)

test4=test4[train_index,]
real=test4$credit_score

set.seed(2022)
na_index=sample(nrow(test4), 580)
test4[na_index,'credit_score']=NA

miss4 = missForest(test4, maxiter = 20)

tt=as.data.frame(miss4$ximp)
predict=tt$credit_score


rmse(real[na_index],predict[na_index]) #77.7

#5.yearly_income, houseown_type 제외
user5=user_pr2 %>% 
  select(-c(yearly_income, houseown_type))

test5=user5 %>% 
  filter(!is.na(credit_score))

set.seed(2022)
train_index <- sample(nrow(test5), 7000)

test5=test5[train_index,]
real=test5$credit_score

set.seed(2022)
na_index=sample(nrow(test5), 580)
test5[na_index,'credit_score']=NA

miss5 = missForest(test5, maxiter = 20)

tt=as.data.frame(miss5$ximp)
predict=tt$credit_score


rmse(real[na_index],predict[na_index]) #79.8


#6.yearly_income, desired_amount 제외
user6=user_pr2 %>% 
  select(-c(yearly_income, desired_amount))

test6=user6 %>% 
  filter(!is.na(credit_score))

set.seed(2022)
train_index <- sample(nrow(test6), 7000)

test6=test6[train_index,]
real=test6$credit_score

set.seed(2022)
na_index=sample(nrow(test6), 580)
test6[na_index,'credit_score']=NA

miss6 = missForest(test6, maxiter = 20)

tt=as.data.frame(miss6$ximp)
predict=tt$credit_score


rmse(real[na_index],predict[na_index]) #81.9


#7.houseown_type, desired_amount 제외
user7=user_pr2 %>% 
  select(-c(houseown_type, desired_amount))

test7=user7 %>% 
  filter(!is.na(credit_score))

set.seed(2022)
train_index <- sample(nrow(test7), 7000)

test7=test7[train_index,]
real=test7$credit_score

set.seed(2022)
na_index=sample(nrow(test7), 580)
test7[na_index,'credit_score']=NA

miss7 = missForest(test7, maxiter = 20)

tt=as.data.frame(miss7$ximp)
predict=tt$credit_score


rmse(real[na_index],predict[na_index]) #78.8

#8.yearly_income, houseown_type, desired_amount 제외
user8=user_pr2 %>% 
  select(-c(yearly_income,houseown_type, desired_amount))

test8=user8 %>% 
  filter(!is.na(credit_score))

set.seed(2022)
train_index <- sample(nrow(test8), 7000)

test8=test8[train_index,]
real=test8$credit_score

set.seed(2022)
na_index=sample(nrow(test8), 580)
test8[na_index,'credit_score']=NA

miss8 = missForest(test8, maxiter = 20)

tt=as.data.frame(miss8$ximp)
predict=tt$credit_score


rmse(real[na_index],predict[na_index]) #83.7


#대출 신청 분할 비교

user_pr3=user_pr2 %>% 
  filter(!is.na(existing_loan_amt)) %>% 
  select(c(yearly_income, houseown_type, desired_amount, 신청서별_상품수,rehabilitation,purpose, existing_loan_cnt, existing_loan_amt,credit_score, income_type, gender, age, work_year,대출신청수_one, 대출신청수_two)) %>% 
  mutate(houseown_type=as.factor(houseown_type),
         rehabilitation=as.factor(rehabilitation),
         purpose=as.factor(purpose),
         gender=as.factor(gender),
         age=as.factor(age),
         work_year=as.factor(work_year),
         income_type=as.factor(income_type),
         대출신청수_one=as.factor(대출신청수_one),
         대출신청수_two=as.factor(대출신청수_two))

test10=user_pr3 %>% 
  filter(!is.na(credit_score)) %>% 
  select(-c(gender, houseown_type))

set.seed(2021)
train_index <- sample(nrow(test10), 7000)

test10=test10[train_index,]
real=test10$credit_score

set.seed(2022)
na_index=sample(nrow(test10), 580)
test10[na_index,'credit_score']=NA

miss10 = missForest(test10, maxiter = 20)

tt=as.data.frame(miss10$ximp)
predict=tt$credit_score

rmse(real[na_index],predict[na_index])






test10=user_pr3 %>% 
  filter(!is.na(credit_score)) %>% 
  select(-c(gender,houseown_type,desired_amount, 대출신청수_two))

set.seed(2022)
train_index <- sample(nrow(test10), 7000)

test10=test10[train_index,]
real=test10$credit_score

set.seed(2022)
na_index=sample(nrow(test10), 580)
test10[na_index,'credit_score']=NA

miss10 = missForest(test10, maxiter = 10)

tt=as.data.frame(miss10$ximp)
predict=tt$credit_score


rmse(real[na_index],predict[na_index]) #70.13




#test
user_pr3=user_pr %>% 
  left_join(mer, by='application_id')

colnames(user_pr)


user_pr3=user_pr3 %>% 
  filter(!is.na(existing_loan_amt)) %>% 
  select(c(yearly_income, houseown_type, desired_amount, 신청서별_상품수,rehabilitation,purpose, existing_loan_cnt, existing_loan_amt,credit_score, income_type,gender, age, work_year)) %>% 
  mutate(houseown_type=as.factor(houseown_type),
         rehabilitation=as.factor(rehabilitation),
         purpose=as.factor(purpose),
         gender=as.factor(gender),
         age=as.factor(age),
         work_year=as.factor(work_year),
         income_type=as.factor(income_type))



test10=user_pr3 %>% 
  filter(!is.na(credit_score)) %>% 
  select(-c(gender,houseown_type,desired_amount))

set.seed(2022)
train_index <- sample(nrow(test10), 7000)

test10=test10[train_index,]
real=test10$credit_score

set.seed(2022)
na_index=sample(nrow(test10), 580)
test10[na_index,'credit_score']=NA

miss10 = missForest(test10, maxiter = 20)

tt=as.data.frame(miss10$ximp)
predict=tt$credit_score


rmse(real[na_index],predict[na_index]) #75.9


sd(predict)
ggplot() + 
  geom_histogram(aes(real),
                 fill='skyblue',
                 col='white')
ggplot() + 
  geom_histogram(aes(predict),
                 fill='skyblue',
                 col='white',
                 binwidth = 10)


user_pr %>% 
  filter(!is.na(credit_score)) %>% 
  select(credit_score) %>% 
  ggplot() + 
  geom_histogram(aes(credit_score),
                 fill='skyblue',
                 col='white',
                 binwidth = 10)


  





