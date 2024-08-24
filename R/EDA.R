library(tidyverse)
library(lubridate)

#데이터 불러오기
user=read.csv('C:/Users/82102/OneDrive - dongguk.edu/바탕 화면/2022 빅콘/data/user_spec.csv', header=T, fileEncoding = 'utf-8')

con=read.csv('C:/Users/82102/OneDrive - dongguk.edu/바탕 화면/2022 빅콘/data/loan_result.csv', header=T, fileEncoding = 'utf-8')


app=con %>% 
  filter(is.na(is_applied)) %>% 
  filter(!is.na(loan_rate))



#yearly_income 이 NA인 경우 90개인데 85개는 삭제해도 될거 같고 나머지 5이 yearly_income과 
user %>% 
  filter(is.na(yearly_income)) %>% 
  view()

car=user %>% 
  filter(purpose %in% c('자동차구입','BUYCAR'))

colSums(is.na(car))

user %>% 
  select(purpose, personal_rehabilitation_yn) %>% 
  table(useNA = 'ifany')


#입사연월 NA 일 때 근무형태가 기타가 대부분임을 확인
user %>% 
  filter(is.na(company_enter_month)) %>% 
  select(employment_type) %>% 
  table(useNA = 'ifany')

app2=user %>% 
  filter(is.na(company_enter_month)) %>% 
  filter(employment_type %in% c('계약직','정규직')) %>% 
  select(user_id) %>% 
  unique()

app2=unique(app2[,'user_id'])

user %>% 
  filter(user_id %in% app2) %>% 
  arrange(user_id) %>% 
  view()

user %>%
  ungroup() %>% 
  mutate(company_year=year(ym(company_enter_month))) %>% 
  mutate(dif=company_year-birth_year) %>% 
  filter(dif <= 0) %>% 
  dim()

year(ym((format(ym('2001.02'),'%Y-%m'))))


# 개인회생자 여부 NA 일 시 3,4월에 대부분인 것 확인 및 3월에 개인회생자 여부가 아예 없음 확인
user %>% 
  select(insert_time, personal_rehabilitation_yn) %>%
  mutate(insert_time=month(insert_time)) %>% 
  table(useNA = 'ifany')

# 개인회생자 여부 묻는란이 4월 18일에 생겼다는 것 확인
user %>%
  select(insert_time, personal_rehabilitation_yn) %>%
  mutate(month=month(insert_time)) %>% 
  filter(month==4) %>%
  mutate(day=mday(insert_time)) %>% 
  select(day, personal_rehabilitation_yn) %>% 
  table(useNA = 'ifany')

#gender NA 찾기
user %>% 
  filter(is.na(gender)) %>% 
  mutate(insert_time=month(insert_time)) %>% 
  select(gender, insert_time) %>% 
  table(useNA = 'ifany')

user %>% 
  filter(is.na(gender)) %>% 
  select(gender, employment_type) %>% 
  table(useNA = 'ifany') %>% 
  view()

#입사연월 여러개
user %>%
  group_by(user_id) %>% 
  mutate(num=length(unique(company_enter_month))) %>% 
  ungroup() %>% 
  select(num) %>% 
  table(useNA = 'ifany')

user %>%
  group_by(user_id) %>% 
  mutate(num=length(unique(company_enter_month))) %>% 
  ungroup() %>% 
  filter(num >=2) %>% 
  select(user_id) %>% 
  unique() %>% 
  dim()

user %>%
  group_by(user_id) %>% 
  mutate(num=length(unique(company_enter_month))) %>% 
  ungroup() %>% 
  filter(num == 31) %>% 
  view()
  
user %>% 
  select(user_id) %>% 
  unique() %>% 
  dim()

user %>%  
  filter(employment_type!='기타') %>% 
  group_by(user_id) %>% 
  filter(is.na(company_enter_month)) %>% 
  group_by(user_id) %>% 
  mutate(num=length(unique(company_enter_month))) %>% 
  ungroup() %>% arrange(user_id) %>% head(100) %>% view()
  filter(num==2) %>% 
  arrange(user_id) %>% 
  head(100) %>% 
  view()


user %>%
  group_by(user_id) %>% 
  mutate(num=length(unique(company_enter_month))) %>% 
  ungroup() %>% 
  filter(num==2) %>% 
  arrange(user_id) %>% 
  head(100) %>% 
  view()


user %>% 
  arrange(user_id) %>% 
  select(user_id, company_enter_month) %>% 
  head(100) %>% 
  view()




apply(A, 1, function(x)unique(x[!is.na(x)]))
#생년월일 2개 이상..?
user %>% 
  group_by(user_id) %>% 
  mutate(num=length(unique(is.na(birth_year)))) %>% 
  ungroup() %>% 
  select(num) %>% 
  table(useNA = 'ifany')

user %>% 
  group_by(user_id) %>% 
  mutate(num=length(unique(is.na(birth_year)))) %>% 
  ungroup() %>% 
  filter(num==2) %>% 
  select(user_id) %>% 
  unique() %>% 
  dim()


user %>% 
  filter(!is.na(birth_year)) %>% 
  group_by(user_id) %>% 
  mutate(num=length(unique(birth_year))) %>% 
  ungroup() %>% 
  select(num) %>% 
  table(useNA = 'ifany')

#rate_rank 대출 신청 비율 그래프
memory.limit(50000)
df=df %>% 
  group_by(application_id) %>% 
  mutate(rate_rank=min_rank(loan_rate)) %>% 
  ungroup()

df %>% 
  filter(!is.na(is_applied)) %>% 
  mutate(is_applied=as.factor(is_applied)) %>% 
  group_by(application_id) %>% 
  mutate(rate_rank=min_rank(loan_rate)) %>% 
  ungroup() %>% 
  ggplot(aes(fill=is_applied)) + 
  geom_histogram(aes(rate_rank),
                 col='white',
                 position='fill',
                 binwidth=1)+
  scale_y_continuous(labels = scales::comma)

#한 신청서 쓰기전 신청서 쓴 수에 따른 대출신청 비율 분포

df %>% 
  filter(!is.na(is_applied)) %>% 
  mutate(is_applied=as.factor(is_applied)) %>% 
  ggplot(aes(fill=is_applied)) + 
  geom_histogram(aes(신청서직전),
                 col='white',
                 position='fill',
                 binwidth=1)+
  scale_y_continuous(labels = scales::comma)

df %>% 
  filter(!is.na(is_applied)) %>% 
  mutate(is_applied=as.factor(is_applied)) %>% 
  ggplot(aes(fill=is_applied)) + 
  geom_histogram(aes(신청서직전),
                 col='white',
                 position='stack',
                 binwidth=1)+
  scale_y_continuous(labels = scales::comma)





  
  
a=c(1,1,1,2,2,2,2,2,3,3,3,4,5,5)
b=c('y','y','u','q','q',NA,NA,'q','w',NA,'h','e',NA,NA)
c=c(10,10,15,20,20,NA,NA,20,30,NA,40,50,NA,NA)
d=c(10,10,12,22,25,17,30,20,50,40,30,20,10,10)
tt=data.frame(a,d)
tt
rank(d)

tt %>% 
  group_by(a) %>% 
  mutate(test=min_rank(d))


sum(is.na(unique(a)))



tt %>% 
  group_by(a) %>% 
  mutate(t=ifelse( sum(is.na(unique(b))) == 0 & length(unique(b)) == 1, b,
                       ifelse(sum(is.na(unique(b))) == 0 & length(unique(b)) >= 1 ,'비정상',
                              ifelse(sum(is.na(unique(b))) == 1 & length(unique(b)) == 1, NA,
                                     ifelse(sum(is.na(unique(b))) == 1 & length(unique(b)) == 2, unique(b[!is.na(b)]), '비정상'
                       )))))

tt %>% 
  group_by(a) %>% 
  mutate(t=ifelse( sum(is.na(unique(b))) == 0 & length(unique(b)) == 1, b,
                   ifelse(sum(is.na(unique(b))) == 0 & length(unique(b)) >= 1 ,'비정상',
                          ifelse(sum(is.na(unique(b))) == 1 & length(unique(b)) == 1, NA,
                                 ifelse(sum(is.na(unique(b))) == 1 & length(unique(b)) == 2, unique(b[!is.na(b)]), '비정상'
                                 )))))




y=c(2,NA,2,2)
unique(y[!is.na(y)])


tt %>% 
  group_by(a) %>% 
  mutate(num=length(unique(!is.na(b)))) %>% 
  ungroup() %>% 
  select(a,num)

tt %>% 
  group_by(a) %>% 
  mutate(num=paste(unique((b))))
  
test=unique(b)
test[test=='u']
sum(test=='u')

te=c('a','b','c',NA)
te[te=='a']

te[is.na(te)]

test[4]

      
c=c(NA,1)
length(unique(!is.na(c)))
apply(b, 1, unique(b[!is.na(b)]))


#성별이 2개 인사람..??
user %>% 
  group_by(user_id) %>% 
  mutate(num=length(unique(gender))) %>% 
  ungroup() %>% 
  select(num) %>% 
  table(useNA = 'ifany')

user %>% 
  group_by(user_id) %>% 
  mutate(num=length(unique(gender))) %>% 
  ungroup() %>% 
  filter(num >= 1) %>% dim()
  select(user_id,gender) %>% 
  arrange(user_id) %>% 
  head(100) %>% 
  view()
  
user %>%
  group_by(user_id) %>% 
  mutate(num=length(unique(gender))) %>% 
  ungroup() %>% 
  select(num) %>% 
  table(useNA = 'ifany')

user %>% 
  group_by(user_id) %>% 
  mutate(gen=sum(unique(gender), na.rm=T)) %>% 
  ungroup() %>% 
  select(gen) %>% 
  table(useNA = 'ifany')

test=user
test[is.na(test[,'gender']),'gender']=5
test[test[,'gender']==0,'gender']=7
test[test[,'gender']==1,'gender']=8


table(test[,'gender'], useNA = 'ifany')

test %>% 
  group_by(user_id) %>% 
  mutate(gen=sum(unique(gender), na.rm=T)) %>% 
  ungroup() %>% 
  select(gen) %>% 
  table(useNA = 'ifany')

test %>% 
  group_by(user_id) %>%
  mutate(gen=sum(unique(gender), na.rm=T)) %>% 
  ungroup() %>% 
  filter(gen==5) %>% 
  select(user_id) %>% 
  dim()

test %>% 
  group_by(user_id) %>% 
  mutate(gen=sum(unique(gender), na.rm=T),
         신청수 = n_distinct(application_id)) %>% 
  ungroup() %>%
  filter(gen==5) %>% 
  head(100) %>% 
  view()


app=unique(loan[,'application_id'])

user %>% 
  filter(application_id %in% app) %>% 
  select(gender) %>% 
  table(useNA = 'ifany')



str(user)


a=c(1,1,1,2,2,3,4,4,4,4)
b=c('남','남',NA,'남','남','여','남',NA,'남','여')

tt=data.frame(a,b)
  
tt %>% 
  group_by(a) %>%
  mutate(c=length(unique(b)))
  


app1=user %>% 
  filter(is.na(birth_year)) %>% 
  select(user_id)



app1=unique(app1[,'user_id'])

user %>% 
  filter(user_id %in% app1) %>% 
  ggplot()+
  geom_histogram(aes(credit_score), 
                 col='white', 
                 fill='skyblue',
                 bins=92)

user %>% 
  filter(!(user_id %in% app1)) %>% 
  ggplot()+
  geom_histogram(aes(credit_score), 
                 col='white', 
                 fill='skyblue',
                 bins=92)

user %>% 
  mutate(a=ifelse(user_id %in% app1, 0,1)) %>%
  group_by(a) %>% 
  summarise(평균=mean(credit_score,na.rm=T))


user %>% 
  mutate(age = 2023-birth_year) %>% 
  mutate(age = ifelse(age < 20 ,'10대',
                      ifelse(age < 30, '20대',
                             ifelse(age < 40 , '30대',
                                    ifelse(age < 50, '40대',
                                           ifelse(age < 60, '50대',
                                                  ifelse(age < 70, '60대',
                                                         ifelse(age < 80, '70대',
                                                                '초고령')))))))) %>% 
  select(age) %>% 
  table(useNA = 'ifany')




df %>% 
  

user %>% 
  filter(age=='초고령') %>% 
  view()

df %>% 
  filter(!is.na(is_applied)) %>% 
  mutate(is_applied=as.factor(is_applied)) %>% 
  mutate(work_year=2022-year(ym(company_enter_month))) %>% 
  mutate(work_year=ifelse(work_year==0, 0,
                          ifelse(work_year==1,1,2))) %>% 
  mutate(work_year=as.factor(work_year)) %>% 
  ggplot(aes(fill=is_applied)) + 
  geom_bar(aes(work_year),
           col='white')
  
df %>% 
  filter(!is.na(is_applied)) %>% 
  mutate(is_applied=as.factor(is_applied)) %>% 
  mutate(work_year=2022-year(ym(company_enter_month))) %>% 
  mutate(work_year=ifelse(work_year==0, 0, 1)) %>% 
  mutate(work_year=as.factor(work_year)) %>% 
  ggplot(aes(fill=is_applied)) + 
  geom_bar(aes(work_year),
           position='fill',
           col='white')

  
  select(work_year) %>% 
  table(useNA = 'ifany')


df %>% 
  filter(!is.na(is_applied)) %>% 
  mutate(is_applied=as.factor(is_applied)) %>% 
  ggplot(aes(fill=is_applied)) +
  geom_bar(aes(age),
           position='fill',
           col='white')

df %>% 
  group_by(user_id) %>% 
  mutate(대출여부= sum(is_applied, na.rm=T)) %>%
  mutate(대출신청수=ifelse(대출여부==0, '대출안함',
                          ifelse(대출여부<=3,'대출신청(1-3)','대출신청다수'))) %>% 
  filter(!is.na(is_applied)) %>% 
  mutate(is_applied=as.factor(is_applied)) %>% 
  ggplot(aes(fill=is_applied)) +
  geom_bar(aes(대출신청수),
           position='stack',
           col='white')  


user_pr %>% 
  mutate(신용=ifelse(is.na(credit_score), '결측','정상')) %>% 
  group_by(신용) %>% 
  summarise(평균=mean(신청서별_상품수))

user_pr %>% 
  mutate(신용=ifelse(is.na(credit_score), '결측','정상')) %>% 
  ggplot(aes(fill=신용))+
  geom_histogram(aes(신청서별_상품수),
                 col='white',
                 position='fill',
                 binwidth=1)

user_pr %>% 
  mutate(신용=ifelse(is.na(credit_score), '결측','정상')) %>% 
  filter(신용=='결측') %>% 
  ggplot()+
  geom_histogram(aes(신청서별_상품수),
                 col='white',
                 fill='skyblue',
                 binwidth=1)

user_pr %>% 
  mutate(신용=ifelse(is.na(credit_score), '결측','정상')) %>% 
  filter(신용=='정상') %>% 
ggplot()+
  geom_histogram(aes(신청서별_상품수),
                 col='white',
                 fill='skyblue',
                 binwidth=1)
dev.off()

                 
windows()

user_pr %>% 
  mutate(신용=ifelse(is.na(credit_score), '결측','정상')) %>% 
  group_by(신용) %>% 
  summarise(평균=mean(yearly_income))

user_pr %>% 
  mutate(신용=ifelse(is.na(credit_score), '결측','정상')) %>% 
  select(신용,rehabilitation) %>% 
  table(useNA = 'ifany')

df %>% 
  group_by(user_id) %>% 
  mutate(대출여부= sum(is_applied, na.rm=T)) %>%
  mutate(대출신청수=ifelse(대출여부==0, '대출안함',
                      ifelse(대출여부==1, '대출처음',
                             ifelse(대출여부==2, '대출2회',
                                    ifelse(대출여부==3,'대출3회','대출신청다수'))))) %>% 
  group_by(대출신청수) %>% 
  summarise(평균=mean(credit_score, na.rm=T)) %>% 
  arrange(평균)

df %>% 
  group_by(user_id) %>% 
  mutate(대출여부= sum(is_applied, na.rm=T)) %>%
  mutate(대출신청수=ifelse(대출여부==0, '대출안함',
                      ifelse(대출여부==1, '대출처음','대출신청다수'))) %>% 
  group_by(대출신청수) %>% 
  summarise(평균=mean(credit_score, na.rm=T)) %>% 
  arrange(평균)


df %>% 
  group_by(user_id) %>% 
  mutate(대출여부= sum(is_applied, na.rm=T)) %>%
  mutate(대출신청수=ifelse(대출여부==0, '대출안함',
                      ifelse(대출여부==1, '대출처음',
                             ifelse(대출여부==2, '대출2회',
                                    ifelse(대출여부==3,'대출3회','대출신청다수'))))) %>% 
  ungroup() %>% 
  select(대출신청수) %>% 
  table(useNA = 'ifany')


df$대출
  
df %>% 
  filter(desired_amount > 1000000000) %>%
  select(loan_limit) %>% 
  unique() %>% 
  view()

user %>% 
  select(desired_amount, credit_score) %>% 
  na.omit() %>% 
  cor()

user %>% 
  select(personal_rehabilitation_yn, personal_rehabilitation_complete_yn) %>% 
  table(useNA = 'ifany')


user %>% 
  filter(is.na(personal_rehabilitation_yn)) %>% 
  colSums(is.na())

loan=con %>% 
  filter(!is.na(loan_rate))

colSums(is.na(loan))


unique(user[,'purpose'])

eda %>% 
  group_by(user_id) %>% 
  mutate(신용종류=length(unique(credit_score))) %>% 
  group_by(신용종류) %>% 
  summarise(n=n()) %>% 
  arrange(신용종류)
