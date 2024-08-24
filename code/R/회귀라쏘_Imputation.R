install.packages('glmnet')
install.packages('caret')

library(caret)
library(modelr)
library(glmnet)


library(tidyverse)

df=read.csv('merge전처리.csv')

user=read.csv('C:/Users/82102/OneDrive - dongguk.edu/바탕 화면/2022 빅콘/빅콘 R/user전처리.csv')


con=read.csv('C:/Users/82102/OneDrive - dongguk.edu/바탕 화면/2022 빅콘/data/loan_result.csv', header=T, fileEncoding = 'utf-8')

loan=con %>% 
  filter(!is.na(loan_limit))





user= user %>% 
  mutate(gender=as.factor(gender),
         income_type=as.factor(income_type),
         employment_type=as.factor(employment_type),
         houseown_type=as.factor(houseown_type),
         purpose=as.factor(purpose),
         age=as.factor(age),
         work_year=as.factor(work_year),
         rehabilitation=as.factor(rehabilitation),
         loan_per=as.factor(loan_per)
  ) %>% 
  select(-insert_time)


na_user= user %>% 
  filter(is.na(credit_score)) %>% 
  as.data.frame()

nor_user= user %>% 
  filter(!is.na(credit_score)) %>% 
  as.data.frame()


X=nor_user %>% 
  select(-application_id,-user_id,-credit_score)

X=model.matrix(~.,data=X)[,-1]
X=unlist(X)

y=nor_user[,'credit_score']


#라쏘
fit=glmnet(X, y , family='gaussian', alpha=1)

weights = (rep(1, dim(X)[1]))


#최적 람다 찾기
lam <- cv.glmnet(X, y, family='gaussian', alpha=1, nfolds = 3, weights=c(0.005,0.995))
lamda=lam$lambda.min # MSE 최소화 lambda = 0.07759729
plot(lam)  


fit2=glmnet(X, y , family='gaussian', alpha=1, lambda = lamda)


row.names(na_user)=na_user$application_id

X=na_user %>% 
  select(-application_id,-user_id,-credit_score)


X=model.matrix(~.,data=X)[,-1]



prediction=predict(fit2, s=lamda, newx=X)
prediction=as.data.frame(prediction)

colnames(prediction)='credit_score'

prediction=prediction %>% 
  mutate(application_id=row.names(prediction)) %>% 
  mutate(application_id=as.numeric(application_id))



na_user=na_user %>% 
  select(-credit_score) %>% 
  left_join(prediction, by='application_id')

na_user=na_user %>% 
  mutate(credit_score=floor(credit_score))


impute_user=rbind(na_user,nor_user)

rownames(impute_user)=NULL

impute_user=impute_user %>% 
  mutate(credit_score=ifelse(credit_score >= 1000, 1000, credit_score))

df=impute_user %>% 
  left_join(loan, by='application_id')




test=read.csv('modeling_data1.csv')
test2=read.csv('user_user.csv')

c1=test2 %>% 
  filter(application_id %in% app) %>% 
  arrange(application_id) %>% 
  select(credit_score)

c2=impute_user %>% 
  filter(application_id %in% app) %>% 
  arrange(application_id) %>% 
  select(credit_score)


dim(c1)

dif=data.frame('c1'=c1,'c2'=c2)

dif %>% 
  mutate(차이= as.numeric(credit_score)-as.numeric(credit_score.1)) %>% 
  ggplot()+
  geom_histogram(aes(차이),
                 col='white',
                 fill='skyblue',
                 bins=200)


head(dif)

app=unique(na_user[,'application_id'])


model_lasso <- train(x=X,y=y,
                     method="glmnet",
                     metric="RMSE",
                     maximize=FALSE,
                     tuneGrid=expand.grid(alpha=1,  # Lasso regression
                                          lambda=c(1,0.1,0.05,0.01,seq(0.009,0.001,-0.001),
                                                   0.00075,0.0005,0.0001)))

model_lasso
mean(model_lasso$resample$RMSE)


# extract coefficients for the best performing model
coef <- data.frame(coef.name = dimnames(coef(model_lasso$finalModel,s=model_lasso$bestTune$lambda))[[1]], 
                   coef.value = matrix(coef(model_lasso$finalModel,s=model_lasso$bestTune$lambda)))

# exclude the (Intercept) term
coef <- coef[-1,]

# print summary of model results
picked_features <- nrow(dplyr::filter(coef,coef.value!=0)) #yk dplyr::filter
lasso_features <- dplyr::filter(coef,coef.value!=0)
not_picked_features <- nrow(dplyr::filter(coef,coef.value==0))

cat("Lasso picked",picked_features,"variables and eliminated the other",
    not_picked_features,"variables\n")


# sort coefficients in ascending order
coef <- dplyr::arrange(coef,-coef.value)

# extract the top 10 and bottom 10 features
imp_coef <- rbind(head(coef,10),
                  tail(coef,10))

ggplot(imp_coef) +
  geom_bar(aes(x=reorder(coef.name,coef.value),y=coef.value),
           stat="identity") +
  coord_flip() +
  ggtitle("Coefficents in the Lasso Model") +
  theme(axis.title=element_blank())

ggplot(coef)+
  geom_bar(aes(x=reorder(coef.name, abs(coef.value)), abs(coef.value)),
           stat='identity')+
  coord_flip() +
  ggtitle("Coefficents in the Lasso Model") +
  theme(axis.title=element_blank())

colSums(is.na(df))



nor_df=df %>% 
  filter(!is.na(is_applied))

na_df=df %>% 
  filter(is.na(is_applied))

write.csv(na_df, file='전처리test.csv', row.names = F)
write.csv(nor_df, file='전처리train.csv', row.names = F)
write.csv(df, file = "전처리complete.csv", row.names = F)

















