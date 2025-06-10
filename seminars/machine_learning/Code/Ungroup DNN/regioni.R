library(readr)
DD <- read_csv("region_italia.csv")

colnames(DD)[1] ="country"
colnames(DD)[4] ="mx_1"
colnames(DD)[8] ="mx_5"
head(DD)


DD$Sex[DD$Sex=="Female"] <-"F" 
DD$Sex[DD$Sex=="Male"] <-"M" 

DD$Age_class[DD$Age_class=="0-0"] <-"0" 

unique(DD$Year)
str(DD)
DD$Year <- as.integer(DD$Year)
DD$Age <- as.integer(DD$Age)
DD$mx_1 <-as.numeric(DD$mx_1)
DD$class <-as.character(DD$class)

library(MortalityLaws)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(data.table)
library(keras)
sessionInfo()



DD <- DD %>% data.table

DD[,logmx:=log(mx_1)]
DD[,ex:=log(mx_5)]


DD[,Country_fact:=as.integer(as.factor(country))-1]
DD[,Sex_fact:=as.integer(as.factor(Sex))-1]

DD=DD[!is.na(logmx) & mx_1>0]

scale_min_max = function(dat,dat_test)  {
  min_dat = min(dat)
  max_dat = max(dat)
  dat_scaled=(dat-min_dat)/(max_dat-min_dat)
  dat_scaled_test = (dat_test-min_dat)/(max_dat-min_dat)
  return(list(train = dat_scaled, test = dat_scaled_test, min = min_dat, max=max_dat))
}

scale_z = function(dat,dat_test)  {
  mean_dat = mean(dat)
  sd_dat = sd(dat)
  dat_scaled=(dat-mean_dat)/(sd_dat)
  dat_scaled_test = (dat_test-mean_dat)/(sd_dat)
  return(list(train = dat_scaled, test = dat_scaled_test, mean_dat = mean_dat, sd_dat=sd_dat))
}


train = DD[Year < 2004]
test = DD[Year >= 2004]

test
table(test$Year)
#scale mx
scaled = scale_min_max(train$logmx, test$logmx)
train$mx_scale = scaled$train
test$mx_scale = scaled$test

#scale e0
scaled2 = scale_min_max(train$ex, test$ex)
train$e0_scale = scaled2$train
test$e0_scale = scaled2$test
tail(test)


#### Regression
head(train)

train_reg = train[,c(2,3,11,12,13,14),with=F]
test_reg = test[,c(2,3,11,12,13,14),with=F]

year_scale = scale_min_max(train_reg$Year,test_reg$Year)

train_reg$Year = year_scale[[1]]
test_reg$Year = year_scale[[2]]


#train
x = list(Year = train_reg$Year,
         Age = train_reg$Age, Country = train_reg$Country_fact, Sex=train_reg$Sex_fact, e0=train_reg$e0_scale)

y = (main_output= train_reg$mx_scale)

#test

x_test = list(Year = test_reg$Year,
              Age = test_reg$Age, Country = test_reg$Country_fact, Sex=test_reg$Sex_fact, e0=test_reg$e0_scale)

y_test = (main_output= test_reg$mx_scale)

library(tensorflow)
set_random_seed(1)

############### Build embedding layers
e0 <- layer_input(shape = c(1), dtype = 'float32', name = 'e0')
Year <- layer_input(shape = c(1), dtype = 'float32', name = 'Year')
Age <- layer_input(shape = c(1), dtype = 'int32', name = 'Age')
Country <- layer_input(shape = c(1), dtype = 'int32', name = 'Country')
Sex <- layer_input(shape = c(1), dtype = 'int32', name = 'Sex')

length(unique(DD$Age))

Age_embed = Age %>% 
  layer_embedding(input_dim = 100, output_dim = 10,input_length = 1, name = 'Age_embed') %>%
  keras::layer_flatten()


Sex_embed = Sex %>% 
  layer_embedding(input_dim = 2, output_dim = 10,input_length = 1, name = 'Sex_embed') %>%
  keras::layer_flatten()

length(unique(DD$country))


Country_embed = Country %>% 
  layer_embedding(input_dim = 18, output_dim = 5,input_length = 1, name = 'Country_embed') %>%
  keras::layer_flatten()


main_output <- layer_concatenate(list(e0,Year,Age_embed,Sex_embed,Country_embed
)) %>% 
 
 layer_dense(units = 300, activation = 'relu') %>% #era 300 unit
# layer_dropout(0.10) %>% 
  
 layer_dense(units = 300, activation = 'relu') %>% #era 300 unit
# layer_dropout(0.10) %>% 
  
 layer_dense(units = 300, activation = 'relu') %>% #era 300 unit
# layer_dropout(0.10) %>% 
  
  layer_dense(units = 1, activation = 'sigmoid', name = 'main_output')

model <- keras_model(
  inputs = c(Year,Age,Country,Sex,e0), 
  outputs = c(main_output))

adam = optimizer_adam(learning_rate=0.0005)
lr_callback = callback_reduce_lr_on_plateau(factor=.80, patience = 8, verbose=1, cooldown = 5, min_lr = 0.0005)
model_callback =callback_model_checkpoint(filepath ="best.h5",verbose = 1,save_best_only = T )

model %>% compile(
  optimizer = adam,
  loss = "mse")

fit = model %>% fit(
  x = x,
  y = y, 
  epochs = 300,# 150
  batch_size =  682.7,verbose = 1, shuffle = T, validation_split = 0.2, callbacks = list(lr_callback,model_callback))


model = load_model_hdf5("regional_param.h5") #parametri del paper


test$mx_hat = model %>% predict(x_test)
test[,mx_hat :=exp(mx_hat *(scaled$max-scaled$min)+scaled$min)]


test %>% filter(Year==2014,Sex=="M") %>% ggplot(aes(Age,log(mx_1)))+geom_point(size=0.4)+
  geom_line(aes(Age,log(mx_hat)),col="red")+ facet_wrap(country~.)+ggtitle("Male 2014")


test %>% filter(Year==2014,Sex=="F") %>% ggplot(aes(Age,log(mx_1)))+geom_point(size=0.4)+
  geom_line(aes(Age,log(mx_hat)),col="red")+ facet_wrap(country~.)+ggtitle("Female 2014")



test$year <- as.factor(test$Year)
head(test)

rmse = function (truth, prediction)  {
  sqrt(mean((prediction - truth)^2))
}
mae = function(truth, prediction){
  mean(abs(prediction-truth))
}

tt <- test  %>% 
  group_by(Sex,country) %>%
  summarise(MAE = mae(log(mx_1),log(mx_hat)),
            RMSE = rmse(log(mx_1),log(mx_hat))) 

tt_m <- tt %>% filter(Sex=="M") %>% rename(MAE_M=MAE,RMSE_M=RMSE) 
tail(tt_m)

tt_f <- tt %>% filter(Sex=="F") %>% rename(MAE_F=MAE,RMSE_F=RMSE)
tail(tt_f)

tt_m <-tt_m[,-c(1)]
tt_f <-tt_f[,-c(1,2)]

final_table <- cbind(tt_m,tt_f)
mean(final_table$MAE_M)
mean(final_table$RMSE_M)
mean(final_table$MAE_F)
mean(final_table$RMSE_F)












