df<-read.csv('C:/Users/tomge/Desktop/EY/train.csv')
df_test<-read.csv('C:/Users/tomge/Desktop/EY/test.csv')

df_add<-df_test[!is.na(df_test$x_exit),]

df<-rbind(df,df_add)

library(Amelia)
library(dplyr)
library(chron)
library(lubridate)

df$time_entry<-as.POSIXct(df$time_entry,format="%H:%M:%S")
df$time_exit<-as.POSIXct(df$time_exit,format="%H:%M:%S")
dataf<-df
ndf<-dataf%>%group_by(hash)%>%mutate(xvel_pre= (lag(x_exit) - lag(x_entry))/as.numeric(1+lag(time_exit)- lag(time_entry))
                                     ,yvel_pre= (lag(y_exit) - lag(y_entry))/as.numeric(1+lag(time_exit)- lag(time_entry)),
                                     xdist_pre =lag(x_exit) - lag(x_entry), ydist_pre=lag(y_exit) - lag(y_entry),x_steps=n(), step_ref= 1:n(),start_hour=lubridate::hour(ymd_hms(time_entry)), day=lubridate::day(ymd_hms(time_entry)))
ndf<- ndf%>%dplyr::select(-c('trajectory_id','vmax','vmin'))
colnames(ndf)
ndf<-ndf%>%mutate(time_s=time_exit-time_entry)
colnames(ndf)
ndf<- ndf%>%dplyr::select(-c('X','time_entry','time_exit'))
ndf<-na.omit(ndf)
ndf$time_s<-as.numeric(ndf$time_s)


ndf<-ndf%>%mutate(var_loc=ifelse(x_exit>=3750901.5068&x_exit<=3770901.5068,ifelse(y_exit>=-19268905.6133&y_exit<=-19208905.6133,1,0),0))
ndf$var_loc<-as.factor(ndf$var_loc)
table(ndf$var_loc)

ndf<-ndf%>%dplyr::select('var_loc',everything())
ndf<-ndf%>%dplyr::select(-c('x_exit','y_exit'))


ndf<-ndf%>%mutate(vels_new=(xvel_pre)^2+(yvel_pre)^2,)
library(h2o)

h2o.init(nthreads=-1, max_mem_size="4G")
h2o.removeAll()
data<-as.h2o(ndf)
splits <- h2o.splitFrame(
  data,           
  0.8,   ##  create splits of 60% and 20%; 
  seed=1234)
# The split is used to get train data
train <- h2o.assign(splits[[1]], "train.hex")   ## R train, H2O train.hex
valid <- h2o.assign(splits[[2]], "valid.hex")   ## R valid, H2O valid.hex





nn_model <- h2o.deeplearning( # Neural Network Model
  model_id="dl_model_nn",     # Model Name
  training_frame=train,       # training data
  validation_frame=valid,     # validation data 
  x=2:14,                     # Predictors
  y=1,                       # dependent variable
  hidden=c(20),               # No of hidden layers and hidden nodes
  epochs=10                   # number of runs
)

nn_train_perf<-h2o.performance(h2o.getModel('dl_model_nn'))
# Validation data performance
nn_vali_perf<-nn_model@model$validation_metrics 
# Test data performance
nn_test_perf<-h2o.performance(h2o.getModel('dl_model_nn'), newdata = test)
# Model Summary
nn_sum<-summary(nn_model) 
# Model prediction
nn_pred<-h2o.predict(nn_model, test)

df1<-df_test[is.na(df_test$x_exit),]

df1$time_entry<-as.POSIXct(df1$time_entry,format="%H:%M:%S")
df1$time_exit<-as.POSIXct(df1$time_exit,format="%H:%M:%S")
dataf1<-df1
ndf1<-dataf1%>%group_by(hash)%>%mutate(xvel_pre= (lag(x_exit) - lag(x_entry))/as.numeric(1+lag(time_exit)- lag(time_entry))
                                     ,yvel_pre= (lag(y_exit) - lag(y_entry))/as.numeric(1+lag(time_exit)- lag(time_entry)),
                                     xdist_pre =lag(x_exit) - lag(x_entry), ydist_pre=lag(y_exit) - lag(y_entry),x_steps=n(), step_ref= 1:n(),start_hour=lubridate::hour(ymd_hms(time_entry)), day=lubridate::day(ymd_hms(time_entry)))
ndf1<- ndf1%>%dplyr::select(-c('trajectory_id','vmax','vmin'))
colnames(ndf1)
ndf1<-ndf1%>%mutate(time_s=time_exit-time_entry)
colnames(ndf1)
ndf1<- ndf1%>%dplyr::select(-c('X','time_entry','time_exit'))

ndf1$time_s<-as.numeric(ndf1$time_s)


ndf1<-ndf1%>%dplyr::select(-c('x_exit','y_exit'))

library(h2o)
h2o.init()
data<-as.h2o(ndf1)


test <- h2o.assign(data, "test.hex")     ## R test, H2O test.hex


new<- as.data.frame(h2o.predict(object=nn_model,newdata=test))

table(new$predict)

df_main<-cbind(df1,new$predict)
sol<-df_main[is.na(df_main$x_exit),]
fin_sol<-sol%>%select(id='trajectory_id',target='new$predict')
colnames(sol)
write.csv(fin_sol,'sol.csv')

h2o.varimp_plot(nn_model)