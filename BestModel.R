

library(Amelia)
library(dplyr)
library(chron)
library(lubridate)

ndf<-read.csv('alldat.csv')

ndf<- ndf%>%dplyr::select(-c('X'))


ndf$time_s<-as.numeric(ndf$time_s)


ndf<-ndf%>%mutate(var_loc=ifelse(x_exit>=3750901.5068&x_exit<=3770901.5068,ifelse(y_exit>=-19268905.6133&y_exit<=-19208905.6133,1,0),0))
ndf$var_loc<-as.factor(ndf$var_loc)
table(ndf$var_loc)



ndf$vmean[ndf$vmean=='NaN']=NA
ndf$vmean[is.na(ndf$vmean)]=ndf$vmeans_n[is.na(ndf$vmean)]
ndf<-ndf%>%mutate(dist=time_s*vmean)

ndf1<-ndf
ndf<-ndf1[!is.na(ndf1$x_exit),]
ndf<-na.omit(ndf)

ndf<-ndf%>%dplyr::select(var_loc,everything())
ndf<-ndf%>%dplyr::select(-c('x_exit','y_exit'))
library(h2o)

h2o.init(nthreads=-1, max_mem_size="6G")
h2o.removeAll()
data<-as.h2o(ndf)
splits <- h2o.splitFrame(
  data,           
  0.8,   ##  create splits of 60% and 20%; 
  seed=1234)
# The split is used to get train data
train <- h2o.assign(splits[[1]], "train.hex")   ## R train, H2O train.hex
valid <- h2o.assign(splits[[2]], "valid.hex")   ## R valid, H2O valid.hex


new_mod<-h2o.gbm(
  training_frame = train,      ## H2O frame holding the training data
  validation_frame = valid,  ## extra holdout piece for three layer modeling
  y=1,                 ## this can be names or column numbers
  x=3:24,                   ## target: using the logged variable created earlier
  model_id="gbm1",              ## internal H2O name for model
  ntrees = 25,                  ## use fewer trees than default (50) to speed up training
  learn_rate = 0.2,             ## lower learn_rate is better, but use high rate to offset few trees
  score_tree_interval = 3,      ## score every 3 trees
  sample_rate = 0.8,            ## use half the rows each scoring round
  col_sample_rate = 0.8
  )
 
model2 <- h2o.randomForest(x = 2:24, #features
                           y = 1, #targets
                           training_frame = train, #training data
                           validation_frame = valid,#validation data 
                           ntrees = 100, #default trees is 50
                           max_depth = 20, #default is 20,
                           score_each_iteration = TRUE,## Predict against training and validation for each tree
                           fold_assignment = 'Stratified', #startified sampling
                           nfolds=2,
                           balance_classes = TRUE#balance class
)

nn_train_perf<-h2o.performance(h2o.getModel("gbm1"))
h2o.performance(model2)
# Validation data performance
nn_vali_perf<-new_mod$validation_metrics 
# Test data performance

h2o.varimp_plot(model2)




ndf1<-read.csv('ndf1.csv')

ndf1$time_entry<-ymd_hms(ndf1$time_entry)
ndf1$time_exit<-ymd_hms(ndf1$time_exit)
ndf1<-ndf1%>%mutate(time_s=time_exit-time_entry)
ndf1<-ndf1%>%mutate(end_hour=lubridate::hour(ymd_hms(time_exit)))
ndf1<- ndf1%>%dplyr::select(-c('X','X.1','time_entry','time_exit'))


ndf1$time_s<-as.numeric(ndf1$time_s)





ndf1<-ndf1%>%dplyr::select(-c('x_exit','y_exit'))


ndf1<-ndf1%>%mutate(vels_new=(xvel_pre)^2+(yvel_pre)^2,vel_rat=(1+xvel_pre)/(1+yvel_pre))


ndf1<-ndf1%>%group_by(hash)%>%mutate(velx_max=max(xvel_pre,na.rm = T),vely_max=max(yvel_pre,na.rm = T),velx_min=min(xvel_pre,na.rm = T),vely_min=min(yvel_pre,na.rm = T))
ndf1<-ndf1%>%group_by(hash)%>%mutate(vmeans_n=mean(vels_new,na.rm = T))

ndf$vmean[ndf$vmean=='NaN']=NA
ndf$vmean[is.na(ndf$vmean)]=ndf$vmeans_n[is.na(ndf$vmean)]
ndf<-ndf%>%mutate(dist=time_s*vmean)

ndf2<-ndf1[is.na(ndf1$x_exit),]

data<-as.h2o(ndf2)


test <- h2o.assign(data, "test.hex")     ## R test, H2O test.hex


new<- as.data.frame(h2o.predict(object=new_mod,newdata=test))
new<- as.data.frame(h2o.predict(object=model2,newdata=test))
table(new$predict)

df_main<-cbind(ndf2,new$predict)
sol<-df_main[is.na(df_main$x_exit),]
fin_sol<-sol%>%select(target='new$predict')
colnames(sol)
write.csv(fin_sol,'sol2.csv')

h2o.varimp_plot(nn_model)