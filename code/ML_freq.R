library(sp)
library(CASdatasets)
data("swmotorcycle")
# noobs=swmotorcycle



mccasefreq=swmotorcycle
mccaseseverity=swmotorcycle

# 
# install.packages("lightgbm")
# install.packages("SHAPforxgboost")
# 
# install.packages("readr")
# install.packages("read_fwf")
# 
# install.packages("mgcv")
# install.packages("caret")
# install.packages("rpart")
# install.packages("gbm")
# 

library(ggplot2)
library(lattice)
library(caret)



# install.packages("R6")

library(R6)
library(lightgbm)



# noobs=noobs[noobs$Exposure==0,]




##데이터 전처리 및 분할

#신규가입자 제외
mccasefreq = mccasefreq[mccasefreq$Exposure>0,]
#사고 발생빈도 파생변수 생성
mccasefreq$freqv=mccasefreq$ClaimNb/mccasefreq$Exposure


#무사고자 제외
mccaseseverity=mccaseseverity[mccaseseverity$ClaimNb>0,]
#사고 중대성 파생변수 생성
mccaseseverity$severity=mccaseseverity$ClaimAmount/mccaseseverity$ClaimNb


#분할
freq_trainIndex <- createDataPartition(mccasefreq$freqv, p=0.75, list=FALSE, times=1)

freq_train = mccasefreq[freq_trainIndex,]

freq_test = mccasefreq[-freq_trainIndex,]

freq_Xtrain = data.matrix(freq_train[,c('Gender','Area','RiskClass','BonusClass',
                                        'OwnerAge', 'VehAge')])
# freq_Xtrain = data.matrix(freq_train[,c('Area','RiskClass',
#                                         'OwnerAge', 'VehAge')])


freq_XYtrain=lgb.Dataset(freq_Xtrain,
                         colnames=c('Gender','Area','RiskClass', 'BonusClass',
                                    'OwnerAge', 'VehAge'),
                         # categorial_feature=c('Gender','Area'),
                         free_raw_data = FALSE,
                         label=freq_train$freqv,
                         weight=freq_train$Exposure)


freq_Xtest=data.matrix(freq_test[,c('Gender','Area','RiskClass','BonusClass',
                                    'OwnerAge', 'VehAge')])


freq_XYtest=lgb.Dataset.create.valid(freq_XYtrain, freq_Xtest,
                                     label=freq_test$freqv, weight=freq_test$Exposure)


#light gradient boosting machine구동

freq_model=lgb.train(
  params=list(
    num_leaves=4L,
    min_data_in_leaf=20L,
    num_trees=100L,
    learning_rate=0.1,
    subsample_freq=1L,
    subsample=0.5,
    categorial_feature=c('Gender','Area'),
    objective='poisson'),
  data=freq_XYtrain,
  colnames=c('Gender','Area','RiskClass', 'BonusClass',
             'OwnerAge', 'VehAge'),
  valids=list(test=freq_XYtest),
  verbose=-1L)


lgb.get.eval.result(booster=freq_model,
                    data_name='test', eval_name='poisson', iters=c(1,100))

imp_mo_freq=lgb.importance(freq_model)
lgb.plot.importance(imp_mo_freq,top_n=Inf)




freq_model$best_iter

freq_model$best_score

# 예측값
pred_y=predict(freq_model,freq_Xtest)
pred_y

###스코어
postResample(freq_test$freqv,pred_y)
(mae=mean((freq_test$freqv-pred_y)^2))
(mae=caret::MAE(freq_test$freqv,pred_y))
(RMSE=caret::RMSE(freq_test$freqv,pred_y))




lgb.interprete(freq_model, freq_Xtest, 1L:5L)

imp=lgb.importance(freq_model,percentage=FALSE)
imp



# normalized quantities
# imp=lgb.importance(freq_model,percentage=TRUE)
# imp

lgb.get.eval.result(booster=freq_model,
                    data_name='test', eval_name='poisson', iters=c(1,100))

#cross validation

freq_modelcv=lgb.cv(
  params=list(
    num_leaves=4L,
    min_data_in_leaf=20L,
    num_trees=100L,
    learning_rate=0.1,
    bagging_freq=1L,
    bagging=0.5,
    objective='gamma'),
  data=freq_Xtrain,
  label=freq_trainIndex,
  verbose=-1L)

#cross validation scoring
cvscore=function(cvb){
  bstrs=cvb$boosters
  N=length(bstrs)
  loss=numeric(0)
  for (i in 1:N) {
    boost=bstrs[[i]]$booster
    eval=boost$eval_valid()
    loss=c(loss,eval[[1]]$value)
  }
  mean(loss)
}
library(ggplot2)
library(randomForest)
library(SHAPforxgboost)

x <- c("Age","Gender","Area","RiskClass",
       "VehAge","BonusClass")
X <- data.matrix(mccasefreq[sample(nrow(mccasefreq), 1000), colnames(x)])

shap <- shap.prep(freq_model, X_train = freq_Xtrain)
shap.plot.summary(shap)

shap.importance(shap,names_only = 1)

for (v in shap.importance(shap, names_only = TRUE)) {
  p <- shap.plot.dependence(shap, v,
                            color_feature = "auto",
                            alpha = 0.5, jitter_width = 0.1) +
    ggtitle(v)
  print(p)
}




lgb.plot.importance(imp)


install.packages("partialPlot")

library(partialPlot)

partialPlot(obj=freq_model, pred.data=freq_Xtrain,
            xname='Age')


cvscore(freq_modelcv)


lgb.get.eval.result(booster=freq_model,
                    data_name='test', eval_name='poisson', iters=c(1,100))

# Get record evaluation result from booster

lgb.importance(freq_model)

# Compute feature importance in a model



# Compute feature contribution of prediction

imp_mo_freq=lgb.importance(freq_model)

lgb.plot.importance(imp_mo_freq, top_n=Inf, measure='Gain',cex=NULL)

# Plot feature importance as a bar graph

lgb.plot.interpretation(imp_mo_freq,
                        top_n=Inf, cols=1L,left_margin=10L,cex='h')

# Plot feature contribution as a bar graph

# install.packages("e1071")

library(e1071)
# install.packages("SHAPforxgboost")


# install.packages("randomForest")
# install.packages('vctrs')


#샤플리부스트 완료
#gam




