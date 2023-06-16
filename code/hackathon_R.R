

columns=c(OwnerAge=2L,Gender=1L,Area=1L,RiskClass=1L, VehAge=2L,  
          BonusClass=1L,  Exposure=8L,  ClaimNb=4L, ClaimAmount=8L)

column.classes <- c("integer", rep("factor", 3), "integer",
                    "factor", "numeric", rep("integer", 2))
stopifnot(length(columns)==length(column.classes))

mccase <- read.fwf('./mccase.txt', widths = columns, header = FALSE,
                                   col.names = names(columns),
                                   colClasses = column.classes,
                                   na.strings = NULL, comment.char = "")
# Warning message:
#   In readLines(file, n = thisblock) :
#   incomplete final line found on './mccase.txt'
# csv파일로 import 할경우 칼럼 클래스를 재지정해야 한다 시간이 없으므로
#  txt로 다운받아서 진행
#에러메시지인데 별 문제 없어보인다
#다만 성별은 스웨덴어로 되어있다 남자 M 여자 K

# rm(columns, column.classes)
#메모리 관리용으로  안쓰는 변수 지운듯 




mccase$RiskClass=ordered(mccase$RiskClass)
mccase$BonusClass=ordered(mccase$BonusClass)
##순서범주화


##팩터 레이팅

mccase$rating.1 <- mccase$Area
mccase$rating.2 <- mccase$RiskClass
mccase$rating.3 <-
  cut(mccase$VehAge, breaks = c(0, 1, 4, 99),
      labels = as.character(1:3), include.lowest = TRUE,
      ordered_result = TRUE)
mccase$rating.4 <- ordered(mccase$BonusClass) # Drop comments
levels(mccase$rating.4) <-                 # Combine levels
  c("1", "1", "2", "2", rep("3", 3))
#안전운전 점수 범주화## Conver to data.table


##포아송분포를 따르는지 대략적으로 분산/평균 비율 계산

vY=mccase$ClaimNb
vE=mccase$Exposure
m=sum(vY)/sum(vE)
v=sum((vY-m*vE)^2)/sum(vE)
cat("average=",m,"variance=",v,"phi=",v/m,"\n")



vX_1=as.factor(mccase$rating.1)
for(i in 1:length(levels(vX_1))){
  vEi=vE[vX_1==levels(vX_1)[i]]
  vYi=vY[vX_1==levels(vX_1)[i]]
  mi=sum(vYi)/sum(vEi)
  vi=sum((vYi-mi*vEi)^2)/sum(vEi)
  cat('average=', mi, 'variance=', vi, 'phi=',vi/mi,'\n')}

vX_2=as.factor(mccase$rating.2)
for(i in 1:length(levels(vX_2))){
  vEi=vE[vX_2==levels(vX_2)[i]]
  vYi=vY[vX_2==levels(vX_2)[i]]
  mi=sum(vYi)/sum(vEi)
  vi=sum((vYi-mi*vEi)^2)/sum(vEi)
  cat('average=', mi, 'variance=', vi, 'phi=',vi/mi,'\n')}

vX_3=as.factor(mccase$rating.3)
for(i in 1:length(levels(vX_3))){
  vEi=vE[vX_3==levels(vX_3)[i]]
  vYi=vY[vX_3==levels(vX_3)[i]]
  mi=sum(vYi)/sum(vEi)
  vi=sum((vYi-mi*vEi)^2)/sum(vEi)
  cat('average=', mi, 'variance=', vi, 'phi=',vi/mi,'\n')}

vX_4=as.factor(mccase$rating.4)
for(i in 1:length(levels(vX_4))){
  vEi=vE[vX_4==levels(vX_4)[i]]
  vYi=vY[vX_4==levels(vX_4)[i]]
  mi=sum(vYi)/sum(vEi)
  vi=sum((vYi-mi*vEi)^2)/sum(vEi)
  cat('average=', mi, 'variance=', vi, 'phi=',vi/mi,'\n')}




# 문제1
# 현재 관세의 셀에 데이터를 집계합니다. 
# 이 수준에서 경험적 청구 빈도와 심각도를 계산하십시오.


library("data.table")
mccase <- data.table(mccase, key = paste("rating", 1:4, sep = "."))

mccase_freq = mccase[mccase$Exposure>0,]


reg=glm(ClaimNb~+rating.1+rating.2+rating.3+rating.4
        +offset(log(Exposure)), family=poisson,
        data=mccase_freq)
summary(reg)

## Aggregate to levels of current rating factors
#rating으로 그룹바이
mccase.current <-
  mccase[,
         list(Exposure = sum(Exposure),
              ClaimNb = sum(ClaimNb),
              ClaimAmount = sum(ClaimAmount),
              num.policies = .N),
         by = key(mccase)]             

## Claim frequency and severity. Change NaN to NA.
#  severity:중대도 - 손실 또는 손실로 인해 (또는 발생할 수 있는) 손해의
# 중대도 비율로 수량화되기도 합니다.
# 특정 기간 동안 손실에 노출된 값에 대한 손실액.
mccase.current$claim.freq <-
  with(mccase.current, ifelse(Exposure != 0, ClaimNb / Exposure, NA_real_))
mccase.current$severity <-
  with(mccase.current, ifelse(ClaimNb != 0, ClaimAmount / ClaimNb, NA_real_))

sum(is.na(mccase.current$claim.freq))

#문제 2: Determine how the Exposure and number of claims is distributed
# Determine how the duration and number of claims is distributed for each 
# of the rating factor classes, as an indication of the accuracy of the statistical analysis.
# 통계 분석의 정확성을 나타내는 지표로서,
# 청구 기간과 청구 횟수가 각 등급별로 어떻게 분배되는지 결정한다.


##GLM ,  클래임 건수는 포아송 분포를 따른다
data <- mccase[order(ClaimNb), list(N = .N, w = sum(Exposure)), by = ClaimNb]
M <- glm(N ~ ClaimNb, family = poisson(), weights = w, data = data)
data$predicted <- round(predict(M, data[, list(ClaimNb)], type = "response"))
print(data[, list(ClaimNb, N, predicted)], digits = 1)
summary(M)
# Call:
#   glm(formula = N ~ ClaimNb, family = poisson(), data = data, weights = w)
# 
# Deviance Residuals: 
#   1       2       3  
# 0.024  -3.828  51.556  
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)  1.106e+01  1.562e-05  708454   <2e-16 ***
#   ClaimNb     -4.594e+00  1.253e-03   -3668   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for poisson family taken to be 1)
# 
# Null deviance: 1.2699e+08  on 2  degrees of freedom
# Residual deviance: 2.6727e+03  on 1  degrees of freedom
# AIC: 839300
# 
# Number of Fisher Scoring iterations: 4


#  예측결과
# ClaimNb     N predicted
# 1:       0 63878     63878
# 2:       1   643       646
# 3:       2    27         7
#결과는 나쁘지 않지만 ClaimNb의 첫번째 값에 무거운 가중치를 둘 필요가 없다
#첫번째 등급 요소의 수준별로 동일한 분할을 표시할수 있다.


data <-
  mccase[order(rating.1, ClaimNb),
         list(N = .N, w = sum(Exposure)),
         by = list(rating.1, ClaimNb)]


modelPoisson <- function(data) {
  M <- glm(N ~ ClaimNb, family = poisson(), weights = w, data = data)
  return(M)
}

predictionPoisson <- function (data) {
  M <- modelPoisson(data)
  p <- predict(M, data[, list(ClaimNb)], type = "response")
  return(p)
}

data$predicted <-
  unlist(lapply(levels(data$rating.1),
                function (l) round(predictionPoisson(data[rating.1 == l]))))

print(data[, list(rating.1, ClaimNb, N, predicted)], digits = 1)


sum(is.na(mccase.current))




data <-
  mccase.current[order(rating.3, ClaimNb),
                   list(N = .N, w = sum(Exposure)),
                 by = list(rating.3, ClaimNb)]
data$predicted <-
  unlist(lapply(levels(data$rating.3),
                function (l) round(predictionPoisson(data[rating.3 == l]))))

## Show the fit

print(data[ClaimNb <= max(ClaimNb),
           list(res = sum(abs(predicted - N))),
           by = rating.3])

## Show simplistic residuals per level of the rating factor
print(data[ClaimNb <= max(ClaimNb),
           list(res = sum(abs(predicted - N))),
           by = rating.3])
## rating.4 is the bonus class
data <-
  mccase.current[order(rating.4, ClaimNb),
                   list(N = .N, w = sum(Exposure)),
                 by = list(rating.4, ClaimNb)]
data$predicted <-
  unlist(lapply(levels(data$rating.4),
                function (l) round(predictionPoisson(data[rating.4 == l]))))
## Show the fit
print(data[ClaimNb <= max(ClaimNb),
           list(rating.4, ClaimNb, N, predicted)], digits = 1)
## Show simplistic residuals per rating factor
print(data[ClaimNb <= max(ClaimNb),
           list(res = sum(abs(predicted - N))),
           by = rating.4])

 
# # problem3 determine the relativities for claim frequency and severity
# Determine the relativities for claim frequency and severity separately, 
# by using GLMs; use the result to get relativities for the pure premium.
# GLM을 사용하여 클레임 빈도와 심각도에 대한 상대성을 개별적으로 결정합니다.
# 결과를 사용하여 순수 프리미엄에 대한 상대성을 얻습니다.


library("data.table")

case.2.4 <-
  data.frame(rating.factor =
               c(rep("Area",        nlevels(mccase.current$rating.1)),
                 rep("RiskClass",    nlevels(mccase.current$rating.2)),
                 rep("VehAge", nlevels(mccase.current$rating.3)),
                 rep("BonusClass", nlevels(mccase.current$rating.4))),
             class =
               with(mccase.current,
                    c(levels(rating.1), levels(rating.2),
                      levels(rating.3), levels(rating.4))),
             ## These are the values from Table 2.8 in the book:
             relativity =
               c(7.678, 4.227, 1.336, 1.000, 1.734, 1.402, 1.402,
                 0.625, 0.769, 1.000, 1.406, 1.875, 4.062, 6.873,
                 2.000, 1.200, 1.000,
                 1.250, 1.125, 1.000),
             stringsAsFactors = FALSE)
print(case.2.4, digits = 3)

#realtivity 적용 완료


library("foreach")
new.cols <-
  foreach (rating.factor = paste("rating", 1:4, sep = "."),
           .combine = rbind) %do%
  {
    totals <- mccase.current[, list(E = sum(Exposure),
                                    N = sum(ClaimNb),
                                    C = sum(ClaimAmount)),
                             by = rating.factor]
    n.levels <- nlevels(mccase.current[[rating.factor]])
    contrasts(mccase.current[[rating.factor]]) <-
      contr.treatment(n.levels)[rank(-totals[["E"]], ties.method = "first"), ]
    data.frame(Exposure = totals[["E"]],
               n.claims = totals[["N"]],
               ClaimAmount = totals[["C"]])
  }
case.2.4 <- cbind(case.2.4, new.cols)


rm(new.cols)

## Model the frequency

model.frequency <-
  glm(ClaimNb ~
        rating.1 + rating.2 + rating.3 + rating.4 + offset(log(Exposure)),
      data = mccase.current[Exposure > 0], family = poisson)


summary(model.frequency)


rels <- coef( model.frequency )
rels <- exp( rels[1] + rels[-1] ) / exp( rels[1] )

coef(model.frequency)

case.2.4$rels.frequency <-
  c(c(1, rels[1:6])[rank(-case.2.4$Exposure[1:7], ties.method = "first")],
    c(1, rels[7:12])[rank(-case.2.4$Exposure[8:14], ties.method = "first")],
    c(1, rels[13:14])[rank(-case.2.4$Exposure[15:17], ties.method = "first")],
    c(1, rels[15:16])[rank(-case.2.4$Exposure[18:20], ties.method = "first")])

## Model the severity. We stick with the non-canonical link function
## for the time being.





model.severity <-
  glm(ClaimAmount ~ rating.1 + rating.2 + rating.3 + rating.4,
      data = mccase.current[ClaimAmount > 0,],
      family = Gamma("log"), weights = ClaimNb)


summary(model.severity)
## Res.dev. 516 on 164 dof

rels <- coef( model.severity )

rels <- exp( rels[1] + rels[-1] ) / exp( rels[1] )


case.2.4$rels.severity <-
  c(c(1, rels[1:6])[rank(-case.2.4$Exposure[1:7], ties.method = "first")],
    c(1, rels[7:12])[rank(-case.2.4$Exposure[8:14], ties.method = "first")],
    c(1, rels[13:14])[rank(-case.2.4$Exposure[15:17], ties.method = "first")],
    c(1, rels[15:16])[rank(-case.2.4$Exposure[18:20], ties.method = "first")])
# 요율산정


## Combine the frequency and severity
case.2.4$rels.pure.prem <- with(case.2.4, rels.frequency * rels.severity)
print(case.2.4)

## Convert to data.table
# library("data.table")
case.2.4 <- data.table(case.2.4)
# 


# ## Compare with current values
print(case.2.4[,list(rating.factor, class, Exposure, n.claims,
                    ClaimAmount = round(ClaimAmount / 1e3),
                    relativity, rels.pure.prem)],
      digits = 3)



## Combine the frequency and severity



# # problem4
# Problem 4: Compare the results in 3 to the current tariff. Is there a need to change 
# the tariff? Which new conclusions can be drawn from the separate analysis in 3? 
#   Can we trust these estimates? With your estimates, what is the ratio between the 
# # highest pure premium and the lowest?
# 문제 4: 3의 결과를 현재 관세와 비교합니다. 변경할 필요가 있습니까
# 관세? 3의 별도 분석에서 어떤 새로운 결론을 도출할 수 있는가?
#   우리가 이 추정치들을 믿을 수 있을까요? 당신의 추정치로, 사이의 비율은 얼마입니까
# 가장 높은 순도의 보험료와 가장 낮은 보험료?


with(case.2.4, max(rels.pure.prem) / min(rels.pure.prem)) # 3552
with(case.2.4, max(relativity) / min(relativity))         #   12


#아무튼 코드 번역 끝 

## by tweedie
library(tweedie)
library(statmod)


# 
# out=tweedie.profile(ClaimAmount~rating.1+rating.2+rating.3+rating.4,
#                     data=mccase[ClaimAmount>0], p.vec=seq(1.05,2.5,by=.05))
out$p.max
plot(out,type='b')
abline(v=out$p.max, lty=2)


regtweedie=  glm(ClaimAmount ~ rating.1 + rating.2 + rating.3 + rating.4,
                 data = mccase[ClaimAmount > 0,],
                 family = tweedie(var.power=2, link.power=0), weights = ClaimNb)

# summary(regtweedie)









