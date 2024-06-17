#FINAL R
#LOGISTIC REGRESSION
library(tidyverse)
library(GGally)
library(texreg)
library(gridExtra)
library(effects)
library(glue)

LowBirth <- as.data.frame(read.csv("LB1.csv", header = TRUE, sep = ","))
LowBirth
class(LowBirth)
names(LowBirth)
summary(LowBirth)

tibble::glimpse(LowBirth)
LowBirthClean <- LowBirth %>% 
  dplyr::mutate(ID = factor(ID)) %>% 
  dplyr::mutate(LOW = LOW %>% 
                  factor() %>% 
                  forcats::fct_recode("birth weight >2500 g (normal)" = "0",
                                      "birth weight < 2500 g (low)"   = "1")) %>% 
  dplyr::mutate(RACE = RACE %>% 
                  factor() %>% 
                  forcats::fct_recode("White" = "1",
                                      "Black" = "2",
                                      "Other" = "3")) %>% 
  dplyr::mutate(PTL = as.numeric(PTL > 0)) %>%         
  dplyr::mutate(PTL = factor(PTL)) %>%                    
  dplyr::mutate_at(vars(SMOKE, HT, UI, PTL),           
                   factor,
                   levels = 0:1,
                   labels = c("No", "Yes"))

tibble::glimpse(LowBirthClean)

low.age   <- glm(LOW ~ AGE,     family = binomial(link = "logit"), data = LowBirthClean)
low.lwt   <- glm(LOW ~ LWT,     family = binomial(link = "logit"), data = LowBirthClean)
low.race  <- glm(LOW ~ RACE,    family = binomial(link = "logit"), data = LowBirthClean)
low.smoke <- glm(LOW ~ SMOKE,   family = binomial(link = "logit"), data = LowBirthClean)
low.ptl   <- glm(LOW ~ PTL,     family = binomial(link = "logit"), data = LowBirthClean)
low.ht    <- glm(LOW ~ HT,      family = binomial(link = "logit"), data = LowBirthClean)
low.ui    <- glm(LOW ~ UI,      family = binomial(link = "logit"), data = LowBirthClean)
low.ftv   <- glm(LOW ~ FTV,     family = binomial(link = "logit"), data = LowBirthClean)
low.bwt   <- glm(LOW ~ BWT,     family = binomial(link = "logit"), data = LowBirthClean)
summary(low.age)
coef(low.age)

ggscatmat(select(LowBirthClean, -AGE))
ggplot(LowBirthClean,aes(x=seq_along(AGE),y=LWT)) + geom_point()

glm.probs=predict(low.age, type = "response")
glm.probs[1:10]


low_1 <- glm(LOW ~ AGE + LWT + RACE + SMOKE + PTL + HT + UI,
             family = binomial(link = "logit"), 
             data = LowBirthClean)

summary(low_1)
coef(low_1)

low_2 <- glm(LOW ~ AGE + LWT + RACE + SMOKE + PTL + HT + UI + AGE:LWT,
             family = binomial(link = "logit"), 
             data = LowBirthClean)

summary(low_2)
anova(low_1, low_2, test = 'LRT')
coef(low_2)

low_3 <- glm(LOW ~ AGE + LWT + RACE + SMOKE + PTL + HT + UI + AGE:SMOKE,
             family = binomial(link = "logit"), 
             data = LowBirthClean)
summary(low_3)
anova(low_1, low_3, test = 'LRT')
coef(low_3)

low_4 <- glm(LOW ~ AGE + LWT + RACE + SMOKE + PTL + HT + UI + LWT:SMOKE,
             family = binomial(link = "logit"), 
             data = LowBirthClean)
summary(low_4)
anova(low_1, low_4, test = 'LRT')
coef(low_4)

performance::compare_performance(low_1, low_3, rank = TRUE)

low_5 <- glm(LOW ~ AGE + LWT + RACE + SMOKE + PTL + HT,
             family = binomial(link = "logit"), 
             data = LowBirthClean)

summary(low_5)
coef(low_5)

low_6 <- glm(LOW ~ I((AGE - 20)/5) + I((LWT - 125)/20) + RACE + SMOKE + PTL + HT,
             family = binomial(link = "logit"), 
             data = LowBirthClean)

summary(low_6)
coef(low_6)

texreg::knitreg(low_6,
                single.row = TRUE,
                digits = 3)

texreg::knitreg(extract(low_6),
                custom.coef.names = c("Body Weight: 125 lb, 20 yr old White Mother",
                                      "Additional 5 years older",
                                      "Additional 20 lbs pre-pregnancy",
                                      "Race: Black vs. White",
                                      "Race: Other vs. White",
                                      "Smoking During pregnancy",
                                      "History of Any Premature Labor",
                                      "History of Hypertension"),
                custom.model.names = "OR, Low Birth Weight",
                single.row = TRUE,
                ci.test = 1)

glm.probs=predict(low_5, type="response")
glm.probs[1:10]

effects::Effect(focal.predictors = c("LWT", "SMOKE", "PTL", "HT"),
                fixed.predictors = list(age = 20),
                mod = low_6,
                xlevels = list(lwt = seq(from = 80, to = 250, by = 5))) %>% 
  data.frame() %>% 
  dplyr::mutate(SMOKE = forcats::fct_rev(SMOKE)) %>% 
  dplyr::mutate(ptl_any_labels = glue("Record of Premature Labor: {PTL}")) %>% 
  dplyr::mutate(ht_labels = glue("Record of Hypertension: {HT}") %>% forcats::fct_rev()) %>% 
  ggplot(aes(x = LWT,
             y = fit)) +
  geom_line(aes(color = SMOKE,
                linetype = SMOKE),
            size = 1) +
  theme_bw() +
  facet_grid(ht_labels ~ ptl_any_labels) +
  labs(title = "Risk of Low Birth Weight",
       subtitle = "Illustates risk given the mother is 20 years old and white",
       x = "Mother's Weight Pre-Pregnancy, pounds",
       y = "Predicted Probability\nBaby has Low Birth Weight (< 2500 grams)",
       color    = "Mother Smoked",
       linetype = "Mother Smoked") +
  theme(legend.position = c(1, .5),
        legend.justification = c(1.1, 1.15),
        legend.background = element_rect(color = "black"),
        legend.key.width = unit(1.5, "cm")) +
  scale_linetype_manual(values = c("solid", "dashed")) +
  scale_color_manual(values = c( "darkgreen", "yellow"))

ggplot(LowBirthClean, aes(x=AGE, y=LWT)) + 
  geom_point(alpha=.5) +
  stat_smooth(method="glm", se=FALSE, method.args = list(family=binomial))

Dat <- read.csv(
  file= "LB1.csv",
  header=T,na.strings = "NA", 
  dec = ".", 
  strip.white = TRUE)
head(Dat)
head(Dat)
glm.fit = glm(LOW~AGE+LWT+RACE+SMOKE+PTL+HT+UI+FTV,data=Dat,family=binomial)
summary(glm.fit)

summary(glm.fit)$coef

glm.probs=predict(glm.fit,type="response")

dim(Dat)

Dat$LOW <- as.factor(Dat$LOW)
contrasts(Dat$LOW)
glm.pred=rep("0",189)
glm.pred[glm.probs>.5] <- "1"


table(glm.pred,Dat$LOW)

mean(glm.probs != Dat$LOW)

#BAGGED
#LOW
idata <- read.csv(file = "LB1.csv",as.is=TRUE, header=TRUE)
idata
set.seed(1)
train = sample(1:nrow(idata), nrow(idata)/2)
library(randomForest)
set.seed(123)
# Ntree 200 with mtry 6
bag.idata=randomForest(LOW~.,data=idata,subset=train,
                       mtry=6,importance=TRUE,ntree = 200)
bag.idata
library(tidyverse)
set.seed(123)
# additional method 
idata <- dplyr::select(idata,LOW,everything())
bag.idata=randomForest(x=idata[train,-1],
                       y=idata[train,1],
                       data=idata,
                       mtry=6,importance=TRUE,ntree = 200)
bag.idata
idata.test=idata[-train,"LOW"]
yhat.bag = predict(bag.idata,newdata=idata[-train,])
mean((yhat.bag-idata.test)^2)
ggplot(data.frame(yhat.bag, idata.test), aes(x=yhat.bag ,y=idata.test)) +
  geom_point() +
  geom_abline(slope=1,intercept=0) +
  labs(x="Predicted", 
       y="Test set",
       title="Predicted VS Test")

#BWT
set.seed(1)
train = sample(1:nrow(idata), nrow(idata)/2)
library(randomForest)
set.seed(123)
bag.idata=randomForest(BWT~.,data=idata,subset=train,
                       mtry=6,importance=TRUE,ntree = 200)
bag.idata
library(tidyverse)
set.seed(123)
idata <- dplyr::select(idata,BWT,everything())
bag.idata=randomForest(x=idata[train,-1],
                       y=idata[train,1],
                       data=idata,
                       mtry=6,importance=TRUE,ntree = 200)
bag.idata
idata.test=idata[-train,"BWT"]
yhat.bag = predict(bag.idata,newdata=idata[-train,])
mean((yhat.bag-idata.test)^2)
ggplot(data.frame(yhat.bag, idata.test), aes(x=yhat.bag ,y=idata.test)) +
  geom_point() +
  geom_abline(slope=1,intercept=0) +
  labs(x="Predicted", 
       y="Test set",
       title="Predicted VS Test")

#RANDOM FOREST
Dat2 <- as.data.frame(read.csv(
  file= "LB1.csv",
  header=T,na.strings = "NA", 
  dec = ".", 
  strip.white = TRUE))

Dat2$LOW <- as.factor(Dat2$LOW)

str(Dat)
library(randomForest)
library(ggplot2)
library(GGally)
# scatter plot matrix 
ggpairs(Dat, title="scatter plot matrix  with ggpairs()",axisLabels="none") 

Dat %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram()
#dividing the training and testing data 
set.seed(1)
train = sample(1:nrow(Dat), nrow(Dat)/2)
dim(train)

length(Dat)
test <-  Dat[-train]
Low.test=Dat[-train,"LOW"]
# ntrees 500 with LOW 
rf.data= randomForest(formula = LOW~.,data=Dat,subset=train, mty=13, ntree = 500 )
rf.data

plot(rf.data)
yhat.Pre = predict(rf.data,newdata=Dat[-train,],type = "Class")
mean((yhat.Pre == Low.test )^2)

# ntrees 500 with BWT
Low.test2=Dat[-train,"BWT"]
rf.data= randomForest(formula = BWT~.,data=Dat,subset=train, mty=13, ntree = 500 )
rf.data

plot(rf.data)
yhat.Pre = predict(rf.data,newdata=Dat[-train,],type = "Class")
mean((yhat.Pre == Low.test2 )^2)

library(randomForest)
set.seed(123)
# viewing very impotent variable
importance(rf.data)
varImpPlot(rf.data)

#find RMSE of best model
sqrt(rf.data$mse[which.min(rf.data$mse)]) 

# Ploting Top 4 Variables 
library(tidyverse)
set.seed(123)
Low.test=Dat[-train,"LOW"]
Low <- dplyr::select(Dat,LOW,LWT,AGE,PTL,Smoke)
rf.Dat2 <- randomForest(x=Dat[train,-1],
                        y=Dat[train,1],
                        data=Low,
                        importance=TRUE,
                        ntree = 500)
rf.Dat2
yhat.rf = predict(rf.Dat2,newdata=Dat[-train,])
mean((yhat.rf - Low.test )[-train]^2)

#DECISION TREES
library(rpart)
Dat <- as.data.frame(read.csv(
  file= "LB1.csv",
  header=T,na.strings = "NA", 
  dec = ".", 
  strip.white = TRUE))
dim(Dat)
ggpairs(dplyr::select(Dat,LOW,everything()))
set.seed(1)
train = sample(1:nrow(Dat), nrow(Dat)/2)
# Tanking the Initial Cp value for testing 
set.seed(543)
rpart.LowBirth=rpart(BWT~.,data = Dat[train,],
                     method="anova",
                     cp=0.000001)
#Piloting the X error
printcp(rpart.LowBirth)
plotcp(rpart.LowBirth)

# Piloting min of Cp= 0.027 for better min error  
library(rpart.plot)
rpart.plot.1se <- prune(rpart.LowBirth,cp=0.027)
rpart.plot(rpart.plot.1se , extra=1,
           roundint=FALSE, digits=3, main="LowBirth regression tree")
printcp(rpart.LowBirth.prune)
#   Predication , MSE and RMSE
yhat=predict(rpart.LowBirth.prune , newdata=Dat[-train,])
LowBirth.test=Dat[-train,"BWT"]
(MSE = mean((yhat-LowBirth.test)^2))
sqrt(MSE) 
# GGplot to see the correlation 
library(ggplot2)
ggplot(data.frame(yhat, LowBirth.test), aes(x=yhat ,y=LowBirth.test)) +
  geom_point() +
  geom_abline(slope=1,intercept=0) +
  labs(x="predicted LOW", 
       y="test-set LOW",
       title="regression tree")


