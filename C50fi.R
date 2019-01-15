View(c)
library(caret)
library(C50)

rdm<-createDataPartition(c$los,p=0.55,list = FALSE)
View(rdm)
tr<-c[rdm,]
ts<-c[-rdm,]
mod<-C5.0(los~.,data=tr)
plot(mod)

summary(mod)


confusionMatrix(predict(mod,newdata = tr),tr$los)


pred_mod<-predict(mod,ts)
plot(pred_mod)


confusionMatrix(predict(mod,newdata = ts),ts$los)

summary(pred_mod)
