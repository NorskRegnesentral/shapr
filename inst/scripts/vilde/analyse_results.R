library(data.table)
library(ggplot2)
library(ggpubr)
theme_set(theme_pubr())

resultsVilde <- fread("inst/scripts/vilde/results_parallel.csv")
resultsVilde2 <- fread("inst/scripts/vilde/results_independence_gaussian.csv")
resultsMartin <- fread("inst/scripts/vilde/results_parallel_martin.csv")
resultsMartin2 <- fread("inst/scripts/vilde/results_parallel_MJ.csv")

results <- rbind(resultsVilde,resultsMartin,resultsMartin2)
#results <- resultsMartin2

cols<- c("n_vars",
         "n_test",
         "n_train",
         "correlation",
         "n_batches",
         "n_cores")

scaled_cols <- paste0(cols,"_scaled")
results[, (scaled_cols) := lapply(.SD, scale), .SDcols=cols] #normalize data
results[,(paste0(scaled_cols, "2")):= lapply(.SD, function(x) x ^ 2),
         .SDcols=scaled_cols]
results[,(paste0(scaled_cols, "3")):= lapply(.SD, function(x) x ^ 3),
        .SDcols=scaled_cols]

#find eq. for independence
results1 <- results[approach == "independence",]
model1 <- lm(time~n_vars_scaled*n_cores_scaled*n_batches_scaled+
               n_train_scaled*n_vars_scaled+
               n_vars_scaled*n_test_scaled*n_cores_scaled*computer_name+
               n_vars_scaled*n_test_scaled*n_batches_scaled+
               n_vars_scaled2+n_batches_scaled2+n_cores_scaled2, data=results1)
summary(model1)
plot(y=predict(model1), x=results1[["time"]],
     ylab='Predicted Values',
     xlab='Actual Values',
     main='Predicted vs. Actual Values',log="")
abline(a=0, b=1)

ggplot(results1, aes(x=n_vars, y=time, col=factor(model)))+geom_point()
ggplot(results1, aes(x=n_test, y=time, col=factor(n_train)))+geom_point()
ggplot(results1, aes(x=n_train, y=time, col=factor(computer_name)))+geom_point()
ggplot(results1, aes(x=n_batches, y=time, col=n_cores))+geom_point()
ggplot(results1, aes(x=n_cores, y=time, col=factor(computer_name)))+geom_point()
ggplot(results1, aes(x=correlation, y=time, col=factor(computer_name)))+geom_point()

#find eq. for empirical
results2 <- results[approach == "empirical",]

ggplot(results2[time>60,], aes(x=n_vars, y=time, col=factor(model)))+geom_point()
ggplot(results2[time>60,], aes(x=n_test, y=time, col=factor(model)))+geom_point()
ggplot(results2[time>60,], aes(x=n_train, y=time, col=factor(computer_name)))+geom_point()
ggplot(results2[time>60,], aes(x=n_batches, y=time, col=n_cores))+geom_point()
ggplot(results2[time>60,], aes(x=n_cores, y=time, col=factor(computer_name)))+geom_point()

#samme antall features, n_train og n_test, voksende med n_batches

model2 <- lm(log(time)~n_vars_scaled+n_cores_scaled+n_batches_scaled+
               n_vars_scaled2+computer_name+
               n_train_scaled+n_test_scaled+
               +n_cores_scaled3+n_cores_scaled3+
               n_vars_scaled2*n_train_scaled+
               n_vars_scaled2*n_test_scaled+
               n_vars_scaled*n_train_scaled+
               n_vars_scaled*n_test_scaled+
               n_vars_scaled*n_batches_scaled+
               n_vars_scaled*n_cores_scaled2+
               n_vars_scaled*n_cores_scaled+
               n_vars_scaled*n_cores_scaled3+
               n_vars_scaled*computer_name+
               n_train_scaled*n_cores_scaled+
               n_train_scaled*n_cores_scaled2+
               n_train_scaled*n_cores_scaled3+
               n_train_scaled*n_batches_scaled+
               n_train_scaled*n_test_scaled+
               n_train_scaled*computer_name+
               n_test_scaled*n_cores_scaled+
               n_test_scaled*n_cores_scaled2+
               n_test_scaled*n_cores_scaled3+
               n_train_scaled*n_cores_scaled*n_vars_scaled+
               n_vars_scaled2*computer_name*n_cores_scaled
               , data=results2)
summary(model2)
plot(y=exp(predict(model2)), x=results2[["time"]],
     ylab='Predicted Values',
     xlab='Actual Values',
     main='Predicted vs. Actual Values',log="")
abline(a=0, b=1)

results2[,pred_time:=predict(model2)]
resultsVilde[approach=="empirical",pred_time:=predict(model2, newdata=resultsVilde[approach=="empirical",])]
results2[,diff:=abs(time-pred_time)]
results2[diff>10,]

ggplot(results2[pred_time>35,], aes(x=n_batches, y=time, col=factor(n_train)))+geom_point()
ggplot(results2[pred_time<35,], aes(x=n_vars, y=time, col=factor(n_train)))+geom_point()
ggplot(results2[pred_time>35,], aes(x=n_test, y=time, col=factor(n_train)))+geom_point()
ggplot(results2[pred_time<35,], aes(x=n_test, y=time, col=factor(n_train)))+geom_point()

ggplot(results2[pred_time>35,], aes(x=n_train, y=time, col=factor(computer_name)))+geom_point()
ggplot(results2[pred_time>35,], aes(x=n_batches, y=time, col=n_cores))+geom_point()
ggplot(results2[pred_time>35,], aes(x=n_cores, y=time, col=factor(computer_name)))+geom_point()
ggplot(results2[pred_time>35,], aes(x=correlation, y=time, col=factor(computer_name)))+geom_point()

p1 <- ggplot(results2, aes(x=n_vars,pred_time))+geom_point()
p2 <- ggplot(results2, aes(x=n_vars,time))+geom_point()
ggarrange(p1,p2,align = 'v')

# find eq. for copula
results3 <- results[approach == "copula",]

ggplot(results3, aes(x=n_vars, y=time, col=factor(n_test)))+geom_point()+xlab("num. features")
ggplot(results3, aes(x=n_test, y=time, col=factor(n_train)))+geom_point()
ggplot(results3, aes(x=n_train, y=time, col=factor(computer_name)))+geom_point()
ggplot(results3, aes(x=n_cores, y=time, col=factor(computer_name)))+geom_point()
ggplot(results3, aes(x=n_batches, y=time, col=factor(n_cores)))+geom_point()

ggplot(results3[time>250,], aes(x=n_vars, y=time, col=factor(n_train)))+geom_point()+xlab("num. features")
ggplot(results3[time>250,], aes(x=n_test, y=time, col=factor(n_train)))+geom_point()
ggplot(results3[time>250,], aes(x=n_train, y=time, col=factor(computer_name)))+geom_point()
ggplot(results3[time>250,], aes(x=n_cores, y=time, col=factor(computer_name)))+geom_point()
ggplot(results3[time>250,], aes(x=n_batches, y=time, col=factor(n_cores)))+geom_point()
#samme antall features og n_test, to "grupper" basert på n_train, martins computer har tiden mere "konsentrert" enn min computer

model3 <- lm(log(time)~n_vars_scaled+n_cores_scaled+n_batches_scaled+n_test_scaled+n_train_scaled+computer_name+
               n_vars_scaled2+n_cores_scaled2+n_batches_scaled2+
               n_cores_scaled3+n_batches_scaled3+
               n_cores_scaled*n_batches_scaled*n_vars_scaled*n_test_scaled+
               n_batches_scaled2*n_cores_scaled2*n_vars_scaled2+
               n_vars_scaled*n_cores_scaled2+
               n_cores_scaled2*n_batches_scaled3, data=results3)
summary(model3)

plot(x=predict(model3), y=results3[["time"]],
     xlab='Predicted Values',
     ylab='ln(Actual Values)',
     main='Copula model: Predicted vs. Actual Values',log="")
abline(a=0, b=1)

plot(x=predict(model3), y=log(results3[["time"]]),
     xlab='Predicted Values',
     ylab='ln(Actual Values)',
     main='Copula model: Predicted vs. Actual Values',log="")
abline(a=0, b=1)

plot(x=exp(predict(model3)), y=results3[["time"]],
     xlab='exp(Predicted Values)',
     ylab='Actual Values',
     main='Copula model: Predicted vs. Actual Values',log="")
abline(a=0, b=1)

# gaussian
results4 <- results[approach == "gaussian",]
model4 <- lm(time~n_vars_scaled+n_cores_scaled+n_batches_scaled+n_train_scaled+n_test_scaled+n_vars_scaled2+
               n_batches_scaled2*n_cores_scaled2*n_cores_scaled3+computer_name+
               n_vars_scaled*n_batches_scaled*n_cores_scaled+
               n_vars_scaled*n_test_scaled*n_cores_scaled+
               n_vars_scaled*n_test_scaled*n_batches_scaled, data=results4)
summary(model4)
plot(y=predict(model4), x=results4[["time"]],
     ylab='Predicted Values',
     xlab='Actual Values',
     main='Predicted vs. Actual Values',log="")
abline(a=0, b=1)

ggplot(results4, aes(x=n_vars, y=time, col=factor(n_train)))+geom_point()+xlab("num. features")
ggplot(results4, aes(x=n_test, y=time, col=factor(n_train)))+geom_point()
ggplot(results4, aes(x=n_train, y=time, col=factor(n_vars)))+geom_point()
ggplot(results4, aes(x=n_cores, y=time, col=factor(computer_name)))+geom_point()
ggplot(results4, aes(x=n_batches, y=time, col=factor(n_cores)))+geom_point()

# plot pred v actual for all methods
p1<-ggplot(results1, aes(x=predict(model1), y=results1[["time"]]))+geom_point()+geom_abline()
p1<-p1+xlab("Predicted values")+ylab("Observed values")+ggtitle("Independence")
p2<-ggplot(results2, aes(x=exp(predict(model2)), y=results2[["time"]]))+geom_point()+geom_abline()
p2<-p2+xlab("exp(Predicted values)")+ylab("Observed values")+ggtitle("Empirical")
p3<-ggplot(results3, aes(x=exp(predict(model3)), y=results3[["time"]]))+geom_point()+geom_abline()
p3<-p3+xlab("exp(Predicted values)")+ylab("Observed values")+ggtitle("Copula")
p4<-ggplot(results4, aes(x=predict(model4), y=results4[["time"]]))+geom_point()+geom_abline()
p4<-p4+xlab("Predicted values")+ylab("Observed values")+ggtitle("Gaussian")

ggarrange(p1,p2,p3,p4,ncol=2,nrow=2)

# compare computers
martin <- results[computer_name=="Linuxjullum",]
vilde <- results[computer_name=="Windowsvilde" & n_cores==1,]
combine <- merge(vilde, martin,by=colnames(martin)[1:9])

plot(combine[,time.x],combine[,time.y],log='xy', xlab = "Vilde", ylab = "Martin")
abline(0,1)

