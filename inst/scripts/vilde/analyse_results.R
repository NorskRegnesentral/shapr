library(data.table)
library(ggplot2)
library(ggpubr)
theme_set(theme_pubr())

results <- fread("inst/scripts/vilde/results_parallel.csv")
results2 <- fread("inst/scripts/vilde/results_parallel_martin.csv")
results <- rbind(results,results2)

cols<- c("n_vars",
         "n_test",
         "n_train",
         "correlation",
         "n_batches",
         "n_cores")
results[, (cols) := lapply(.SD, scale), .SDcols=cols] #normalize data

#find eq. for independence and gaussian
results1 <- results[approach == "independence" | approach == "gaussian",]
model1 <- lm(time~n_vars*approach*n_test, data=results1)
summary(model)
plot(x=predict(model1), y=results1[["time"]],
     xlab='Predicted Values',
     ylab='Actual Values',
     main='Predicted vs. Actual Values',log="")
abline(a=0, b=1)

#find eq. for empirical
results2 <- results[approach == "empirical",]
results2[,(c("n_vars2","n_test2", "n_train2","n_batches2", "n_cores2")):= lapply(.SD, function(x) x ^ 2),
        .SDcols=c("n_vars", "n_test", "n_train", "n_batches", "n_cores")]

ggplot(results2[time>55,], aes(x=n_vars, y=time, col=factor(n_train)))+geom_point()
ggplot(results2[time>55,], aes(x=n_test, y=time, col=factor(n_train)))+geom_point()
ggplot(results2[time>55,], aes(x=n_train, y=time, col=factor(computer_name)))+geom_point()
ggplot(results2[time>55,], aes(x=n_batches, y=time, col=n_cores))+geom_point()
ggplot(results2[time>55,], aes(x=n_cores, y=time, col=factor(computer_name)))+geom_point()
ggplot(results2[time>55,], aes(x=correlation, y=time, col=factor(computer_name)))+geom_point()
#samme antall features, n_train og n_test, voksende med n_batches

ggplot(results2[time>25 & time < 38,], aes(x=n_vars, y=time, col=factor(n_train)))+geom_point()
ggplot(results2[time>25 & time < 38,], aes(x=n_test, y=time, col=factor(n_train)))+geom_point()
ggplot(results2[time>25 & time < 38,], aes(x=n_train, y=time, col=factor(computer_name)))+geom_point()
ggplot(results2[time>25 & time < 38,], aes(x=n_batches, y=time, col=n_cores))+geom_point()
ggplot(results2[time>25 & time < 38,], aes(x=n_cores, y=time, col=factor(computer_name)))+geom_point()
ggplot(results2[time>25 & time < 38,], aes(x=correlation, y=time, col=factor(computer_name)))+geom_point()
#samme antall features, n_train og n_test, men synkende sammenheng mellom n_cores og n_batches


model2 <- lm(time~n_vars*n_test*n_train+n_batches+n_cores+n_cores2, data=results2)
summary(model2)
plot(x=predict(model2), y=results2[["time"]],
     xlab='Predicted Values',
     ylab='Actual Values',
     main='Predicted vs. Actual Values',log="")
abline(a=0, b=1)


# find eq. for copula
results3 <- results[approach == "copula",]
results3[,(c("n_vars2","n_test2", "n_train2","n_batches2", "n_cores2")):= lapply(.SD, function(x) x ^ 2),
         .SDcols=c("n_vars", "n_test", "n_train", "n_batches", "n_cores")]

ggplot(results3, aes(x=n_vars, y=time, col=factor(n_train)))+geom_point()+xlab("num. features")
ggplot(results3, aes(x=n_test, y=time, col=factor(computer_name)))+geom_point()
ggplot(results3, aes(x=n_train, y=time, col=factor(computer_name)))+geom_point()
ggplot(results3, aes(x=n_cores, y=time, col=factor(computer_name)))+geom_point()
ggplot(results3, aes(x=n_batches, y=time, col=factor(n_cores)))+geom_point()

ggplot(results3[time>250,], aes(x=n_vars, y=time, col=factor(n_train)))+geom_point()+xlab("num. features")
ggplot(results3[time>250,], aes(x=n_test, y=time, col=factor(n_train)))+geom_point()
ggplot(results3[time>250,], aes(x=n_train, y=time, col=factor(computer_name)))+geom_point()
ggplot(results3[time>250,], aes(x=n_cores, y=time, col=factor(computer_name)))+geom_point()
ggplot(results3[time>250,], aes(x=n_batches, y=time, col=factor(n_cores)))+geom_point()
#samme antall features og n_test, to "grupper" basert på n_train, martins computer har tiden mere "konsentrert" enn min computer

model3 <- lm(log(time)~n_train+n_vars*n_test+computer_name+n_cores*n_batches, data=results3)
summary(model3)
plot(x=exp(predict(model3)), y=results3[["time"]],
     xlab='Predicted Values',
     ylab='Actual Values',
     main='Copula model: Predicted vs. Actual Values',log="")
abline(a=0, b=1)


p1<-ggplot(results1, aes(x=predict(model1), y=results1[["time"]]))+geom_point()+geom_abline()
p1<-p1+xlab("Predicted values")+ylab("Observed values")+ggtitle("Independence&Gaussian")
p2<-ggplot(results2, aes(x=predict(model2), y=results2[["time"]]))+geom_point()+geom_abline()
p2<-p2+xlab("Predicted values")+ylab("Observed values")+ggtitle("Empirical")
p3<-ggplot(results3, aes(x=exp(predict(model3)), y=results3[["time"]]))+geom_point()+geom_abline()
p3<-p3+xlab("Predicted values")+ylab("Observed values")+ggtitle("Copula")

ggarrange(p1,p2,p3,ncol=3)

# vi ser på data hvor n_cores=1 og n_batches<30 og corr=0, så data er uniform

# generelt se på data
ggplot(results, aes(x=model, y=time, col=factor(computer_name)))+geom_point()
ggplot(results, aes(x=correlation, y=time, col=factor(computer_name)))+geom_point()
# ser ut som at model og corr. har lite å si
ggplot(results, aes(x=n_vars, y=time, col=factor(n_train)))+geom_point()
ggplot(results, aes(x=n_test, y=time, col=factor(n_train)))+geom_point()
ggplot(results, aes(x=n_train, y=time, col=factor(approach)))+geom_point()
ggplot(results, aes(x=n_batches, y=time, col=factor(computer_name)))+geom_point()
ggplot(results, aes(x=approach, y=time, col=factor(computer_name)))+geom_point()
ggplot(results, aes(x=n_cores, y=time))+geom_point()
ggplot(results, aes(x=computer_name, y=time))+geom_point()

copula <- ggplot(results[approach=="copula",], aes(x=n_test, y=time, col=factor(n_vars)))+geom_point()+ scale_y_log10()+ggtitle("Copula")
independence<-ggplot(results[approach=="independence",], aes(x=n_test, y=time, col=factor(n_vars)))+geom_point()+ scale_y_log10()+ggtitle("Independence")
gaussian <- ggplot(results[approach=="gaussian",], aes(x=n_test, y=time, col=factor(n_vars)))+geom_point()+ scale_y_log10()+ggtitle("Gaussian")
empirical <- ggplot(results[approach=="empirical",], aes(x=n_test, y=time, col=factor(n_vars)))+geom_point()+ scale_y_log10()+ggtitle("Empirical")
ggarrange(copula,gaussian,empirical,independence,ncol=2,nrow=2)

# independence&gaussian equation
model <- lm(time~n_vars*approach*n_test+n_batches*n_cores, data=results[approach=="independence" | approach =="gaussian"])
summary(model)
b <- coefficients(model)
pred_func <- function(n_vars, approach, n_test){
  #ŷ = b0 + b1X1 + b2X2 + b3X3 + b4X1X2 + b5X1X3 + b6X2X3 + b7X1X2X3
  #scaling the variables
  n_vars <- (n_vars-8.715897)/2.184796
  n_test <- (n_test-12.48636)/7.501352

  y <- {b[1] + #b0
        b[2]*n_vars + #b1X1
        b[3]*(approach=="empirical")+b[4]*(approach=="gaussian")+b[5]*(approach=="independence") + #b2X2
        b[6]*n_test + #b3X3
        b[7]*n_vars*(approach=="empirical") + b[8]*n_vars*(approach=="gaussian")+ b[9]*n_vars*(approach=="independence") + #b4X1X2
        b[10]*n_vars*n_test + #b5X1X3
        b[11]*n_test*(approach=="empirical") + b[12]*n_test*(approach=="gaussian")+ b[13]*n_test*(approach=="independence") + #b6X2X3
        b[14]*n_test*n_vars*(approach=="empirical") + b[15]*n_test*n_vars*(approach=="gaussian")+ b[16]*n_test*n_vars*(approach=="independence")
        # missing terms from n_batches*n_cores
        }

  return(y)
}


