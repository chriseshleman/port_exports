
rm(list = ls()) # clear global environment 
cat("\014") # clear the console 


library(olsrr) 
library(data.table) 
library(ggplot2) 
library(zoo) 
library(corrplot) 
library(forecast) 
library(lmtest) 
library(car) 
library(dplyr) 
library(DataCombine)
library(glmnet) 
library(Hmisc) 


setwd("~/Dropbox/Work and research/Port Authority/Port Exports 2019/port_exports")

####################################################################################
# TRADITIONAL MODEL TRADITIONAL MODEL TRADITIONAL MODEL TRADITIONAL MODEL 
####################################################################################

exports = read.csv("./Loaded_exports19Q3.csv") 
exports$quarter = seq(as.Date("1997/1/1"), as.Date("2019/07/01"), by="quarter") # Add date

exports$Year=NULL 
exports$Quarter=NULL 

# Add time series 
old = read.csv("./Inputs_19Q2pess.csv") 
old$quarter = seq(as.Date("1997/1/1"), as.Date("2037/10/01"), by="quarter") # Add date
old$Mnemonic = NULL 
exports_old = merge(exports,old,by="quarter", all.y=TRUE) 

hist = subset(exports_old,exports_old$quarter<="2019-07-01") 
forc = subset(exports_old,exports_old$quarter>"2019-07-01") 

hist_reg = hist 
hist_reg$Actual=NULL 
hist_reg$quarter=NULL 

## Quick sidebar - # Raw input correlations 
hist_prices = hist_reg 
hist_prices$Q2=NULL 
hist_prices$Q3=NULL 
hist_prices$Q4=NULL 
str(hist_prices) 
cor(hist_prices) 


#
hist_reg = as.matrix(data.frame(hist_reg)) 
forc_reg = forc 
forc_reg$Actual = NULL 
forc_reg$quarter=NULL 
forc_reg = as.matrix(data.frame(forc_reg)) 

fit = arima(ts(hist$Actual),xreg = hist_reg, order=c(1,1,2), include.mean=T)# as of 2018-09 (1,0,1) before that (1,1,2)) 
#fit = auto.arima(ts(hist$Actual),xreg=hist_reg, ic="aic", trace=TRUE, allowdrift=FALSE)#,lambda=0, seasonal=TRUE)# 

summary(fit) 

export_predict = predict(fit, n.ahead=74, newxreg=forc_reg, level=95)#interval = "prediction", conf.level=.9) # predict 
export_predict$quarter = seq(as.Date("2019-07-01"),as.Date("2037-10-01"),by="quarter")  

resids = as.vector(resid(fit)) 
durbinWatsonTest(resids) # Autocorrelation 
toy.fit = as.data.frame(cbind(hist$Actual,fitted(fit))); names(toy.fit) = c("actual","fitted")
mape.fit = mean(abs(toy.fit$actual-toy.fit$fitted/toy.fit$actual) * 100) # MAPE # rowMeans
rm(toy.fit) 
export_predict 
#write.csv(export_predict,"./Forecast from arima model_pess 20190903.csv") 
#write.csv(resids, "./Residuals from arima model 20190709.csv") 
#write.csv(summary(fit),"./Equation from arima_base 20190903.csv") 


 
fit_alt = arima(ts(hist$Actual), xreg = hist_reg, order=c(0,0,0), include.mean=T) # as of 2018-09 (1,0,1) before that (1,1,2)) 

export_predict.alt = predict(fit_alt, n.ahead=74, newxreg=forc_reg, level=95) #interval = "prediction", conf.level=.9) # predict 
export_predict.alt$quarter = seq(as.Date("2019-07-01"),as.Date("2037-10-01"),by="quarter")  

summary(fit_alt) 
export_predict.alt
resids_alt = as.vector(resid(fit_alt)) 
durbinWatsonTest(resids_alt)
toy.fit_alt = as.data.frame(cbind(hist$Actual,fitted(fit_alt))); names(toy.fit_alt) = c("actual","fitted")
mape_alt = mean(abs(toy.fit_alt$actual-toy.fit_alt$fitted/toy.fit_alt$actual) * 100) # MAPE # rowMeans
rm(toy.fit_alt) 

#d = sum((fit_alt$residuals - lag(fit_alt$residuals))^2, na.rm = TRUE) /
# sum(fit_alt$residuals^2, na.rm = TRUE) 


#write.csv(export_predict,"./Forecast from old model 20190903.csv") 
#write.csv(resids_alt, "./Residuals from old model 20190709.csv") 
#write.csv(summary(fit_alt),"./Equation from old 20190709.csv") 


### Third: variable selection 

# IHS Forecasts: "Other Aggregates", Quarterly 
#other = read.csv("./IHS_otheragg2019Q3.csv") 
other = read.csv("./IHS_otheragg2019Q3.csv", na.strings = c("#N/A", "bar"))
other$quarter = as.Date(other$Mnemonic, format="%m/%d/%y") 
other$Mnemonic = NULL 

# Clear NAs 
not_all_na = function(x) {!all(is.na(x))} # Quick function 
other = other %>% select_if(not_all_na) # Remove variables with all NAs. 
# (There aren't any.) 
other.noNa = other %>%
  select_if(~ !any(is.na(.)))  # Remove variables with any NAs. 
exports.mega = merge(exports_old, other.noNa, by="quarter") 

####### 

# Lag the variable 
# exports.mega = slide(exports.mega, Var = "Actual", slideBy = -1) # This way sucks 
exports.mega$Actual_1 = Lag(exports.mega$Actual, +1) # Way better. 

exports.mega = exports.mega[-1,] # Dump the first row 

hist.mega = subset(exports.mega,exports.mega$quarter<="2019-07-01") # Take the old stuff for building model 

# Prep for regularization 
# Split into Y and everything else, and dump quarter variable
actual = hist.mega[,2] # Distinguish second column, the Y variable 

#hist.mega.matrix = hist.mega[,-1:2] # Lose the first and second column in main matrix 
hist.mega.matrix = hist.mega[,-1] # Lose the first and second column in main matrix 
hist.mega.matrix = hist.mega.matrix[,-1] 

hist.mega.matrix = data.matrix(hist.mega.matrix) 

# Elastic Net 
set.seed(1039) 
cv.mega = cv.glmnet(hist.mega.matrix, actual, alpha = 1, nfolds = 10) # pick penalization process (between 0 and 1) 
options(scipen=999)
plot(cv.mega) 
set.seed(221)
cv.model = glmnet(hist.mega.matrix, actual, alpha=1, lambda=cv.mega$lambda.1se) 
cv.model 
coef(cv.model) 
# So extract those coefficients 
    # https://gist.github.com/ydavidchen/a166bc364cfadd53921bb9a6f07100bb
myCoefs = coef(cv.model); 
myCoefs[which(myCoefs != 0 ) ]               #coefficients: intercept included
myCoefs@Dimnames[[1]][which(myCoefs != 0 ) ] #feature names: intercept included

myResults = data.frame(
  features = myCoefs@Dimnames[[1]][ which(myCoefs != 0 ) ], #intercept included
  coefs    = myCoefs              [ which(myCoefs != 0 ) ]  #intercept included
)

myResults 

exports.mini = exports.mega[myResults$features] 
exports.mini$Q2 = NULL 
exports.mini$Q3 = NULL 
exports.mini$Q4 = NULL 
exports.dummies = exports_old[c("quarter","Q2", "Q3", "Q4")] 
#head(exports_old[-1,1:2]) 
#exports.mini = cbind(exports.dummies[-1,1:2],exports.mini) #exports_old$Actual[-1,])#,exports_old.mega)
head(exports.dummies) 
head(exports.mini) 

#
exports.mini = merge(exports.mini,exports.dummies, by="quarter")  

# Fit ARIMA terms 
before = subset(exports.mini,exports.mini$quarter <= "2019-07-01") 
after = subset(exports.mini,exports.mini$quarter > "2019-07-01") 

oldreg=as.matrix(data.frame(before$GDP..G8.Q.FGBA1, before$GCRE.WEHEM.Q.FGBA1, before$WPI06.Q.FMFT, 
                             before$CR.NP.WEHEM.Q.FGBA1, before$WPI10.Q.FMFT, before$WPI09.Q.FMFT, 
                             before$JEXCHMTPREAL.Q.FMFT, before$WPIINDO.Q.FMFT, before$Q2, before$Q3, before$Q4))#, before$Actual_1)) 
newreg=as.matrix(data.frame(after$GDP..G8.Q.FGBA1, after$GCRE.WEHEM.Q.FGBA1, after$WPI06.Q.FMFT, 
                             after$CR.NP.WEHEM.Q.FGBA1, after$WPI10.Q.FMFT, after$WPI09.Q.FMFT, 
                             after$JEXCHMTPREAL.Q.FMFT, after$WPIINDO.Q.FMFT, after$Q2, after$Q3, after$Q4))#, after$Actual_1)) 

# Model 
dim(oldreg)[1]==length(before$Actual) 
#fit.net = arima(ts(before$week),xreg = oldreg, order=c(0,0,1), include.mean=T)# as of 2018-09 (1,0,1) before that (1,1,2)) 
fit.net = auto.arima(ts(before$Actual),xreg=oldreg, ic="aic", trace=TRUE, allowdrift=FALSE)#,lambda=0, seasonal=TRUE)# 

predict.net = predict(fit.net, n.ahead=nrow(after), newxreg=newreg, level=95)#interval = "prediction", conf.level=.9) # predict 
predict.net$quarter = seq(as.Date("2019-07-01"),as.Date("2037-10-01"),by="quarter") #+extra 


summary(fit.net) 
resids.net = as.vector(resid(fit.net)) 
durbinWatsonTest(resids.net)
# toy.fit.net = as.data.frame(cbind(actual,fitted(fit.net))); names(toy.fit.net) = c("actual","fitted")
# mape.net = mean(abs(toy.fit.net$actual-toy.fit.net$fitted/toy.fit.net$actual) * 100) # MAPE # rowMeans
rm(toy.fit.net) 
### DONE 

export_predict 

predict.net

# Diagnostics 
out1 = tidy(fit) 
#out2 = tidy(glance(fit)) ## why is this crashing my program? 
out2 = glance(fit) 
out1 
out2 
out3 = tidy(fitsat) 
#out4 = tidy(glance(fitsat)) ## why is this crashing my program? 
out5 = tidy(fitsun) 
#out6 = tidy(glance(fitsun)) ## why is this crashing my program? 
accuracy(fit) 
accuracy(fit)[,'MAPE']
