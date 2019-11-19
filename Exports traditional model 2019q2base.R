
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


setwd("~/Dropbox/Work and research/Port Authority/Port Exports 2019/port_exports")

####################################################################################
# TRADITIONAL MODEL TRADITIONAL MODEL TRADITIONAL MODEL TRADITIONAL MODEL 
####################################################################################


exports = read.csv("./Loaded_exports19Q2.csv") 
exports$quarter = seq(as.Date("1997/1/1"), as.Date("2019/04/01"), by="quarter") # Add date

tail(exports) 
exports$Year=NULL 
exports$Quarter=NULL 
#dat3 = merge(exports, dat2, by = "quarter", all.x=TRUE) 
#dat3$Year=NULL 
#head(exports) 
#summary(as.numeric(dat3$Japan..Real.Price.of.Crude.Oil..Units..real.2011.Japanese.yen.per.barrel..Data.source..IHS.Economics.calculation.using.data.from.Department.of.Energy)) 

# Add time series 
old = read.csv("./Inputs_19Q2pess.csv") 
old$quarter = seq(as.Date("1997/1/1"), as.Date("2037/10/01"), by="quarter") # Add date
old$Mnemonic = NULL 
exports_old = merge(exports,old,by="quarter", all.y=TRUE) 

hist = subset(exports_old,exports_old$quarter<="2019-04-01") 
forc = subset(exports_old,exports_old$quarter>"2019-04-01") 

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


# 
fit_alt = arima(ts(hist$Actual), xreg = hist_reg, order=c(0,0,0), include.mean=T) # as of 2018-09 (1,0,1) before that (1,1,2)) 

export_predict = predict(fit_alt, n.ahead=75, newxreg=forc_reg, level=95) #interval = "prediction", conf.level=.9) # predict 
export_predict$quarter = seq(as.Date("2019-04-01"),as.Date("2037-10-01"),by="quarter")  

summary(fit_alt) 
export_predict 
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

exports_old.mega = merge(exports_old, other.noNa, by="quarter") 

# Lag the variable 
actual_lag = slide(exports_old.mega, Var = "Actual", slideBy = -1)
exports_old.mega = cbind(exports_old.mega,actual_lag) 
exports_old.mega = exports_old.mega[-1,] # Dump the first row 

hist.mega = subset(exports_old.mega,exports_old.mega$quarter<="2019-04-01") 
forc.mega = subset(exports_old.mega,exports_old.mega$quarter>"2019-04-01") 

# Elastic Net 
blend = .9 # pick penalization process (between 0 and 1)
hist.mega.actual = hist.mega[,2] # Distinguish second column, the Y variable
#hist.mega.matrix = hist.mega[,-1:2] # Lose the first and second column in main matrix 
hist.mega.matrix = hist.mega[,-1] # Lose the first and second column in main matrix 
hist.mega.matrix = hist.mega.matrix[,-1] # Not sure why this takes two rows but it does. 

hist.mega.matrix = data.matrix(hist.mega.matrix) 

set.seed(1042) 
cv.mega = cv.glmnet(hist.mega.matrix, hist.mega.actual, alpha = blend, nfolds = 10) 
plot(cv.mega) 
set.seed(222)
cv.model = glmnet(hist.mega.matrix, hist.mega.actual, alpha=blend, lambda=cv.mega$lambda.1se) 
cv.model 
coef(cv.model) 
# So extract those coefficients 
    # https://gist.github.com/ydavidchen/a166bc364cfadd53921bb9a6f07100bb

myCoefs = coef(cv.model); #, s="lambda.min");
myCoefs[which(myCoefs != 0 ) ]               #coefficients: intercept included
myCoefs@Dimnames[[1]][which(myCoefs != 0 ) ] #feature names: intercept included

myResults = data.frame(
  features = myCoefs@Dimnames[[1]][ which(myCoefs != 0 ) ], #intercept included
  coefs    = myCoefs              [ which(myCoefs != 0 ) ]  #intercept included
)
myResults 
head(myResults) 
str(myResults) 
myNames = c(myResults$features) 
myNames 

#
# cv.predict = predict(cv.mega, s=cv.mega$lambda.1se ,newx=hist.mega.actual)