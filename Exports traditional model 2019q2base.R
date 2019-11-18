
rm(list = ls()) # clear global environment 
cat("\014") # clear the console 


library(olsrr) 
library(data.table) 
library(ggplot2) 
library(zoo) 


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
#install.packages("corrplot")
library(corrplot) 
corrplot(cor(hist_prices)) 

#
hist_reg = as.matrix(data.frame(hist_reg)) 
forc_reg = forc 
forc_reg$Actual = NULL 
forc_reg$quarter=NULL 
forc_reg = as.matrix(data.frame(forc_reg)) 


library(forecast) 
tail(hist) 
fit = arima(ts(hist$Actual),xreg = hist_reg, order=c(1,1,2), include.mean=T)# as of 2018-09 (1,0,1) before that (1,1,2)) 
#fit = auto.arima(ts(hist$Actual),xreg=hist_reg, ic="aic", trace=TRUE, allowdrift=FALSE)#,lambda=0, seasonal=TRUE)# 

summary(fit) 

export_predict = predict(fit, n.ahead=74, newxreg=forc_reg, level=95)#interval = "prediction", conf.level=.9) # predict 
export_predict$quarter = seq(as.Date("2019-07-01"),as.Date("2037-10-01"),by="quarter")  

resids = as.vector(resid(fit)) 
export_predict
#write.csv(export_predict,"./Forecast from arima model_pess 20190903.csv") 
#write.csv(resids, "./Residuals from arima model 20190709.csv") 
#write.csv(summary(fit),"./Equation from arima_base 20190903.csv") 
# 

# 
fit_alt = arima(ts(hist$Actual),xreg = hist_reg, order=c(0,0,0), include.mean=T)# as of 2018-09 (1,0,1) before that (1,1,2)) 

export_predict = predict(fit_alt, n.ahead=75, newxreg=forc_reg, level=95)#interval = "prediction", conf.level=.9) # predict 
export_predict$quarter = seq(as.Date("2019-04-01"),as.Date("2037-10-01"),by="quarter")  

summary(fit_alt) 
export_predict 
resids_alt = as.vector(resid(fit_alt)) 

#write.csv(export_predict,"./Forecast from old model 20190903.csv") 
#write.csv(resids_alt, "./Residuals from old model 20190709.csv") 
#write.csv(summary(fit_alt),"./Equation from old 20190709.csv") 
