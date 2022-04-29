DM <- read.csv("C:/Users/rohan/Desktop/HOME/Personal/Purdue/Courses/Module_2/WebDataAnalytics/Homework/HW_6/GlobalMobileAppsExercise.csv", sep=",", header=T); # data is loaded to a frame

#The attach() function in R can be used to make objects within dataframes accessible in R with fewer keystrokes
attach(DM) 

#Features
names(DM)

#Unique Value count of each variable
rapply(DM,function(x)length(unique(x)))

#The str function shows an outline of the structure of its argument
str(category)

###############################################################################
#Checking for factors and nominal variables
unique(DM$device) 
DM$device = factor(DM$device)

unique(DM$region)
DM$region = factor(DM$region)

unique(DM$category)
DM$category = factor(DM$category)

unique(DM$rindex)
DM$rindex = factor(DM$rindex)

unique(DM$t_day)
DM$t_day = factor(DM$t_day)

unique(DM$in_app_ads)
DM$in_app_ads = factor(DM$in_app_ads)

unique(DM$inapp_addummy)
DM$inapp_addummy = factor(DM$inapp_addummy)

unique(DM$crawl_date)
unique(DM$num_screenshot)
unique(DM$developer)
DM$developer = factor(DM$developer)

unique(DM$app_type)
DM$app_type = factor(DM$app_type)

unique(DM$in_app_purchase)
DM$in_app_purchase = factor(DM$in_app_purchase)

unique(DM$inapp_purchasedummy)
DM$inapp_purchasedummy = factor(DM$inapp_purchasedummy)

unique(DM$deviceindex)
DM$deviceindex = factor(DM$deviceindex)

unique(DM$categoryindex)
DM$categoryindex = factor(DM$categoryindex)

unique(DM$app_store)
DM$app_store = factor(DM$app_store)

unique(DM$appstoreindex)
DM$appstoreindex = factor(DM$appstoreindex)

unique(DM$apptypeindex)
DM$apptypeindex = factor(DM$apptypeindex)

detach(DM) 
attach(DM) 
###############################################################################
#Create summary
summary(DM)
###############################################################################
#Creating Log transformations for interval variabes
hist(DM$rank, col = 'red')
hist(log(DM$rank), col = 'blue')
Log_rank = log(DM$rank)

hist(DM$rating_count+0.01, col = 'red')
hist(log(DM$rating_count+0.01), col = 'blue')
Log_rating_count = log(DM$rating_count+0.001)

hist(DM$average_rating+0.001, col = 'red')
hist(log(DM$average_rating+0.001), col = 'blue')
average_rating = log(DM$average_rating+0.001)

hist(DM$price+0.01, col = 'red')
hist(log(DM$price+0.01), col = 'blue')
Log_price = log(DM$price+0.001)

hist(DM$filesize, col = 'red')
hist(log(DM$filesize), col = 'blue')
Log_filesize = log(DM$filesize)

hist(DM$app_age_current_version+0.1, col = 'red')
hist(log(DM$app_age_current_version+0.1), col = 'blue')
Log_app_age_current_version = log(DM$app_age_current_version+0.1)

hist(DM$num_screenshot, col = 'red')


########################################################################################################
#Correlation#

#cor(DM$rank,DM$app_age_current_version)
#cor(DM$rank,DM$apptypeindex)
#cor(log(DM$rank),log(DM$price))

#T test
#Region Check
t.test(DM[rindex==1,]$rank, DM[rindex==2,]$rank, alternative = 'greater')
print("rindex1: China has higher ranking in average than 2: USA as we reject the null hypothesis")
t.test(DM[rindex==1,]$price, DM[rindex==2,]$price, alternative = 'less')
print("rindex1: China has lower average prices than 2: USA as we reject the null hypothesis")
t.test(DM[rindex==1,]$average_rating, DM[rindex==2,]$average_rating, alternative = 'less')
print("rindex1: China has smaller average ratings than 2: USA as we reject the null hypothesis")
t.test(DM[rindex==1,]$rating_count, DM[rindex==2,]$rating_count, alternative = 'less')
print("rindex1: China has smaller number of ratings than 2: USA as we reject the null hypothesis")
print("Although China has lower average prices, the average ratings and average rating counts are both low compared to USA")

#device check
t.test(DM[deviceindex==1,]$rank, DM[deviceindex==2,]$rank, alternative = 'less')
print("deviceindex: SmartPhones have lower rankings in average than 2: tablets as we reject the null hypothesis")
t.test(DM[deviceindex==1,]$price, DM[deviceindex==2,]$price, alternative = 'less')
print("deviceindex: SmartPhones have lower average prices than 2: tablets as we reject the null hypothesis")
t.test(DM[deviceindex==1,]$average_rating, DM[deviceindex==2,]$average_rating, alternative = 'greater')
print("deviceindex: SmartPhones have greater value of average ratings than 2: tablets as we reject the null hypothesis")
t.test(DM[deviceindex==1,]$rating_count, DM[deviceindex==2,]$rating_count, alternative = 'greater')
print("deviceindex: SmartPhones have greater number of ratings than 2: tablets as we reject the null hypothesis")
print("This indicates that the prices of apps are lower for smartphones than for tablets. This may also be the reason why the average ratings and rating count is greater in the smartphone platform.")

#Apple vs Google
t.test(DM[appstoreindex==2,]$rank, DM[appstoreindex==3,]$rank, alternative = 'greater')
print("Free Apps have higher ranking in average than paid apps as we reject the null hypothesis")
t.test(DM[appstoreindex==2,]$average_rating, DM[appstoreindex==3,]$average_rating, alternative = 'less')
print("Free apps have smaller average ratings than paid apps as we reject the null hypothesis.")
t.test(DM[appstoreindex==2,]$rating_count, DM[appstoreindex==3,]$rating_count, alternative = 'less')
print("Apple has smaller average rating count than Google as we reject the null hypothesis")
print("The demand in terms for rank in Apple seems to be higher compared to google. However, the prices are also higher in apple which may explain the demand in terms of higher rank. On the other hand, Apple has smaller number of ratings and smaller average ratings compared to google. This is possibly because Google has low-cost devices which are accessible to a larger number of people, making apps also available to a larger number of people.")

# Free vs Paid apps
t.test(DM[apptypeindex==1,]$rank, DM[apptypeindex==3,]$rank, alternative = 'greater')
print("Apple has higher ranking in average than Google as we reject the null hypothesis")
t.test(DM[apptypeindex==1,]$price, DM[apptypeindex==3,]$price, alternative = 'greater')
print("Apple has higher prices in average than Google as we reject the null hypothesis")
t.test(DM[apptypeindex==1,]$average_rating, DM[apptypeindex==3,]$average_rating, alternative = 'less')
print("Apple has smaller average ratings than Google as we reject the null hypothesis")
t.test(DM[apptypeindex==1,]$rating_count, DM[apptypeindex==3,]$rating_count, alternative = 'less')
print("Apple has smaller average rating count than Google as we reject the null hypothesis")

#########################
#########################
#  Linear Regression    #
#########################
# Uses moderndrive package to get output


Linear_reg = lm(Log_rank~ Log_price+Log_rating_count+Log_filesize+Log_app_age_current_version+DM$deviceindex+
             DM$appstoreindex+DM$rindex+DM$apptypeindex+DM$num_screenshot+
             DM$average_rating+DM$inapp_addummy+DM$inapp_purchasedummy+
             DM$categoryindex+DM$developer)
#summary(Linear_reg)

####--Reinstall moderndive from the below commented line if the output is not generated or there is an error. It should work.--###
#install.packages("moderndive")
library(moderndive)
res <- as.data.frame(get_regression_table(
  Linear_reg,
  conf.level = 0.95,
  digits = 3,
  print = FALSE,
  default_categorical_levels = FALSE
))
View(res)
write.csv(res,"C:/Users/rohan/Desktop/HOME/Personal/Purdue/Courses/Module_2/WebDataAnalytics/Homework/HW_6/LinearRegressionModel.csv")

###########################################
