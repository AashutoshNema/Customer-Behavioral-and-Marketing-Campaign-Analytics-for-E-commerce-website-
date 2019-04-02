#Stoping the warning signs
options(warn = -1)
#Package installation 
install.packages("readr")
install.packages("knitr")
install.packages("lubridate")
install.packages("plyr")
install.packages("ggplot2")
install.packages("usmap")
install.packages("rbokeh")
install.packages("plotly")
install.packages("scales")

#-----------------Part-I Data Wrangling------------
#Importing Libraries and data 
library(readr)
library(knitr)
library(lubridate) #Extracting TimeSeries
library(plyr)

data.url = "https://raw.githubusercontent.com/jasonadki/SafeautoCodingChallenge/master/safeAuto_data_challenge.csv"
df = read.csv(data.url, na.strings = "")
summary(df)


#Preprocessing
#Organizing Events Column
count(df$Event)

#Excluding factor levels which contribute to less than 2% of the data ,to reduce noise
df$Event = factor(df$Event, exclude = c('#/404', '#/howtopay', 'The TokenGuid [ 086043cc-2979-c647-0d10-94a8c9e349bf ] already exists.', 'The TokenGuid [ 1d89ed28-00c6-9bb3-b018-1aa3df20e498 ] already exists.',
                                        'The TokenGuid [ 554b3253-7218-c19c-3df8-f1aeb0572561 ] already exists.', 'The TokenGuid [ 6efe6786-043d-76fc-8c64-e3f9376bc45a ] already exists.', 'The TokenGuid [ 7d5ff955-1730-6b3c-3c25-4842137cff90 ] already exists.',
                                        'The TokenGuid [ 7ff0b9de-71e7-93f4-915c-82ab21386fe7 ] already exists.', 'The TokenGuid [ 81cc4e21-8098-1726-a3e2-f8b9c0406231 ] already exists.',
                                        'The TokenGuid [ abf54c90-b707-bc18-a86e-7dba6cc07eca ] already exists.','The TokenGuid [ c02412cd-5077-12a9-f11d-12b592cdd1da ] already exists.',
                                        'The TokenGuid [ e73293d9-b6ec-78cf-48f7-857b24a9b5eb ] already exists.', 'The underlying provider failed on Open.','TokenGuid is not valid','Unable to SaveToken due to error: SaveToken Failed:'))

#Organizing Browser Columns
count(df$Browser)

df$Browser[df$Browser == "IE" |
             df$Browser == "IEMobile"] = "InternetExplorer"
df$Browser[df$Browser == "Mozilla"] = "Firefox"
df$Browser = factor(df$Browser, exclude = c("Unknown", "IE","IEMobile", "Mozilla"))
                    
#organizing Device Brand Column
df$DeviceBrand[df$DeviceBrand == "iPhone"|
                 df$DeviceBrand == "IPad"|
                 df$DeviceBrand == "iPad"|
                 df$DeviceBrand == "IPod"|
                 df$DeviceBrand == "iPod touch"] = "IPhone"
df$DeviceBrand[df$DeviceBrand == "Windows Phone 10.0"|
                 df$DeviceBrand == "BB10"] = "Mobile" #Clubing Windows and BB with Mobile due to low counts


df$DeviceBrand = factor(df$DeviceBran, exclude = c("iPhone", "IPad", "iPad", "IPod", "iPod touch", "Windows Phone 10.0","BB10"))

#Omitting missing values
sum(is.na(df))
df = na.omit(df)
sum(is.na(df))

#Creating a Resolution column from ScreenWidhth and ScreenHeight
df$ScreenWidth = as.numeric(df$ScreenWidth)
df$Resolution = cut(df$ScreenWidth, breaks = c(-Inf,640, 720, 1280,Inf),labels = c("<=640 x 480","720 x 480","1280 x 720", ">=1280 x 720"))

#Converting Timestamp
df$RecordDateTime = as.POSIXlt(df$RecordDateTime, tz="", origin = "2016-10-01")

#Extracting Day,Date,Time, Month
attach(df)
df$Date = RecordDateTime$mday
df$weekday = RecordDateTime$wday
df$Time = RecordDateTime$hour

days = c("Sunday","Monday","Tuesday","Wednesday", "Thursday", "Friday", "Saturday")
df$weekday = factor(df$weekday, labels = days)

df = df[-c(1,4,9,10,13)] #Final dataframe to be used for visualization

#--------------Part-II VISUALIZATION OF DATA-----------------------

#Visualization and Inferencing
#Packages for Visualization
library(ggplot2)
library(usmap)
library(rbokeh)
library(plotly)
library(scales)



#Visualizing Frequency of user visits

#Plot of Customer Visits w.r.t. time of the day
cv_hr <- figure(title = 'User visit round the day', width = 800, height = 400) %>%
  ly_hist(Time, data = df, breaks = 35, freq = FALSE) %>%
  ly_density(Time, data = df)
cv_hr

#Plot of customer visit w.r.t to week of the day
cv_week <- ggplot(df, aes(x=weekday)) + geom_bar(color = "black", fill = "light blue") + 
  theme_bw() + ggtitle("Customer visit during the weekday") + xlab("Day of the week") + ylab("Density")
cv_week

#plot of customer visit w.r.t date of the month
cv_date <- figure(title = 'User visit round the Month', width = 800, height = 400) %>%
  ly_hist(Date, data = df, breaks = 35, freq = FALSE) %>%
  x_range(c(1,13))
cv_date


#Browser Type Analysis 
pt_br <- ggplot(df, aes(x=Browser, fill = factor(Browser))) + geom_bar(width = 1) + coord_polar(theta = "x",direction = -1)+
  theme_grey() + ggtitle("Browser used by customer") + scale_y_continuous(labels = comma) + ylab("No. of customers")+
  scale_fill_discrete(name = "Browser Used")
pt_br

pt_mo <- ggplot(df, aes(x=Browser, y = DeviceBrand, fill = factor(Browser))) + geom_jitter() +
  ggtitle("Browser relation with devise used")
  
pt_mo

#Events Analysis
pt_ev <- ggplot(df, aes(x=Event, fill = factor(Event))) + geom_bar(width = 1) + coord_polar(theta = "x",direction = 1)+
  theme_grey() + ggtitle("Frequency of Events") + scale_y_continuous(labels = comma) + ylab("No. of Visitors")+
  scale_fill_discrete(name = "Events")
pt_ev


#Heatmap as per state frequency
state.count = count(State)
mapDetails <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = TRUE,
  lakecolor = toRGB('white')
)
usaMap <- plot_geo(state.count, locationmode = 'USA-states') %>% 
  add_trace(z = state.count$freq, locations = state.count$x, color = state.count$freq, colors = 'Blues' ) %>%
  layout( geo = mapDetails )

usaMap

#Screen Resolution Analysis
cv_r <- ggplot(df, aes(x=Resolution)) + geom_bar(color = "black", fill = "light blue") + 
  theme_bw() + ggtitle("User device resolution") + xlab("Resolution") + ylab("No. of Users") + scale_y_continuous(labels = comma)
cv_r

#Is device mobile? 
cv_k <- ggplot(df, aes(x=DeviceIsMobile)) + geom_bar(color = "black", fill = "light blue") + 
  theme_bw() + ggtitle("Is the device Mobile") + xlab("True or False") + ylab("No. of Users") + scale_y_continuous(labels = comma)
cv_k

cv_k <- ggplot(df, aes(x=DeviceIsMobile, fill = factor(DeviceIsMobile))) + geom_bar(width = 1) + coord_polar(theta = "y",direction = 1)+
  theme_grey() + ggtitle(" Is the Device Used Mobile") + scale_y_continuous(labels = comma) + ylab("No. of customers")+
  scale_fill_discrete(name = "Events")
cv_k

#Turning the warning signs backon
options(warn = 0)

