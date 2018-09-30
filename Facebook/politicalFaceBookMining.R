#Facebook 


library(readr)
library(readxl)
library(ggmap)
library(maps)

Post.Acts <- read_excel("Desktop/Projects/Political Tweets/Facebook/Post Insights.xlsx",
                                                                                           sheet = 'Lifetime Post Stories by act...')


ggplot(Post.Acts) 
Post.Acts$Date <- as.Date(Post.Acts$Posted)


ggplot(Post.Acts, aes(x = Date, y =like)) + geom_bar(stat="identity", width=0.5)

Post.Acts$`Post ID`


#### Sample Graph


Data=c(rep("sorgho" , 3) , rep("poacee" , 3) , rep("banana" , 3) , rep("triticum" , 3) )
condition=rep(c("normal" , "stress" , "Nitrogen") , 4)
value=abs(rnorm(12 , 0 , 15))
data=data.frame(specie,condition,value)

Date <- c(rep("June 13",3),rep("June 14",3), rep("June 15",3), rep("June 16",3), rep("June 17",3),rep("June 18",3),rep("June 19",3),rep("June 20",3))
variable <- rep(c("share","like","comment"),4)
value <- sample(1:40, 24, replace=F)
data=data.frame(Date,variable,value)
                
ggplot(data, aes(fill=variable, y=value, x=Date)) + 
  geom_bar( stat="identity", position="fill",width=0.5) + ggtitle(" Facebook Page User Engagement Over Time (by %) ")               
                
 
ggplot(data, aes(fill=variable, y=value, x=Date)) + 
  geom_bar( stat="identity")+ ggtitle(" Facebook Page User Engagement Over Time  ")  



ggplot(data, aes(Date, value)) +   
  geom_bar(aes(fill = variable), position = "dodge", stat="identity")+ ggtitle(" Facebook Page User Engagement Over Time  ") 



###########               
Post.Eng <- select(Post.Acts, Date, share, like, comment)

Post.Eng <- melt(Post.Eng, id.vars = 'Date')

 # Stacked Percent
  
  
  ggplot(Post.Eng, aes(fill=variable, y=value, x=Date)) + 
  geom_bar( stat="identity", position="fill",width=0.5)
  
  
  
  
  
############################################
  
  
  
  
  #age.gen <- read_excel("/Users/georgesericcolbert/Desktop/Projects/Facebook Mining/Facebook Insights Data Export - Corey Stewart - 2018-07-16.xlsx",
                #        sheet = 'Lifetime Likes by Gender and...') 
  
  age.gen <- read_excel("/Users/georgesericcolbert/Desktop/Projects/Facebook Mining/Facebook Insights Data Export - Corey Stewart - 2018-08-13.xlsx",
                     sheet = 'Lifetime Likes by Gender and...')  
  
#age.F <-select(age.gen,Date,"F.18-24","F.25-34","F.35-44","F.45-54","F.55-64") 

age.F <- select(age.gen,Date, contains("F."))

age.M <- select(age.gen,Date, contains("M."))



### Female Fans

age.F$Date <- as.character(age.F$Date)
age.F <- filter(age.F,Date=="2018-08-12")
  
age.F <- melt(age.F )

colnames(age.F) <- c("Date","Age Bracket","Count")
 
age.F$Date <- as.Date(age.F$Date) 
  
#age.F$value223 <- sample(1:25, 140, replace=T)

ggplot(age.F, aes(`Age Bracket`, Count)) +   
  geom_bar(stat="identity",fill="magenta")+ ggtitle(" Female Page Likes by Age Bracket  ") 

#### Male Fans

age.M$Date <- as.character(age.M$Date)
age.M <- filter(age.M,Date=="2018-08-12")

age.M <- melt(age.M)

colnames(age.M) <- c("Date","Age Bracket","Count")



ggplot(age.M, aes(`Age Bracket`, Count)) +   
  geom_bar(stat="identity",fill="maroon")+ ggtitle("Male Page Likes by Age Bracket ") 

### Compare by Gender

age.F$Gender <- "Female"

age.M$Gender <- "Male"

age.gen <- rbind(age.F,age.M) 

age.gen$`Age Bracket` <- gsub("F.","",age.gen$`Age Bracket`)

age.gen$`Age Bracket` <- gsub("M.","",age.gen$`Age Bracket`)




ggplot(age.gen, aes(fill=Gender, y=Count, x=`Age Bracket`)) + 
  geom_bar(position = "dodge", stat="identity")+ ggtitle(" Facebook Page User Engagement by Age and Sex  ")  


 
x <- subset(age.gen, Date > as.Date("2018-05-12"), na.rm = TRUE )


x$Description <- NULL


x<- na.omit(x)
 x <- filter(x,row_number()==1 | row_number()==n())
x$Date <- NULL

 x <- t(x)
colnames(x) <- c("May-12", "August-12")

x <- as.data.frame(x)

x$Growth <- ((x$`August-12`-x$`May-12`)/x$`May-12`)
x$GrowthN <- (x$`August-12`-x$`May-12`)
x$Growth <- percent(x$Growth)


##############################
library(data.table)
library(stringr)
library(ggplot2)
library(ggmap)
library(maps)
library(tm)
library(SnowballC)
library(readr)
library(dplyr)
#city.likes <- read_excel("Desktop/Projects/Facebook Mining/Facebook Insights Data Export - Corey Stewart - 2018-07-13 (2).xlsx",
                     # sheet = 'Lifetime Likes by City')  

age.gen <- read_excel("/Users/georgesericcolbert/Desktop/Projects/Facebook Mining/Facebook Insights Data Export - Corey Stewart - 2018-08-13.xlsx",
                      sheet = 'Lifetime Likes by City')  



VA <- map_data("state", region="Virginia")


 Vir.likes<- select(city.likes,contains(", VA"),Date)
 
 Vir.likes$State <- "Virginia"

Vir.likes$Date <- city.likes$Date


#w <- colSums(Vir.likes,na.rm=TRUE)
 w <- filter(Vir.likes, Date=="2018-08-12") 
 
 
w$Description <- NULL
 
 
 w<- as.data.frame(w) 
 
#w <- setDT(w, keep.rownames = TRUE)[]

 w <- melt(w)
 
 
 colnames(w)<- c("State","Date","City","Count")

w <-  cbind(geocode(as.character(w$City)), w)
                    


## Geo Map One


ggplot(VA, aes(x=long, y=lat)) +
  geom_polygon() +
  coord_map() +
  geom_point(data=w, aes(x=lon, y=lat, size=Count), color="orange")+ ggtitle(" Lifetime Page Likes by City") 

#### Geo Map two

ggmap(get_map(location = 'Virginia', zoom = 7)) +
  geom_point(data=w, aes(x=lon, y=lat, size=Count), color="red")+ ggtitle(" Lifetime Page Likes by City")


###### identify fastest growing cities

library(zoo)


x <- subset(Vir.likes, Date > as.Date("2018-05-12"))


x$`Yorktown, VA` <- NULL
x$`Prince George, VA`<- NULL
x$`Colonial Heights, VA` <- NULL
x$`Martinsville, VA` <- NULL

### file NA row with the value preceding it

x <- na.locf(x)

#x<- na.omit(x)

x <- filter(x,row_number()==1 | row_number()==n())
x$Date <- NULL

x <- t(x)
colnames(x) <- c("May-12", "July-12")

x <- as.data.frame(x,stringsAsFactors=FALSE)

x$`May-12` <- as.numeric(x$`May-12`)
x$`July-12` <- as.numeric(x$`July-12`)

x$Growth.percent <- ((x$`July-12`-x$`May-12`)/x$`May-12`)
x$GrowthN <- (x$`July-12`-x$`May-12`)
x$Growth.100 <- percent(x$Growth.percent)



######################

library(scales)
key.metrics <- read_excel("Desktop/Projects/Facebook Mining/Facebook Insights Data Export - Corey Stewart - 2018-07-13 (2).xlsx",
                         sheet = 'Key Metrics') 


a <- key.metrics[1:4]

likes <- tail(a,20)

likes$`Daily New Likes` <- as.integer(likes$`Daily New Likes`)
likes$Date <- as.Date(likes$Date)
likes$`Daily Unlikes` <- as.integer(likes$`Daily Unlikes`)




ggplot(likes,aes(Date,`Daily New Likes`))+ geom_line(colour = "green")+
  scale_x_date(breaks = pretty(likes$Date, n = 10),labels = date_format("%b-%d"))+ ggtitle(" How Many People Liked the Page per Day")




ggplot(likes,aes(Date,`Daily Unlikes`))+ geom_line(colour = "red")+ 
  scale_x_date(breaks = pretty(likes$Date, n = 10),labels = date_format("%b-%d"))+ ggtitle(" How Many People Unliked the Page per Day") 



ggplot(likes,aes(Date))+geom_line(aes(y = `Daily Unlikes`, colour = "Daily Unlikes`")) + 
  geom_line(aes(y = `Daily New Likes`, colour = "`Daily New Likes`"))


###########


 a <- select(key.metrics,`Daily Negative Feedback From Users`,`Daily Positive Feedback From Users`)





###########################

#Daily: The number of times people have given positive feedback to your Page, by type.(Total Count)







pos.metrics <- read_excel("Desktop/Projects/Facebook Mining/Facebook Insights Data Export - Corey Stewart - 2018-07-16.xlsx",
                          sheet = 'Daily Positive Feedback from...')

pos.metrics$DayTotal <- rowSums(Filter(is.numeric, pos.metrics))


pos.metrics$Description <- NULL

pos.metrics <- melt(pos.metrics,id.vars = 'Date')

colnames(pos.metrics) <- c("Date","Type", "Count")

pos.metrics$Date <- as.Date(pos.metrics$Date)

ggplot(pos.metrics, aes(fill=Type, y=Count, x=Date)) + 
  geom_bar( stat="identity")+ ggtitle("Count of Positive Feedback by Date ")+ylab("Count")+
  scale_x_date(breaks = pretty(likes$Date, n = 6),labels = date_format("%b-%d"))









#Daily: The number of times people have given negative feedback to your Page, by type. (Total Count)

neg.metrics <- read_excel("Desktop/Projects/Facebook Mining/Facebook Insights Data Export - Corey Stewart - 2018-07-13 (2).xlsx",
                          sheet = 'Daily Negative Feedback From...')



#############################



#Lifetime: The number of unique people who created a story about your Page post by interacting with it. (Unique Users)

users.eng <- read_excel("Desktop/Projects/Facebook Mining/Facebook Insights Data Export (Video Posts) - Corey Stewart - 2018-07-13 (1).xlsx",
                          sheet = 'Lifetime Talking About This...')
users.eng$Date <- as.Date(users.eng$Posted)

df <-  select(users.eng,Date,comment,like,share)

df[is.na(df)] <- 0

df <- melt(df,id.vars = 'Date')

colnames(df) <- c("Date","Engagement", "Count")


ggplot(df, aes(fill=Engagement, y=Count, x=Date)) + 
  geom_bar(position = "stack", stat="identity")+ ggtitle(" Laugh  ")+
  scale_x_date(labels=date_format("%b-%d"),
               limits = as.Date(c('2018-06-20','2018-07-14')))




plot(users.eng$Hour,users.eng$Total, main= "Does Hour affect Engagement", xlab="Hour", ylab= " Total" )






#Lifetime: The number of stories created about your Page post, by action type. (Total Count)

count.eng <- read_excel("Desktop/Projects/Facebook Mining/Facebook Insights Data Export (Video Posts) - Corey Stewart - 2018-07-13 (2).xlsx",
                        sheet = 'Lifetime Post Stories by act...')



#####################


###Lifetime video view time (in MS) by Top Audience (Age and Gender).

age.gen.eng <- read_excel("Desktop/Projects/Facebook Mining/Facebook Insights Data Export (Video Posts) - Corey Stewart - 2018-08-13.xlsx",
                        sheet = 'Lifetime Video View Time...002')






#########

vid.met <- read_excel("Desktop/Projects/Facebook Mining/Facebook Insights Data Export (Video Posts) - Corey Stewart - 2018-07-13 (2).xlsx",
                        sheet = 'Video Metrics Total vs. Unique')






  