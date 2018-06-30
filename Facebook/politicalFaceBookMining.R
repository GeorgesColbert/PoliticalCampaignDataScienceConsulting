#Facebook 


library(readr)
library(readxl)

Post.Acts <- read_excel("Desktop/Projects/Political Tweets/Facebook/Post Insights.xlsx",
                                                                                           sheet = 'Lifetime Post Stories by act...')


ggplot(Post.Acts) 
Post.Acts$Date <- as.Date(Post.Acts$Posted)
Post.Acts$Date <- as.character(Post.Acts$Posted)

ggplot(Post.Acts, aes(x = Date, y =like)) + geom_bar(stat="identity", width=0.5)

Post.Acts$`Post ID`



specie=c(rep("sorgho" , 3) , rep("poacee" , 3) , rep("banana" , 3) , rep("triticum" , 3) )
condition=rep(c("normal" , "stress" , "Nitrogen") , 4)
value=abs(rnorm(12 , 0 , 15))
data=data.frame(specie,condition,value)


Post.Eng <- select(Post.Acts, Date, share, like, comment)

Post.Eng <- melt(Post.Eng, id.vars = 'Date')

 # Stacked Percent
  
  
  ggplot(Post.Eng, aes(fill=variable, y=value, x=Date)) + 
  geom_bar( stat="identity", position="fill",width=0.5)