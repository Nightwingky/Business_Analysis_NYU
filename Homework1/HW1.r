#PartI: Load data via Github
url<-"https://raw.githubusercontent.com/jcbonilla/BusinessAnalytics/master/BAData/zagat.CSV"
data<-read.csv(url, header=TRUE,stringsAsFactors=FALSE)
data

#Part II: Compute Summary Statistics for price, decor, service, and food
summary(data$Price)
summary(data$Decor)
summary(data$Service)
summary(data$Food)

#Part III: Segmentation of restaurants
#We believe that to judge the quality of a restaurant, the food and the price are the first factors to consider
#and then the service and the decoration.
#Assume the computation of score follows 0.4 food + 0.3 price + 0.2 service + 0.1 decor.
score<-0.4*data$Price+0.3*data$Food+0.2*data$Service+0.1*data$Decor
score

#Combine the score column to the original dataset
frame<-data.frame(data,score)
frame

#Compute the mean value of Score
m<-mean(score)
m

#Segmentation
#Restaurants that have scores above average
above=subset(frame,frame$score > m)
above

#Count the restaurants that have scores above average
num.above=length(above$Name)
paste("Restaurants that have scores above average: ", num.above)

#Restaurants that have scores below average
below=subset(frame,frame$score < m)
below

#Count the restaurants that have scores below average
num.below=length(below$Name)
paste("Restaurants that have scores below average: ", num.below)

#Part IV: Compute and compare the summary statistics for 2 segments
#Food
summary(above$Food)
summary(below$Food)
boxplot(above$Food,below$Food,main="Food Comparison in Two Different Segments",names=c(
  "above","below"),col=c("pink", "purple"),
  horizontal=TRUE,notch=TRUE,xlab="Food",ylab="Group")

#Decor
summary(above$Decor)
summary(below$Decor)
boxplot(above$Decor,below$Decor,main="Decor Comparison in Two Different Segments",names
        =c("above","below"),col=c("pink", "purple"),
        horizontal=TRUE,notch=TRUE,xlab="Decor",ylab="Group")

#Service
summary(above$Service)
summary(below$Service)
boxplot(above$Service,below$Service,main="Service Comparison in Two Different Segments"
        ,names=c("above","below"),col=c("pink","purple"),
        horizontal=TRUE,notch=TRUE,xlab="Service",ylab="Group")

#Price
summary(above$Price)
summary(below$Price)
boxplot(above$Price,below$Price,main="Price Comparison in Two Different Segments"
        ,names=c("above","below"),col=c("pink","purple"),
        horizontal=TRUE,notch=TRUE,xlab="Price",ylab="Group")


#Part V: Extend Part --- Scatterplot Matrix 
pairs(above[,2:5], upper.panel=NULL)
pairs(below[,2:5], upper.panel=NULL)

