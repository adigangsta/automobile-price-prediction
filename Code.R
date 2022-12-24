#import data
automobile=read.csv('/Users/adityodasgupta/Documents/McGill/MGSC661/Automobile_data.csv')

## DATA PREPROCESSING

#drop engine loc and normalized losses
automobile=automobile[,c(-2,-9)]

#replace categorical by numerical
automobile$num.of.doors[automobile$num.of.doors=='two']=as.integer(2)
automobile$num.of.doors[automobile$num.of.doors=='four']=as.integer(4)

#convert to integerr
automobile$num.of.doors=as.integer(automobile$num.of.doors)

#missing data imputation for num of doors

automobile[is.na(automobile$num.of.doors),]


library(ggplot2)


#check mode for sedans
ggplot(automobile[automobile$body.style=='sedan',], aes(x=num.of.doors)) + geom_bar()

find_mode <- function(x) {
  u <- unique(x)
  tab <- tabulate(match(x, u))
  u[tab == max(tab)]
}

#impute missing data using mode
automobile$num.of.doors[is.na(automobile$num.of.doors)]=
  find_mode(automobile[automobile$body.style=='sedan','num.of.doors'])


#replace categorical by numerical
automobile$num.of.cylinders[automobile$num.of.cylinders=='eight']=8
automobile$num.of.cylinders[automobile$num.of.cylinders=='five']=5
automobile$num.of.cylinders[automobile$num.of.cylinders=='four']=4
automobile$num.of.cylinders[automobile$num.of.cylinders=='six']=6
automobile$num.of.cylinders[automobile$num.of.cylinders=='three']=3
automobile$num.of.cylinders[automobile$num.of.cylinders=='twelve']=12
automobile$num.of.cylinders[automobile$num.of.cylinders=='two']=2

#convert to integer
automobile$num.of.cylinders=as.integer(automobile$num.of.cylinders)


#convert to float
automobile$bore=as.double(automobile$bore)
automobile$stroke=as.double(automobile$stroke)
automobile$horsepower=as.double(automobile$horsepower)
automobile$peak.rpm=as.double(automobile$peak.rpm)
automobile$price=as.double(automobile$price)


#missing data imputation for bore and stroke
subset(automobile, is.na(automobile$bore))
subset(automobile, is.na(automobile$stroke))


#check distribution of bore and stroke
a=ggplot(automobile[automobile$body.style=='hatchback',], aes(x=bore)) + geom_histogram()
b=ggplot(automobile[automobile$body.style=='hatchback',], aes(x=stroke)) + geom_histogram()

#impute using mean
automobile$bore[is.na(automobile$bore)]=
  mean(automobile[automobile$body.style=='hatchback',c("bore")],na.rm=TRUE)

automobile$stroke[is.na(automobile$stroke)]=
mean(automobile[automobile$body.style=='hatchback',c("stroke")],na.rm=TRUE)

#missing data imputation for horsepower and peak-rpm
subset(automobile, is.na(automobile$horsepower))
subset(automobile, is.na(automobile$peak.rpm))

#check distribution of horsepower and peak-rpm
c=ggplot(automobile[automobile$body.style=='wagon',], aes(x=horsepower)) + geom_histogram()+xlab('horsepower-wagon')
d=ggplot(automobile[automobile$body.style=='hatchback',], aes(x=horsepower)) + geom_histogram()+xlab('horsepower-hatchback')

e=ggplot(automobile[automobile$body.style=='wagon',], aes(x=peak.rpm)) + geom_histogram()+xlab('peak.rpm-wagon')
f=ggplot(automobile[automobile$body.style=='hatchback',], aes(x=peak.rpm)) + geom_histogram()+xlab('peak.rpm-hatchback')

#impute using median
automobile$horsepower[is.na(automobile$horsepower) & automobile$body.style=='wagon']=
  median(automobile[automobile$body.style=='wagon',c("horsepower")],na.rm=TRUE)

automobile$horsepower[is.na(automobile$horsepower) & automobile$body.style=='hatchback']=
  median(automobile[automobile$body.style=='hatchback',c("horsepower")],na.rm=TRUE)


automobile$peak.rpm[is.na(automobile$peak.rpm) & automobile$body.style=='wagon']=
  median(automobile[automobile$body.style=='wagon',c("peak.rpm")],na.rm=TRUE)

automobile$peak.rpm[is.na(automobile$peak.rpm) & automobile$body.style=='hatchback']=
  median(automobile[automobile$body.style=='hatchback',c("peak.rpm")],na.rm=TRUE)

#missing data imputation for price
subset(automobile, is.na(automobile$price))

#check distribution of price
g=ggplot(automobile[automobile$body.style=='sedan',], aes(x=price)) + geom_histogram()+xlab('price-sedan')
h=ggplot(automobile[automobile$body.style=='hatchback',], aes(x=price)) + geom_histogram()+xlab('price-hatchback')

#impute using mean
automobile$price[is.na(automobile$price) & automobile$body.style=='sedan']=
  mean(automobile[automobile$body.style=='sedan',c("price")],na.rm=TRUE)

automobile$price[is.na(automobile$price) & automobile$body.style=='hatchback']=
  mean(automobile[automobile$body.style=='hatchback',c("price")],na.rm=TRUE)

qplot<-list()

qplot[[1]] <- ggplotGrob(a)
qplot[[2]] <- ggplotGrob(b)
qplot[[3]] <- ggplotGrob(c)
qplot[[4]] <- ggplotGrob(d)
qplot[[5]] <- ggplotGrob(e)
qplot[[6]] <- ggplotGrob(f)
qplot[[7]] <- ggplotGrob(g)
qplot[[8]] <- ggplotGrob(h)

grid.arrange(grobs=qplot,ncol=2,nrow=4)

## EXPLORATORY DATA ANALYSIS



#find all numerica columns and get scatter plots against price
num_col=names(automobile)[sapply(automobile, is.numeric)]

num_col=num_col[c(-17)]


j=1

splot <- list()
install.packages(grid)
library(gridExtra)

for (i in num_col[1:8]){
  g=ggplot(automobile, aes(x =automobile[,i] , y =price)) + geom_point()+xlab(i)+ylab('price')
  splot[[i]] <- ggplotGrob(g)
  j=j+1
}

grid.arrange(grobs=splot,ncol=2,nrow=4)


#get residual plots for all
library(car)

attach(automobile)

reg4=lm(price~symboling+num.of.doors+wheel.base+length+width+height+curb.weight+num.of.cylinders)
residualPlots(reg4)

reg5=lm(price~engine.size+bore+stroke+compression.ratio+horsepower+peak.rpm+city.mpg+highway.mpg)
residualPlots(reg5)

residualPlots(reg4)

summary(residualPlots(reg4))

install.packages('stargazer')
library(stargazer)

stargazer(residualPlots(reg5))

## MODELLING

library(tree)
library(rpart)
library(rpart.plot)

#install.packages("gbm")
library(gbm)
set.seed (1)

attach(automobile)

library(caTools)

#convert categorical to factors
automobile$make=as.factor(automobile$make)
automobile$fuel.type=as.factor(automobile$fuel.type)
automobile$aspiration=as.factor(automobile$aspiration)
automobile$fuel.system=as.factor(automobile$fuel.system)
automobile$engine.type=as.factor(automobile$engine.type)
automobile$drive.wheels =as.factor(automobile$drive.wheels )
automobile$body.style=as.factor(automobile$body.style)

#train test split
sample=sample.split(automobile$price, SplitRatio=0.8)
train_set=subset(automobile, sample==TRUE)
test_set=subset(automobile, sample==FALSE)

#finding the best parameters for the gbt model

df=data.frame('NA','NA','NA','NA','NA')
colnames(df)<-c('depth','ntrees','shrinkage','min_split','mse')


for (i in c(1, 3, 5))
{
  for (j in c(25,50,100))
{
    for (k in c(0.1,0.01, 0.001))
{
      for (l in c(5,10,15))
{
        boosted <- gbm(price~ . , data=train_set,
                       interaction.depth=i,
                       n.trees = j,
                       shrinkage=k,
                       n.minobsinnode=l,
                       cv.folds=4,
                       verbose=TRUE,
                       distribution = 'gaussian'
                       )
        
        pred_y = predict.gbm(boosted, test_set[,-24])
        residuals = test_set$price - pred_y
        RMSE = sqrt(mean(residuals^2)) 
        df = rbind(df, c(i,j,k,l,RMSE))
      }
    }
  }
}

#best model after parameter tuning
boosted <- gbm(price~ . , data=train_set,
               interaction.depth=5,
               n.trees = 100,
               shrinkage=0.01,
               n.minobsinnode=5,
               cv.folds=4,
               verbose=TRUE,
               distribution = 'gaussian'
)

summary(boosted)
boosted


#check metrics for performance
cor(boosted$fit, train_set$price)^2
mean((train_set$price - boosted$fit)^2)

pred_y = predict.gbm(boosted, test_set[,-24])

residuals = test_set$price - pred_y
mean(residuals^2)

cor(pred_y, test_set$price)^2

attach(automobile)

automobile$predicted_price=c(predict.gbm(boosted, automobile[,-24]))


##PCA


#subset relevant columns

pca_data=automobile[,c("wheel.base","length","width","height","curb.weight","num.of.cylinders",
                       "engine.size","compression.ratio","horsepower","peak.rpm","city.mpg",
                       "highway.mpg","predicted_price"
                       )]

#get the principal components and visualize scree plot
pca=prcomp(pca_data, scale=TRUE)

pve=(pca$sdev^2)/sum(pca$sdev^2)
par(mfrow=c(1,2))
plot(pve, ylim=c(0,1))
plot(cumsum(pve), ylim=c(0,1))

cumsum(pve)

library(ggfortify)

autoplot(pca, data = pca_data, loadings = TRUE, loadings.label = TRUE )

#keep only 5 Principal Components
auto_transform = as.data.frame(-pca$x[,1:5])

autoplot(pca, data = auto_transform, loadings = TRUE, loadings.label = TRUE ) 

##Clustering
install.packages("tidyverse")

library(tidyverse)
# Use map_dbl to run many models with varying value of k (centers)
tot_withinss <- map_dbl(1:10,  function(k){
  model <- kmeans(x = auto_transform, centers = k)
  model$tot.withinss
})
tot_withinss
# Generate a data frame containing both k and tot_withinss
elbow_df <- data.frame(
  k = 1:10,
  tot_withinss = tot_withinss
)


# Plot the elbow plot
ggplot(elbow_df, aes(x = k, y = tot_withinss)) +
  geom_line() + geom_point()+
  scale_x_continuous(breaks = 1:10)


km.5=kmeans(auto_transform, 5)         

auto_transform$cluster=as.factor(km.5$cluster)

#plot the clusters between PC1 & PC2
autoplot(pca, data = auto_transform, loadings = TRUE, loadings.label = TRUE ,
         colour=auto_transform$cluster) 



#plot the clusters between PC1 & PC2 &PC3
install.packages('plotly')
library(plotly)

attach(auto_transform)

plot_ly(x=PC1, y=PC2, z=PC3, type="scatter3d", mode="markers", color=cluster)

#get the cluster centers
km.5

#look at the 5 principal components for inferences
pca

#Put clusters back in main data set
automobile$cluster=as.factor(km.5$cluster)

#write_csv(automobile,file='/Users/adityodasgupta/Documents/McGill/MGSC661/export.csv')

#get boxplot for all predictors

cluster_cols=names(pca_data)

gplot<-list()
for (i in cluster_cols[9:13]){
  
  g=ggplot(automobile, aes(y=automobile[,i], color=cluster)) +
    geom_boxplot() +ylab(i)
  gplot[[i]] <- ggplotGrob(g)
  j=j+1
  
}

grid.arrange(grobs=gplot,ncol=2,nrow=3)

