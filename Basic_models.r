---
title: "Assignment 2-INFO424"
author: "Indu Sharma"
date: "28/04/2020"
output:
  html_document:
    df_print: paged
  pdf_document: default
  word_document: default
---

```{r setup, include=FALSE}

setwd("~/Desktop/INFO424/Assingment 2/Assn2(6)")
.libPaths("~/Desktop/INFO424")
```

```{r,warning=FALSE,message=FALSE}
#load these libraries
library(ggplot2)
library(forecast)
library(tseries)
library(tibble)
library(corrplot)
library(gclus)
library(grid)
library(gridExtra)
library(ggpubr)
library(rpart)
library(rpart.plot)
```
### LINEAR MODELLING (10 marks)

#### Task 1
**Question**: Construct a linear model (model 1) using the data loaded from the comma-separated file
pdata.csv, assuming that you want to predict y given x and that the model you produce
should go through the origin (i.e. have no intercept). 

```{r,warning=FALSE,message=FALSE}
pdata<-read.csv ("pdata.csv", header= T)
attach(pdata)
xsq<- x^2
xcub<- x^3
xquar<- x^4
question1_mdl1<-lm(y~x,data = pdata)
question2_mdl1<-lm(y~x+xsq+xcub, data = pdata)

pdata$mdl1<-predict(question1_mdl1)
pdata$mdl2<-predict(question2_mdl1)

ggplot(pdata, aes(x = x, y = y)) +  geom_point() +
geom_line(aes(y=mdl1, color="mdl1")) +
geom_line(aes(y=mdl2, color="mdl2"))+ 
scale_color_manual(name = "Model",values = c("mdl1" = "blue", 
                                             "mdl2" = "yellow")) +
ggtitle("Fitting two types of linear models")

```

**Question**: State the formula that you used and
produce a plot showing the (x,y) values and the model predictions.

Ans: Formula = lm(y~x)

#### Task 2
Use additive (linear) modelling to estimate the coefficient values. Show the
summary statement for this model, produce a plot showing the original data and the
predictions using this model for the given x values (Figure 1 is one possible example
answer). 
```{r}
summary(question2_mdl1)
```

Q: Finally, justify which variables are important in model 2 and state the final model
that you would use for representing the relationships in this data.
Ans: Since x and xcub have significant p-values we can conclude that these two variable are important in model2. The variable xsq shows no significant pvalue. Hence we can re-write the formula as:

Formula = lm(y ~ x + xcub)


### BIKE SHARING – TIME SERIES, DECISION TREES & LINEAR MODELS (90 MARKS)
#### Task 1

```{r,warning=FALSE,message=FALSE}

#read the bike_sharing CSV file
bike_shareCSV<-read.csv("day.csv", header = TRUE)

bike_shareCSV$dteday = as.Date(bike_shareCSV$dteday,format="%d/%m/%Y")  # Convert date column to proper Date class

ggplot(bike_shareCSV, aes(dteday, bikes)) + 
  geom_line() + 
  scale_x_date('month')  + 
  ylab("Daily Bike Checkouts") + 
  xlab("") +
  ggtitle ("Seasonality of the bikes data (before cleaning)")

#clean the data
count_ts = ts(bike_shareCSV[, c('bikes')])
bike_shareCSV$clean_bikes = tsclean(count_ts) #creats a new column where only the cleaned data exsits
ggplot() +
  geom_line(data = bike_shareCSV, aes(x = dteday, y = clean_bikes)) + 
  ylab('Cleaned Bicycle Count') +
  ggtitle ("Seasonality of the bikes data (after cleaning)")
```
**Question**: Describe the data and present summary visualisations/statistics for the bike sharing count
(bikes) over weekdays/weekends, holidays, weather patterns, etc. This should also include
comments regarding the seasonality of the bikes data (see lab), any patterns that you
discover, outliers for the response, and any other information of interest. Note that the
explanatory “instant” should be removed prior to doing this assessment since this is just an
ID for each example (row).

Ans: Initial observation on seasonality of bike_sharing data suggests two main points:
1) In the year 2012 bike_sharing was much higher compared to year 2011
2) Bike sharing peaks during fall season in both years, owing the favourable climating conditions during that period. 
[Answer continues in the following sections]

**Question**: Clean the data – remove outliers based on the bikes reponse and discuss why these
measurements have been removed.
Ans: *tsclean()* is an inbuilt function in the package forecast. It identifies outliers and replaces them with values closer to the trend. For replacement linear interpolation is used where the new value is estimated based in the adjacent known values (straight line fit between neighbouring datapoints is drawn).


```{r,echo=FALSE,width=7,fig.height=4,}
dist.table <- function(d, response.var = ncol(d))  
{
    d <- scale(d) # scale data
	d.dist <- dist(d[,-response.var])  # distance all X values	
	d.resp <- dist(d[,response.var])
	
	d.dist <- (d.dist-min(d.dist))/(max(d.dist)-min(d.dist))
	d.resp <- (d.resp-min(d.resp))/(max(d.resp)-min(d.resp))
	
	data.frame(cbind(d.dist,d.resp))
}

#remove two columns instance and date and 14th and 15th column which only gives information about casual and registered usage. The last column is the sum of casual and registered usage column. Also remove uncleaned bikes column 
bike_shareCSV_minus4cols<- bike_shareCSV[,-c(1,2,14,15,16)]

#run the dist.table function
d <- dist.table(bike_shareCSV_minus4cols, response.var = ncol(bike_shareCSV_minus4cols))
plot(x=d$d.dist, y = d$d.resp,xlab="Normalised Distance in Feature Space",
                              ylab="Normalised Distance in Response Space",cex=0.5)

#Fine correlation between the explanatories
summary(bike_shareCSV)
#Correlation matrix and pairwise plot
round(cor(bike_shareCSV_minus4cols),2)
corrplot(cor(bike_shareCSV_minus4cols))

bike.cor <- abs(cor(bike_shareCSV_minus4cols))  # Take absolute value of correlation
bike.colors <- dmat.color(bike.cor)
bike.order <- order.single(cor(bike_shareCSV_minus4cols))  # Order by correlation
```

Ans: The above two graphs show:

1) Normalised distance plot:

The normalised distance plot shows that this bike_sharing dataset is reasonably good to model the y-response. The fact that the top left side of the graph is fairly empty suggests, that the explanatories which are similar tend to respond similarly. But one caveat is that explanatories dissimilar in distance can also tend to respond similarly. But overall this plot suggests that the y-response can be predicted from the cumulative behaviour of all the explanatories. 

2) Correlation plot:

This plot shows many of the variables associated with the climatic conditions correlate well with seasonality and also with y-response variable. However, variables associated with non-business days do not significantly influence bike_sharing response and neither do they show any kind of correlation with other variables.

This suggests, that climatic conditions (which is largely a function of change in seasons in a year) influences whether people opt to hire bikes in that particular day. Another strong variable which seem to have influenced y-response is the year in which the bikes were shared. This may have been influenced by the fact that in the first year (year 2011) of this scheme, people might not have been aware of such facilities. As the scheme got popularity, the number of bike_sharing increased the following year (year 2012).

```{r, warning= FALSE, error=FALSE, message=FALSE,width=4,fig.height=4,}

Pri_comparison<- ggplot(data=bike_shareCSV, aes(x = temp, y = clean_bikes)) 
Pri_comparison1<- Pri_comparison + geom_point(aes(color=mnth)) +ylab("Total bikes shared")
Pri_comparison2<-Pri_comparison+ geom_point(aes(color=season))
Pri_comparison3<-Pri_comparison+ geom_point(aes(color=yr))
Pri_comparison4<-Pri_comparison+ geom_point(aes(color=weathersit))
Pri_comparison5<-Pri_comparison+ geom_point(aes(color=windspeed))

ggarrange(Pri_comparison1, Pri_comparison2, Pri_comparison3, Pri_comparison4, Pri_comparison5,
          labels = c("A", "B", "C", "D", "E"),
          ncol = 3, nrow = 2, font.label = list(size = 10, color = "black"))

  Sec_comparison<- ggplot(data=bike_shareCSV, aes(x = mnth, y = clean_bikes)) 
  Sec_comparison1<-Sec_comparison + geom_point(aes(color=temp))# during 5-8th month the temperature seems ideal to go out on a bike ride
  Sec_comparison2<-Sec_comparison + geom_point(aes(color=weathersit)) #outliers are because of the adverse weather
  Sec_comparison3<-Sec_comparison + geom_point(aes(color=yr))
  Sec_comparison4<-Sec_comparison + geom_point(aes(color=windspeed))
  ggarrange(Sec_comparison1, Sec_comparison2, Sec_comparison3, Sec_comparison4,
          labels = c("A", "B", "C", "D"),
          ncol =2, nrow = 2, font.label = list(size = 10, color = "black"))

Ter_comparison <- ggplot(data=bike_shareCSV, aes(x = weekday, y = clean_bikes)) 
Ter_comparison1 <- Ter_comparison + geom_point(aes(color=holiday))
Ter_comparison2 <- Ter_comparison + geom_point(aes(color=season))

ggarrange(Ter_comparison1, Ter_comparison2,
            labels = c("A", "B"),
            ncol = 2, font.label = list(size = 10, color = "black"))
```


The above three set of graphs aim to show responses of some of the important explanatories.  

First Set:

Shows how climattic conditions and the specific year at which bike was shared influences y-response. Graph C clearly shows the difference in y-response between year 2011 and 2012. Since months and seasons impact atmospheric temperature (Graph A and B), they too impact y-response. And finally adverse climatic conditions (graph D and E) also have influence on y-response. 

Second set:

In this set we can see some of the variables which influence the outliers. For example sudden change in climatic conditions (Graph B and D) can cause disruption in the overall yresponse treand.

Third set:

This set shows that non-business days/working days do not impact bike sharing, as compared to the impact due to climatic conditions. 

#### Task 2- Clean the data
Ans: The answer to this question is embedded in task 1. 

#### Task 3- Build a linear model 
**Question** :Build a linear model for predicting count. Do not use the explanatories instant, dteday,
casual, registered. Do not convert any variables to factors.
```{r, message=FALSE,warning=FALSE}

#Convert some of the columns to factors
bike_shareCSV_factor<-bike_shareCSV_minus4cols
bike_shareCSV_factor$holiday <- as.factor(bike_shareCSV_factor$holiday)
bike_shareCSV_factor$season <- factor(format(bike_shareCSV_factor$season),levels = c("1", "2", "3", "4") , labels = c("Winter","Spring", "Summer", "Fall"))
bike_shareCSV_factor$yr <- factor(format(bike_shareCSV_factor$yr),levels = c("0", "1") , labels = c("year_2011","year_2012"))
bike_shareCSV_factor$mnth <- as.factor(bike_shareCSV_factor$mnth)
bike_shareCSV_factor$weekday <- factor(format(bike_shareCSV_factor$weekday),levels = c("0", "1", "2", "3", "4", "5","6") , labels = c("Sun","Mon", "Tue", "Wed", "Thu", "Fri", "Sat"))
bike_shareCSV_factor$workingday <- factor(format(bike_shareCSV_factor$workingday),levels = c("0", "1") , labels = c("Not working","Working"))
bike_shareCSV_factor$weathersit <- factor(format(bike_shareCSV_factor$weathersit),levels = c("1", "2", "3") , labels = c("Clear","Misty", "Light_Snow"))
#convert clean_bike to numeric
bike_shareCSV_factor$clean_bikes <- as.numeric(bike_shareCSV_factor$clean_bikes)
bike_shareCSV_minus4cols$clean_bikes <- as.numeric(bike_shareCSV_minus4cols$clean_bikes)

########################################################################################################################
#inspect each of the explanatories with clean_bikes column
a2<-ggplot(bike_shareCSV_factor) +  
  geom_point(aes(x = season, y = clean_bikes))
b2<-ggplot(bike_shareCSV_factor) +  
  geom_point(aes(x = yr, y = clean_bikes))
c2<-ggplot(bike_shareCSV_factor) +  
  geom_point(aes(x = mnth, y = clean_bikes))
d2<-ggplot(bike_shareCSV_factor) +  
  geom_point(aes(x = holiday, y = clean_bikes))
e2<-ggplot(bike_shareCSV_factor) +  
  geom_point(aes(x = weekday, y = clean_bikes))
f2<-ggplot(bike_shareCSV_factor) +  
  geom_point(aes(x = workingday, y = clean_bikes))
g2<-ggplot(bike_shareCSV_factor) +  
  geom_point(aes(x = weathersit, y = clean_bikes))
h2<-ggplot(bike_shareCSV_factor) +  
  geom_point(aes(x = temp, y = clean_bikes))
i2<-ggplot(bike_shareCSV_factor) +  
  geom_point(aes(x = atemp, y = clean_bikes))
j2<-ggplot(bike_shareCSV_factor) +  
  geom_point(aes(x = hum, y = clean_bikes))
k2<-ggplot(bike_shareCSV_factor) +  
  geom_point(aes(x = windspeed, y = clean_bikes))

grid.arrange(a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2, ncol=4,nrow=3, top= "Comparison of all explanatories in factorised bike_share dataset with clean_bikes yresponse")
########################################################################################################################
#write a function to replicate traning/test data split 50 times

assess.linearModel<-function(dataFrame, LMformula,yResponse.col){
sampling<-sample(c(1:nrow(dataFrame)),size=(0.7*nrow(dataFrame)),replace = FALSE, prob = NULL)
train_linearModel <- dataFrame[sampling,]
test_linearModel  <- dataFrame[-sampling,]

#Build a linear model
mdl<-lm(LMformula, data=dataFrame)
yhat <- predict(mdl, newdata=test_linearModel) #predicting y-response

#Calculate the R2 for assessing the linear model
y <- test_linearModel[,yResponse.col]
rSquareVector.linearModel <- 1 - (sum((y - yhat)^2) / sum((y - mean(y))^2)) # R-squared measure
  
#return value  
rSquareVector.linearModel
}
########################################################################################################################
#Call the linear function on both the factorised and non-factorised bike_share data

rSquareVector1<-replicate(n=50,assess.linearModel(dataFrame= bike_shareCSV_minus4cols, LMformula = clean_bikes~season+yr+mnth+holiday+weekday+workingday+weathersit+temp+atemp+hum+windspeed,12) )#non-factorised
rSquareVector2<-replicate(n=50,assess.linearModel(dataFrame= bike_shareCSV_factor, LMformula = clean_bikes~season+yr+mnth+holiday+weekday+workingday+weathersit+temp+atemp+hum+windspeed,12) )#factorised


boxPlotLinearModel_bikes_nonFactorised<-ggplot() +
  geom_boxplot(aes (x="nonFactorised linear model", y=rSquareVector1))+
  ylab("") + ylim(0.75,0.9)

  
boxPlotLinearModel_bikes_Factorised<-ggplot() +
  geom_boxplot(aes (x="Factorised linear model", y=rSquareVector2))+
  ylab("") + ylim(0.75,0.9)
grid.arrange(boxPlotLinearModel_bikes_nonFactorised,boxPlotLinearModel_bikes_Factorised, ncol=2)

fitall_LM<-lm(clean_bikes~season+yr+mnth+holiday+weekday+workingday+weathersit+temp+atemp+hum+windspeed, data=bike_shareCSV_factor)
summary(fitall_LM)
```

**Question**
Now consider whether any of the explanatories (expect for those already removed) should be treated as factors. Discuss which variables you have selected to be factors, and justify your decisions. Rerun the model with these variables now as factors and compare the results with the
previous linear model. Discuss/explain why there are differences (if any).

Ans:
Variables related to defining climatic conditions are continuous as they are holding numeric data. The remaining set of variables are discrete as they hold only particular values (or specific range of integers). These discrete variables (includig season, year, month, holiday, weekday, workingday, weathersit) have all been converted to factors. 

The boxplot above shows the difference in the r2 values between non-factorised and factorised data set. There seems to be a slight improvement in r2 value in factorised dataset. Overall, the linear model having a mean r2 value of 0.855 (in factorised dataset) shows that this type of model can reliably predict the y-response from the list of explanatories provided. 

In categorical variable, each of the values are independent to each other and are no way connected with each other linearly. For example, the season variable has 4 distinct values which correspond to the 4 seasons. Whereas the air temperature variable, each of its value is measured and take on any kind of value within a range. Since each of the values in categorical variables are discrete and independent, every value here gets a seperate coefficient when doing linear regression. However, in continuous variable the entire vector of data gets a single coefficient value. For example in the summary (fitall_LM) (see the Rconsole above) each of the values in the catogorical variables have coefficent value, but the continuous variable have one single coefficient value. This tells us that the individual values in the catogorical variables can independently influence linear regression, irrespective of how other values are acting within the same category. 

Thus, converting categorical values to factors tends to better predict y-values, than considering them as numeric/integer variables. 

#### Task 4- Build a decision tree model

**Question** Build a decision tree model for the data, doing 50 training/test splits and error measurements as before, setting maxdepth from 1 to 20. In other words, for each maxdepth you do 50 training/test splits. INCLUDE the R CODE FOR THIS OPERATION and a figure showing how R2 varies with depth.
```{r}
#function to calculate r2 for decision tree model

assess.decisionTreeModel<-function(dataFrame, LMformula, yResponse.col,MaxDepth){
sampling<-sample(c(1:nrow(dataFrame)),size=(0.7*nrow(dataFrame)),replace = FALSE, prob = NULL)
train_decisionTreeModel <- dataFrame[sampling,]
test_decisionTreeModel  <- dataFrame[-sampling,]

#Build a decision tree model
     rp <- rpart(LMformula, data = train_decisionTreeModel,
     # ~. means that it uses all the explanatories in train_mtCars data set
     # if have explanatories which are factors, then we can select only those which we are interested in. 
              control=rpart.control(maxdepth=MaxDepth,
                                    minsplit=2,
                                    minbucket=2,
                                    cp=0))
     yhat_decisionTree <- predict(rp,
                               newdata=test_decisionTreeModel,
                               type="vector")
#Calculate the R2 for assessing the decision tree model
y <- test_decisionTreeModel[,yResponse.col]
rSquareVector.decisionTreeModel <- 1 - (sum((y -  yhat_decisionTree)^2) / sum((y - mean(y))^2)) # R-squared measure
  
#return value  
rSquareVector.decisionTreeModel
}

#####################################################################################################################################

#Call the decisionTreeModel function
Data_sampling = 50
maxdepth_intervals = 20

rSquare_decisiontree <- matrix(ncol=maxdepth_intervals, nrow=Data_sampling)

for(i in 1:maxdepth_intervals){
rSquare_decisiontree[,i]<-replicate(n=50, assess.decisionTreeModel(dataFrame= bike_shareCSV_factor, LMformula = clean_bikes~season+yr+mnth+holiday+weekday+workingday+weathersit+temp+atemp+hum+windspeed, yResponse.col=12, MaxDepth = i))
   }
boxplot(rSquare_decisiontree, xlab="Depth", ylab = "rSquare")
###########################################################################################################################################
#Compare linear model (rSquareVector2-factorised linear model) with decision tree (use max depth of 5)
library(ggplot2)
library(gridExtra)
rSquare_decisiontree <- as.data.frame(rSquare_decisiontree)

boxPlotLinearModel_bikes_Factorised<-ggplot() +
  geom_boxplot(aes (x="Factorised linear model", y=rSquareVector2))+
  ylab("")

boxPlotDecisionTreeModel_bikes<-ggplot() +
  geom_boxplot(aes (x="Decision Tree Model", y=rSquare_decisiontree$V4))+
  ylab("")
grid.arrange(boxPlotLinearModel_bikes_Factorised,boxPlotDecisionTreeModel_bikes, ncol=2)
```

**Question**: Compare the results to the linear model, and discuss what maxdepth of the tree is suitable to be
used to build the decision tree over all of the data.

Ans:
Based on the first boxplot above, we can say for this bike_sharing dataset, ideal max_depth to build a decision tree is about 5. Beyond this depth the r2 value does not seems to improve.

Based on the second boxplot above, factorised linear model seems to fare better than the decision tree model. However the difference in r2 is subtle, and it will be difficult to conclusive judge which is the supirior model in predicting y-response.  Both the models indicate that they can accurately predict the y-response ~80% of the time.


# Task 5- Fit a single (stepwise) additive (linear) model

```{r}
#decision tree for entire data set

decisionTree_mdl <- rpart(clean_bikes~season+yr+mnth+holiday+weekday+workingday+weathersit+temp+atemp+hum+windspeed, data = bike_shareCSV_factor,
     # ~. means that it uses all the explanatories in train_mtCars data set
     # if have explanatories which are factors, then we can select only those which we are interested in. 
              control=rpart.control(maxdepth=5,
                                    minsplit=2,
                                    minbucket=2,
                                    cp=0))


#linear model for entire data set
linear_mdl1<-lm(clean_bikes ~ season + yr + mnth + holiday + weekday + workingday + weathersit + temp + atemp + hum + windspeed, data=bike_shareCSV_factor)
summary(linear_mdl1)
prp(decisionTree_mdl)
```


```{r}
step_wise_lm<-step(linear_mdl1, direction = "both")
summary(step_wise_lm)
```


Looking into the summary of the linear model, we can infer that regression is possible based on the high adjusted R-squared value (0.856) and low p-value (2.2e-16). 
Using these significant variable lets re-run the linear model step wise regression in both the forward and backward direction. Lower the Akaike information criterion (AIC), the better is the formula for building linear regression. AIC provides an indication as to which combination of variables produce the most accurate y-response for linear regression. 

As seen from the above results, step wise regression in both the direction dosen't seem to improve the r2 value (it is still at 0.856) despite removing the varible workingday. This suggests that all the variables (except working day variable) in bike_share dataset seem to cumulatively influence in accurately predicting the y-response.  

**Question**: Discuss/interpret the meaning of the coefficients/decision tree. Do the models agree in terms of important explanatories? Do they agree in terms of the relationships of the variables that you examined in Task 1? Summarise what the models suggest about which factors influence the
high/low behaviour with bike sharing.

Ans:

A decision tree uses attributes to split the data in such a way where recursive splitting ultimately leads to groups of variables with similar kind of response. Upon each recursive splitting of subsets, the varience is reduced in the newely created set and should ultimately produce a homogenous set of groups. To determine the effectiveness of the algorithm/the split entropy is measured, where every recursive splitting leads to a decrease in randomness of the variables, and increase in ordering of groups with similar properties. A gain in information indicates that the entropy has reduced. An attribute and its corresponding subset which provides maximum gain in information is selected each time the tree undergoes recursive split. 
A decision tree is ideally useful when we observe one/more explanatories splitting the y-response into a homogenous population. Which means that the y-response can be predicted when the explanatories exsist within a certain range of value/s.

On the other hand, linear models are best used when the explanatories show a continuous y-response. This is quite the opposite to a decision tree, where discrete response is ideal in splitting the treee, hence reliably predicting the y-response.

Bike_share model:

In the decision tree (DT) model, the root node which splits the data is the temperature variable. However, in the linear model temp or atemp are not significant (the AIC value being very close to <none> and the p-value being more than 0.05). Since the decision tree can split the data vector values of a variable at any point (so as to maximum the information gain), the influence of the entire vector does not matter. However, in linear model the entire vector value is taken into consideration when building the model. In question 1, temparature does appear to influence y-response, albeit with a huge margin of error. 

Subsequent to temperature, 'year' happens to the next attribute used to further split the data in DT. The heavy influence of year variable is also reflected in linear model (AIC being 10479.6 - which means that if year is removed then the AIC will drastically increase from the current AIC=9666.71 to 10479.6). Additionally the linear model summary shows that year 2012 has a p-value < 2e-16. In question 1, indeed year variable showed very clear difference in y-response, where bike_sharing was much higher in the year 2012 than compared to the year 2011. 

The remainig nodes use climatic conditions to further split the data in DT. These include weathersit, season, month, humidity and atemp. These variables very much appear to be significant in the linear model also. 

On the contrary windspeed which seems to have high AIC (AIC= 9704.5)  and significantly low p-value (6.38e-10) dose not appear to be an important variable in splitting data in DT model. 

The above mentioned variables can be considered to heavily influence the overall accuracy of the model. The remaining variables (such as holiday and workingday) can be considered as low behaviour as they seem to minimally impact both the models.


Looking at the graph produced in task 3 titled "Comparison of all explanatories in factorised bike_share dataset with clean_bikes yresponse" we can see that temperature and to some extent windspeed seem to have linear relationship with y-response. Similarly, the categorical variables such as season and month (which also represent climatic condition) also seems to evidently influence y-response. Thus, in bike_share dataset, climatic condition is represented by both the continuous and categorical data. Hence, this might be one of the main reasons as to why we see both decision tree and linear model (almost) equaly predicting y-response. 

In addition to the bike_share dataset, I have also used two other datasets (namely #1 mtCars and #2 Olives) to provide further explanations regarding the two types of models, and what factors could potentially influence the accuracy of each of the models.

Example 1: MtCars dataset
```{r}
#inspect each of the explanatories with mpg column
a<-ggplot(mtcars) +  
  geom_point(aes(x = cyl, y = mpg))
b<-ggplot(mtcars) +  
  geom_point(aes(x = disp, y = mpg))
c<-ggplot(mtcars) +  
  geom_point(aes(x = hp, y = mpg))
d<-ggplot(mtcars) +  
  geom_point(aes(x = drat, y = mpg))
e<-ggplot(mtcars) +  
  geom_point(aes(x = wt, y = mpg))
f<-ggplot(mtcars) +  
  geom_point(aes(x = qsec, y = mpg))
g<-ggplot(mtcars) +  
  geom_point(aes(x = vs, y = mpg))
h<-ggplot(mtcars) +  
  geom_point(aes(x = am, y = mpg))
i<-ggplot(mtcars) +  
  geom_point(aes(x = gear, y = mpg))
j<-ggplot(mtcars) +  
  geom_point(aes(x = carb, y = mpg))

grid.arrange(a,b,c,d,e,f,g,h,i,j, ncol=5, top= "Comparison of all explanatories in mtCars dataset with mpg yresponse")

###################################################################################################################################################################
#Comparison of linear model with decision tree model, which one fares better?
# convert some of the columns to factors
mtcars$vs <- as.factor(mtcars$vs)
mtcars$am <- as.factor(mtcars$am)
mtcars$gear <- as.factor(mtcars$gear)
mtcars$carb <- as.factor(mtcars$carb)
mtcars$cyl <- as.factor(mtcars$cyl)
rSquareLMmtCars<-c()

# linear model and check r2 (do the 70:30 split)
rSquareLMmtCars<-replicate(n=50,assess.linearModel(dataFrame= mtcars, 
                                                   LMformula = mpg ~ .,1) )#non-factorised
# decision tree and check r2 (do the 70:30 split)
Data_sampling = 50
maxdepth_intervals = 20
rSquare_decisiontreemtCars <- matrix(ncol=maxdepth_intervals, nrow=Data_sampling)

for(i in 1:maxdepth_intervals){
  rSquare_decisiontreemtCars[,i]<-replicate(n=50, assess.decisionTreeModel(dataFrame= mtcars, 
                                                                           LMformula = mpg ~ ., 
                                                                           yResponse.col=1, MaxDepth = i))
}
boxplot(rSquare_decisiontreemtCars)


# the boxplot reveals that maxdepth of 5 is the best to compare the r2 value with the linear r2 value
rSquare_decisiontreemtCars<-as.data.frame(rSquare_decisiontreemtCars)#covert it to dataframe
boxPlotLinearModel_mtCars<-ggplot() +
  geom_boxplot(aes (x="Linear model", y=rSquareLMmtCars))+
  ylab("")

boxPlotDecisionTreeModel_mtCarss<-ggplot() +
  geom_boxplot(aes (x="Decision Tree Model-Max depth 5", y=rSquare_decisiontreemtCars$V5))+
  ylab("")
grid.arrange(boxPlotLinearModel_mtCars,boxPlotDecisionTreeModel_mtCarss, ncol=2, top = "MtCars dataset LMr2 and DTr2 comparison")
###########################################################################################################################################

#plot the entire olive dataset at maxdepth=5
mtCars.DTmodel <- rpart(mpg ~.,
                      mtcars,

                control=rpart.control(maxdepth=5,
                      minsplit=2,
                      minbucket=2,
                      cp=0))
prp(mtCars.DTmodel)

###############################################################################################################################################
```


Example 1 mtcars dataset

Looking at the "Comparison of all explanatories in mtCars dataset with mpg yresponse" ggplot, we can see some explanatories have continuous (e.g., disp,hp, drat, and wt) and discrete repsonse (e.g., cyl, vs, am, grear, and carb). Amongst these, the continuous variables seem to show a better picture at predicting y-response more so than the discrete variables. Thus, one might argue that this might be the reasone why linear modelling seems to fare better than the decision tree ("MtCars dataset LMr2 and DTr2 comparison"). The r2 from the linear model also shows less flactuation between the 50 replicate training/test splits, than that of the decision tree. 


Example 2: Olive dataset
```{r}
data(olives, package="classifly")

aO<-ggplot(olives) +  # Set up canvas with outcome variable on y-axis
  geom_point(aes(x = palmitic, y = Region))

bO<-ggplot(olives) +  # Set up canvas with outcome variable on y-axis
  geom_point(aes(x = stearic, y = Region))

cO<-ggplot(olives) +  # Set up canvas with outcome variable on y-axis
  geom_point(aes(x = palmitoleic, y = Region))
dO<-ggplot(olives) +  # Set up canvas with outcome variable on y-axis
  geom_point(aes(x = oleic, y = Region))
eO<-ggplot(olives) +  # Set up canvas with outcome variable on y-axis
  geom_point(aes(x = linoleic, y = Region))
fO<-ggplot(olives) +  # Set up canvas with outcome variable on y-axis
  geom_point(aes(x = linolenic, y = Region))
gO<-ggplot(olives) +  # Set up canvas with outcome variable on y-axis
  geom_point(aes(x = arachidic, y = Region))
hO<-ggplot(olives) +  # Set up canvas with outcome variable on y-axis
  geom_point(aes(x = eicosenoic, y = Region))

grid.arrange(aO,bO,cO,dO,eO,fO,gO,hO, ncol=4, top="Comparison of all explanatories in olives dataset with Region yresponse")

##############################################################################################################
#Comparison of linear model with decision tree model, which one fares better?
rSquareLMOlives<-c()
rSquare_decisiontreeOlivesVector<-c()
olives[,3:13] <- lapply(olives[2:10], as.numeric)#convert to numeric 
# linear model and check r2 (do the 70:30 split)
rSquareLMOlives<-replicate(n=50,assess.linearModel(dataFrame= olives, 
    LMformula = Region ~ palmitic + palmitoleic + stearic + oleic + linoleic + linolenic + arachidic + eicosenoic,1) )#non-factorised
# decision tree and check r2 (do the 70:30 split)
Data_sampling = 50
maxdepth_intervals = 20

rSquare_decisiontreeOlives <- matrix(ncol=maxdepth_intervals, nrow=Data_sampling)

for(i in 1:maxdepth_intervals){
  rSquare_decisiontreeOlives[,i]<-replicate(n=50, assess.decisionTreeModel(dataFrame= olives, 
                LMformula = Region ~ palmitic + palmitoleic + stearic + oleic + linoleic + linolenic + arachidic + eicosenoic, 
                yResponse.col=1, MaxDepth = i))
}
boxplot(rSquare_decisiontreeOlives)
# the boxplot reveals that maxdepth of 5 is the best to compare the r2 value with the linear r2 value
rSquare_decisiontreeOlivesVector<-replicate(n=50, assess.decisionTreeModel(dataFrame= olives, 
                                                                         LMformula = Region ~ palmitic + palmitoleic + stearic + oleic + linoleic + linolenic + arachidic + eicosenoic, 
                                                                         yResponse.col=1, MaxDepth = 5))

rSquare_decisiontreeOlives<- as.data.frame(rSquare_decisiontreeOlives)
# compare the r2 values between the two models
boxPlotLinearModel_olives<-ggplot() +
  geom_boxplot(aes (x="Linear model", y=rSquareLMOlives))+
  ylab("")

boxPlotDecisionTreeModel_olives<-ggplot() +
  geom_boxplot(aes (x="Decision Tree Model-Max depth 5", y=rSquare_decisiontreeOlives$V5))+
  ylab("")
grid.arrange(boxPlotLinearModel_olives,boxPlotDecisionTreeModel_olives, ncol=2, top = "Olives dataset LMr2 and DTr2 comparison")

###########################################################################################################################################

#plot the entire olive dataset at maxdepth=5
olives.modelDT <- rpart(Region ~ palmitic + palmitoleic + stearic + oleic + linoleic + linolenic + arachidic + eicosenoic,
                      olives,

                control=rpart.control(maxdepth=5,
                      minsplit=2,
                      minbucket=2,
                      cp=0))
prp(olives.modelDT)
```

Example 2 Olive dataset

Looking at the "Comparison of all explanatories in Olives dataset with Region yresponse" ggplot, we can see that the discrete variables can easily predict the y-response. In other words, the qualitative and quantitative values of fatty acids in olive oil can predict which geographic region the olive oil is derived from. Hence when comparing of r2 values between linear model and decision tree, in this case decsion tree provides a reliable model to predict the yresponse.  

#### Task 6 -Anomalous events from Table 4 Fanaee-T and Gama, 2014
**Question**: Select 2 events from Table 4 (Fanaee-T and Gama, 2014) and examine the bike sharing data.
Explain why these events (dates) have been labelled as anomalous and briefly discuss what
are the properties of an anomalous event.

Ans: The authors wanted to understand how time series dates can be associated with anomalous events (events which are outside the regular occurances). When analysing a time series data it is important to understand which dates are associated with anamalous events, as the variables associated with these dates can potentially be the casue of outliers in the data. 
The events from Table 4 have been termed as anamalous events owing to the detection of an important event related to the date. Google web, Google images and YouTube are used to identify events associated with candidate list of dates that the authors have zeroed in on. 
Hence the dates listed in Table 4 have many anamalous events associated with them, such as adverse weather conditions or some special events occuring on that day. A prior knowledge of such anamalous events in a time series data can help us in producing more reliable models and machine learning algorithms that can account for outliers. 

```{r}
rsession()
```


