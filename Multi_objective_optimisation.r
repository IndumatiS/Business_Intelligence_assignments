---
title: "Assingment 3"
author: "Indu Sharma"
date due: "28th/05/2020"
output: html_document
---

```{r setup, include=FALSE}
setwd("~/Desktop/INFO424/Assignment 3/INFO424Assn3(2)")
.libPaths("Desktop/INFO424")
library(ggplot2)
library(corrplot)
library(grid)
library(gridExtra)
library(ggcorrplot)
knitr::opts_chunk$set(echo = TRUE)
```
### ELECTRICITY MODELLING (40 MARKS)

#### Task 1

**Question 1** Examine the boxplot and briefly discuss the aggregated pattern of electricity usage.


```{r, width=2,fig.height=4,echo=FALSE}
simple.fn <- function(x,fn,cols=1:length(x))
{
    fn(x[cols])
}
power.csv <- read.csv('power.csv',header=FALSE)
# Since first column is the place, let's change it to the row name and remove it

rownames(power.csv) <- power.csv[,1] # Set rownames to the household ids
power.csv <- power.csv[,2:ncol(power.csv)] # Remove the first column since this the holdhold ids
colnames(power.csv) <- seq(from=1,to=48,by=1)

######################################################################################################################################
par(mfrow=c(1,2))
par(mar=c(5,5,1,1))
boxplot(power.csv,xlab="Time",ylab="Electricity Usage", main="All Households")
#
# Plot the first household data
plot(as.numeric(power.csv[5,]),type="l",xlab="Time",ylab="Electricity Usage",main="One Household")
#
par(mfrow=c(1,1)) # turn display back to single figure
```


Ans:
*-There are two time slots in 24h where the electricity consumption is higher than the rest of the times. There are morning peak hourse between 7-9am and 5-9pm.*
*-There is huge variation of electricity consumption between users, as evident from the large number of outliers.*
*-The variabtion of electricity consumption is much higher during peak hours as compared to off peak hours.*
*- If we segregate households based on daily electricity consumption, we might see more consistensies in electicity consumption. This may help in:*
 *- setting suitable pricing for based on overall consumer usage and*
 *- understanding the demand in electicity and thus predicting the how much of power generation is needed to meet the demand.*


```{r,echo=FALSE}
# Rename columns to be the sample "time"
# build.table
#--------------------------------------------------------------------------
# Build the table of explanatory variables using the 
# original electricity data (p)
# IN: p - the original electricity dataset
# OUT: The constructed table of data to use with kmeans
# NOTE: Each row of p is a household
#       The columns of p are the 48 readings in time for each household
#############################################################################################
#rename the columns so that they match the 24h 
names(power.csv)[1] <- paste("00",":30")
names(power.csv)[48] <- paste("00",":00")
for (i in 2:(length(power.csv)-1)){
  if(i %% 2==0) {
    x<-i/2
    names(power.csv)[i] <- paste(x,":00")
  }
  else {
      names(power.csv)[i] <- paste(x,":30")
    }
  }
#peak hours : 7am to 9am and 5pm to 9pm
```


#### Task2

**Question 2** Construct a table of explanatory variables, where each row is a household, and each column
is an explanatory variable that you have constructed from the original usage data. Describe the functions that you have constructed to create these explanatory variables, and in particular discuss how you have described (represented) the temporal structure of usage. Think about different household electricity usage and how you can capture aspects of this behaviour with different features.
```{r,echo=TRUE}
build.table <- function(p)
{
	tab <- apply(p[,1:48],1,simple.fn,mean)  # mean over all time periods
	tab <- cbind(tab,apply(p[,1:48],1,simple.fn,var))
	tab <- cbind(tab,apply(p[,1:48],1,simple.fn,sum))	
	tab <- cbind(tab,apply(p[,c("7 :00", "7 :30", "8 :00", "8 :30", "9 :00")],1,simple.fn,var)) 
	tab <- cbind(tab,apply(p[,c("17 :00", "17 :30", "18 :00", "18 :30", "19 :00", "19 :30", "20 :00", "21 :00")],1,simple.fn,var))
	tab <- cbind(tab,apply(p[,c(43:48, 1:13)],1,simple.fn,var)) 
	tab <- cbind(tab,apply(p[,c(19:33)],1,simple.fn,var)) 
	tab <- cbind(tab,apply(p[,c("7 :00", "7 :30", "8 :00", "8 :30", "9 :00")],1,simple.fn,sum)) 
	tab <- cbind(tab,apply(p[,c("17 :00", "17 :30", "18 :00", "18 :30", "19 :00", "19 :30", "20 :00", "21 :00")],1,simple.fn,sum))
	tab <- cbind(tab,apply(p[,c(43:48, 1:13)],1,simple.fn,sum)) 
	tab <- cbind(tab,apply(p[,c(19:33)],1,simple.fn,sum)) 
	colnames(tab) <- c("Average consumption",
	                   "Variation in consumption",
	                   "Total consumption",
	                   "Variation in consumption during morning peak hour", 
	                   "Variation consumption during evening peak hour",
	                   "Variation consumption during first off-peak hour",
	                   "Variation consumption during second off-peak hour",
	                   "Total consumption during morning peak hour", 
	                   "Total consumption during evening peak hour",
	                   "Total consumption during first off-peak hour",
	                   "Total consumption during second off-peak hour")  # labels for each column
	
	as.data.frame(tab)   # Return the final table
}

power.csv.tab<-build.table(power.csv)
```

```{r}
head(power.csv.tab)
```



*Ans:*

*To construct the table I have used information related to:*

*1) overall electricity consumption in 24h - this is an important parameter to consider when understanding cumulative energy consumption amongst customers. This helps electricity distributers to collect, and subsequently pass the data to generation companies, as to how much of energy is consumed within a set period.*

*2) peak hours of electicity consumption - Once again this parameter is also important in determing energy demands during different times of the day. During peak-hours electricity distributers would need to provide, for a sustained period, a significantly higher than average power supply. Knowing which times of the day experiences larger consumption of energy and by how much is a useful information for power generation. Having such information also provides additional insights for distribution companies to incentivise those customers who tend to have relatively consistent usage of energy throughout the day. Reducing peak electricity demand, both for consumers and the wider economy, helps easing pressure on lines networks and generation, thus resulting in millions of dollars of cost reduction for supplying electricity to consumers.*

*3) off-peak hours of electicity consumption - knowning the during and intensity of power consumption during off-peak hours, can help distribution companies to come up with special deals to encourage customers to re-schedule their power usage so as to reduce peak-hour consumption.*

*I have not used any specific functions to create these explanatories, rather extended the pre-exsiting function to incorporate the extra explanatories needed to better understand electricity consumption.*


#### Task 3

**Question 3** Apply kmeans clustering with 6 centers to your final explanatory datatable. Produce two
figures: one with boxplots (just like the original script example) showing the final clustering
patterns of usage for each of the six clusters, the second as 6 line plots showing the mean
usage pattern for each timestep for each cluster. See Figures 1 and 2 below for an example
(NOTE THIS USES 9 CLUSTERS, so you will have different results).

```{r,echo=FALSE}
####################################################################
# do.cluster
#--------------------------------------------------------------------
# IN: power.table  - the table of explanatory variables from build.table
#     num.clusters - number of clusters for kmeans
# OP: Performs kmeans clustering for num.clusters
# OUT: The result of the clustering algorithm
# NOTE: Assumes that the power.table has rows for each household, and
#       columns for the explanatory variables
#########################################################################
do.cluster <- function(power.table,num.clusters=6)
{	
	kmeans(scale(power.table),centers=num.clusters)
}
#call function
power.csv.cluster<-do.cluster(power.csv.tab)
power.csv.cluster.df<-as.data.frame(power.csv.cluster$cluster)
hist(power.csv.cluster$cluster, xlab="Clusters", main = "Number of households which fall into each of the clusters")
#segregate the clusters
cluster1<-as.data.frame(power.csv.cluster$cluster[which(power.csv.cluster$cluster==1)])
cluster1.explanatory<-power.csv[row.names(cluster1),]

cluster2<-as.data.frame(power.csv.cluster$cluster[which(power.csv.cluster$cluster==2)])
cluster2.explanatory<-power.csv[row.names(cluster2),]

cluster3<-as.data.frame(power.csv.cluster$cluster[which(power.csv.cluster$cluster==3)])
cluster3.explanatory<-power.csv[row.names(cluster3),]

cluster4<-as.data.frame(power.csv.cluster$cluster[which(power.csv.cluster$cluster==4)])
cluster4.explanatory<-power.csv[row.names(cluster4),]

cluster5<-as.data.frame(power.csv.cluster$cluster[which(power.csv.cluster$cluster==5)])
cluster5.explanatory<-power.csv[row.names(cluster5),]

cluster6<-as.data.frame(power.csv.cluster$cluster[which(power.csv.cluster$cluster==6)])
cluster6.explanatory<-power.csv[row.names(cluster6),]

#produce a boxplot of all the clusters to visualise how each of the clusters behave differently

par(mfrow=c(3,2))
boxplot(cluster1.explanatory, main= "cluster1", xlab="Time -  30 mins intervals from 12:30 am", ylab="electricity consumption (kwh)", ylim = c(0,3))
boxplot(cluster2.explanatory, main= "cluster2", xlab="Time -  30 mins intervals from 12:30 am", ylab="electricity consumption (kwh)", ylim = c(0,3))
boxplot(cluster3.explanatory, main= "cluster3", xlab="Time -  30 mins intervals from 12:30 am", ylab="electricity consumption (kwh)", ylim = c(0,3))
boxplot(cluster4.explanatory, main= "cluster4", xlab="Time -  30 mins intervals from 12:30 am", ylab="electricity consumption (kwh)", ylim = c(0,3))
boxplot(cluster5.explanatory, main= "cluster5", xlab="Time -  30 mins intervals from 12:30 am", ylab="electricity consumption (kwh)", ylim = c(0,3))
boxplot(cluster6.explanatory, main= "cluster6", xlab="Time -  30 mins intervals from 12:30 am", ylab="electricity consumption (kwh)", ylim = c(0,3))
#The first entry is at 12:30am, 
#the second at 1am, and so on. 
#So, for example, the 14th sample is at 7am, 
#and the 24th sample is at midday.

#produce a lineplot of all the clusters to visualise how each of the clusters behave differently

par(mfrow=c(3,2))
plot(apply(cluster1.explanatory,2,mean), type = "l", main = "cluster1", xlab="Time -  30 mins intervals from 12:30 am", ylab="electricity consumption (kwh)", ylim = c(0,2)) 
plot(apply(cluster2.explanatory,2,mean), type = "l", main = "cluster2", xlab="Time -  30 mins intervals from 12:30 am", ylab="electricity consumption (kwh)", ylim = c(0,2))
plot(apply(cluster3.explanatory,2,mean), type = "l", main = "cluster3", xlab="Time -  30 mins intervals from 12:30 am", ylab="electricity consumption (kwh)", ylim = c(0,2))
plot(apply(cluster4.explanatory,2,mean), type = "l", main = "cluster4", xlab="Time -  30 mins intervals from 12:30 am", ylab="electricity consumption (kwh)", ylim = c(0,2))
plot(apply(cluster5.explanatory,2,mean), type = "l", main = "cluster5", xlab="Time -  30 mins intervals from 12:30 am", ylab="electricity consumption (kwh)", ylim = c(0,2))
plot(apply(cluster6.explanatory,2,mean), type = "l", main = "cluster6", xlab="Time -  30 mins intervals from 12:30 am", ylab="electricity consumption (kwh)", ylim = c(0,2))
```

**Question 3** Comment on how the patterns of usage vary across the clusters, and whether this aligns
with your design regarding different household behaviour (i.e. the types of features that you
have used to distinguish different behaviour).

Ans: *Cluster 1: has about 60 households as per the histogram; Their electricity consumption is relatively consistent throughout the day, with only slightly peaking during the morning peak hours.*

*Cluster 2 has about 80 households as per the histogram - Cluster 3 - Cluster 4- Cluster 5- CLuster 6*

#### Task 4

**Question 4:**Select a pair of explanatory variables (say x1 and x2) that are not highly correlated and plot
x1 versus x2 for each household colouring each point by cluster number. Comment on how
the clustering is related to these variables.

```{r,width=10,fig.height=6,echo=FALSE}
#do correlation between explanatories
library(tibble)
library(corrplot)
library(gclus)
library(grid)
library(gridExtra)

power.tab.cor <- abs(cor(power.csv.tab))  # Take absolute value of correlation
power.tab.colors <- dmat.color(power.tab.cor)
power.tab.order <- order.single(cor(power.csv.tab))  # Order by correlation


power.correlation<-ggcorrplot(power.tab.cor,
           type = "lower",
           outline.color = "white",
           lab= "TRUE") +
          labs(title = "Correlation matrix for power consumption across 250 households") +
  theme(title = element_text(size = 20)) +
  theme(axis.title.x = element_text(size = 10)) 
power.correlation
#pick the one that is not highly correlated
#1) 0.31- Variation in consumption during morning peak hour and average consumption during everning peak hour
#2) 0.20- Variation consumption during evening peak hour and average consumption during second off-peak hour
```

```{r,echo=FALSE}
#colouring each point by cluster number
power.csv.cluster.df<-as.data.frame(power.csv.cluster$cluster)
power.csv.cluster.df <- cbind(power.csv.cluster.df,power.csv.tab$`Variation consumption during second off-peak hour`)
power.csv.cluster.df <- cbind(power.csv.cluster.df,power.csv.tab$`Variation consumption during first off-peak hour`)
colnames(power.csv.cluster.df)<- c("cluster", "Variation consumption during second off-peak hour", "Variation consumption during first off-peak hour")
power.csv.cluster.df$cluster<-as.factor(power.csv.cluster.df$cluster)
#ggplot
weak.correlation<- ggplot(data=power.csv.cluster.df, aes(x = `Variation consumption during second off-peak hour`, y =`Variation consumption during first off-peak hour`)) + geom_point(aes(color=cluster))
weak.correlation
```


### Investment portfolio management (60 marks)

**Question 1:**Visualise and discuss the different ROI and Risk associated with Stocks, Bonds and Cash. Secondly, load, visualise and give a simple interpretation for the correlation table (“corr.tab”).

```{r, width=3,fig.height=3,echo=FALSE}
invest <- read.table("invest.tab")
invest$Type<-as.factor(invest$Type)
invest$Type_of_investment<-c ("Stock", "Stock", "Stock", "Stock", "Stock", "Stock", "Stock", "Stock","Stock", "Stock", "Bond", "Bond", "Bond",  "Bond", "Bond", "Bond",  "Bond", "Bond", "Bond", "Bond", "Cash", "Cash", "Cash", "Cash", "Cash", "Cash", "Cash", "Cash", "Cash", "Cash")

#Visualise different ROI and risk associated with Stocks, Bonds and Cash
diffROI.risk<- ggplot(data=invest, aes(x = ROI, y = Risk)) + geom_point(aes(color=Type_of_investment))
                                             
diffROI.risk

correlation_tab <- read.table("corr.tab")
ggcorrplot(correlation_tab,
           type = "lower",
           outline.color = "white",
           lab= "TRUE",
           lab_size = 2.5,
           title = "Correlation matrix for investment portfolio")
```

ROI and Risk: 
- The more the ROI the higher the risk. These two factors seems to be highly correlated.
- Stocks have low ROI hence low risk
- Bonds have slightly more ROI than the stocks, hence have more risks.
- But cash seem to have the highest ROI amongst all the portfolio hence also have the highest risk on returns. 


**Question 2**:Run the “invest.R” script. This script does a multi-objective criteria analysis to determine the best mix of stocks, bonds and cash over a range of tradeoffs. Describe in words what the “invest.R” script is doing. In particular, state how a solution on the pareto front is represented and the relationship between the solution space, the objective space and the constraints. Include in your description a histogram showing the number and type of investments for each solution on the pareto front.
Ans: *Multi-objective constrained model requires a definition of solution space, objective spage and contraints to run the NSGA function. *

*Solution space:*
*Objective space:*
*1) Return on Investion (ROI) is to be maximised. Only include amounts greater than minAmount*
*2) Risk is to be minimised. Only include amounts greater than minAmount.*
*Constraints:*
*1) The sum of the portfolios must be between 0.95 and 1.0*
*2) Each selected option must be  >= minAMOUNT and <= maxAMOUNT*
*3) Constraints on the minimum and maximum number of selected stocks/bonds/cash*
*minNumber = 8*
*maxNumber = 12*
*Multi-objective contrained model produces not the best solution, rather a vector of optimal solutions. These constraints help us to narrow down suitable solutions in pareto front which are likely to be attrictive (more realistic) to potential investors.*
*Solutions on the pareto front are the non-dominated solutions, i.e.,equal weightage is given to both the objectives under consideration.*
*The pareto front which is produced from "invest.R" produces a convex curve. This type of pareto front happens to be better than the concave one, as the tradeoff between meeting the objectives is more favourable. In such cases, 10% compromise in one of the objectives leads to more than 10% gain in meeting the other objective.*  

```{r, echo=FALSE}
###########################################
# ASSN 3
# invest.R
###########################################
# Build a multi-objective constrained model
# for an investment portfolio
###########################################
#install.packages("mco")
library(mco)

# The 30 investments that will be used to select
# a portfolio are in "invest.tab"
#
invest <- read.table("invest.tab")
#
# Determine number of options from the invest table
#
numberOptions <- nrow(invest)
#
# The format of invest is:
# Each row is an option
# Columns are : ROI (return on investment), Risk, Type
# 			    where Type = 1 (Stock), 2 (bond), 3 (cash)
#
##########################################
# CONSTRAINTS
##########################################
# The sum of the portfolios must be between
# 0.95 and 1.0
# Constraints are satisfied by being >= 0
#############################################
portfolioSUM <- function(x)
{
	selected <- which(x >= minAMOUNT)
	sumx = sum(x[selected])
	if ((sumx >=0.95) && (sumx <=1.0)) return(1) # ok
	return(-1) # Fails constraint
}
#############################################################
# Each option must either not be selected
# OR must be between an amount
#   minAMOUNT <= amount <= maxAMOUNT
#
# We will assume that anything < minAMOUNT
# is equivalent to zero (and therefore not included)
#############################################################
minAMOUNT = 0.05
maxAMOUNT = 0.2
#############################################################
# Set the lower and upper bounds for
# each investment option
#############################################################

# The lower bound is 0.
lower = rep(0,numberOptions)

# The upper bound is the maximum amount of an
# option, which is maxAMOUNT
upper = rep(maxAMOUNT,numberOptions)
#
###########################################################################
# Constraint - each selected option must be  >= minAMOUNT
#              and <= maxAMOUNT
# We ignore options with value < minAMOUNT because they
# aren't part of the final selection of investments.
###########################################################################
portfolioRANGE <- function(x)
{
	selected <- which(x >= minAMOUNT)  # Only count if at least minimum
	over <- which(x[selected] > maxAMOUNT)
	if (length(over) > 0) return(-1)
	return(1)
}

##################################################################
# Constraints on the minimum and maximum number of selected
# stocks/bonds/cash
##################################################################
minNumber = 8
maxNumber = 12

portfolioNUM <- function(x)
{
	numselected <- length(which(x >= minAMOUNT))
	if (numselected < minNumber) return(-1)
	if (numselected > maxNumber) return(-1)
	return(numselected)
}

############################################################
# Functions to be minimised/maximised
############################################################
###############################
# Return on Investion (ROI)
###############################
# This wants to be MAXIMISED
# Only include options that are greater than the minAMOUNT
#############################################################
ROI <- function(x)
{
	selected <- which(x >= minAMOUNT)
	roi <- sum(invest$ROI[selected]*x[selected])
	return(-roi) # Since nsgaII minimises we take the negative
}
#################################
# Risk (RISK)
#################################
# This is to be MINIMISED
# Only include options that are greater than the minAMOUNT
##############################################################
RISK <- function(x)
{
	selected <- which(x >= minAMOUNT)
	risk <- sum(invest$Risk[selected]*x[selected])  # Just the sum of risk
	return(risk) # we want to minimise the risk
}
##################################################
# Here are the functions that are to be minimised
# Note ROI is actually maximised, while RISK is
# minimised.
###################################################
funs <- function(x)
{
	return(c(ROI(x),RISK(x)))
}
######################################################
# Here are the constraints
# Since nsga2 assumes a single constraint function, we
# call each constraint in turn, and return the results
# of all the constraints as a concatenated list
######################################################
constraintFNS <- function(x)
{
	psum = portfolioSUM(x)
	prange = portfolioRANGE(x)
	pnum = portfolioNUM(x)
	return(c(prange,pnum,psum))
}
# Set the lower and upper bounds for each investment option
# The lower bound is 0.
lower = rep(0,numberOptions)
# The upper bound is the maximum amount of an option, which is maxAMOUNT
upper = rep(maxAMOUNT,numberOptions)
#
###########################################################
# CALL nsga2 to find the pareto optimal solutions
###########################################################
portfolio <- nsga2(funs,
                   idim=numberOptions, #inputs for each option,
                   odim=2, #2 outputs (ROI,RISK)
                   popsize=52,generations=500,
				           lower.bounds=lower,
				           upper.bounds=upper,
					         constraints = constraintFNS,
					         cdim=3) # 3 constraints

######## Plot the pareto front using default plotting
###########################################################
plot(portfolio,xlab="-ROI (%)",ylab="RISK",main="Objective Space")
```
```{r,echo=TRUE, error=FALSE, message=FALSE}
#Type = 1 (Stock), 2 (bond), 3 (cash)
#convert invest$type as.factor
#Matrix of investment blends across the pareto front [52,3]
# While doing this loop also add the total risk and ROI associated with each of the solutions
Pareto.front.investmentBlends<-as.data.frame(matrix(nrow = 52, ncol = 10))
for(i in 1:52) {
  x<-invest[which(portfolio$par[i,] > 0.05),] 
 Pareto.front.investmentBlends[i,1]<- length(which(x$Type == 1))
 Pareto.front.investmentBlends[i,2]<- length(which(x$Type == 2))
 Pareto.front.investmentBlends[i,3]<- length(which(x$Type == 3))
 sum1<-sum(as.numeric(Pareto.front.investmentBlends[i,1]), as.numeric(Pareto.front.investmentBlends[i,2]), as.numeric(Pareto.front.investmentBlends[i,3]) )
 Pareto.front.investmentBlends[i,4]<- (Pareto.front.investmentBlends[i,1] / sum1) *100 # percentage of Stock blend
 Pareto.front.investmentBlends[i,5]<- (Pareto.front.investmentBlends[i,2] / sum1) *100 # percentage of Bond blend
 Pareto.front.investmentBlends[i,6]<- (Pareto.front.investmentBlends[i,3] / sum1) *100 # percentage of Cash blend
 Pareto.front.investmentBlends[i,7]<- sum(x$ROI)
 Pareto.front.investmentBlends[i,8]<- sum(x$Risk)
if(Pareto.front.investmentBlends[i,8]<=0.3349) {Pareto.front.investmentBlends[i,9]<- "low risk"}
if((Pareto.front.investmentBlends[i,8]>= 0.3349) && (Pareto.front.investmentBlends[i,8]<= 0.6544)) {Pareto.front.investmentBlends[i,9]<- "medium risk"}
if(Pareto.front.investmentBlends[i,8]>=0.6544) {Pareto.front.investmentBlends[i,9]<- "high risk"}
 }

Pareto.front.investmentBlends<-as.data.frame(Pareto.front.investmentBlends)
colnames(Pareto.front.investmentBlends) <- c("Stock",
	                   "Bond",
	                   "Cash",
	                   "Percentage_of_Stock_Blend",
	                   "Percentage_of_Bond_Blend",
	                   "Percentage_of_Cash_Blend",
	                   "ROI", 
	                   "Risk",
	                   "Risk_level",
	                   "Correlation_Sum")

plot(Pareto.front.investmentBlends$ROI, Pareto.front.investmentBlends$Risk)
Pareto.front.investmentBlends$Risk_level<-as.factor(Pareto.front.investmentBlends$Risk_level)


p1 <- ggplot(Pareto.front.investmentBlends) +
        geom_histogram(aes(x=Stock),binwidth = 0.5)+
        xlab("Stock")+ xlim (0,8) + ylim(0,20)
        
p2 <- ggplot(Pareto.front.investmentBlends) +
        geom_histogram(aes(x=Bond),binwidth = 0.5)+
        xlab("Bond")+ xlim (0,8) + ylim(0,20)

p3 <- ggplot(Pareto.front.investmentBlends) +
        geom_histogram(aes(x=Cash), binwidth = 0.5)+
        xlab("Cash") + xlim (0,8) + ylim(0,20)
grid.arrange (p1,p2,p3, ncol = 3, nrow = 1, top="Type of investments for each solutions on the pareto front")
```
*In the pareto front there are 52 solutions, each of them having a different blend of investment portfolios. The histogram above shows the frequency at which each of the investment types occur in each of the 52 solutions. The trend seen in the above three histogram very much reflects the plot in question 1 which plots ROI vs Risk for each of the investment types, i.e.:*
*1) cash investment occurs at a low frequency across the pareto front given that this type of investment yeilds low ROI*
*2) bond investment occurs at a high frequency across the pareto front given that this type of investment yeilds medium ROI with medium risk- this means that majority of the 52 solutions have a significant proportion of bond blend in its investment portfolio*
*3) stock investment occures at a medium frequence across the pareto front give that this type of investment yeild high ROI with high risk- this means having low frequency of stock blend is reasonably a safe bet in getting high ROI despite of its high risk.*

**Question 3**
Using the result of the nsga2 model you have previously run, examine and present the blend of stocks, bonds and cash for a low risk, moderate risk and high risk investment blend (just pick one from each general category). Discuss, in relation to Table 1, the level of risk that seems to be taken by the brokerage houses and whether the one year return performance is related to the associated risk of the brokerage house.
```{r, echo=FALSE}
lbls <- c("Stock", "Bond", "Cash")
slices1 <- c(Pareto.front.investmentBlends[1,4], Pareto.front.investmentBlends[1,5], Pareto.front.investmentBlends[1,6])
lbls1 <- paste(lbls, slices1) # add percents to labels
lbls1 <- paste(lbls1,"%",sep="") # ad % to labels
pie(slices1, labels = lbls1, main="Low Risk Investment")

slices2 <- c(Pareto.front.investmentBlends[2,4], Pareto.front.investmentBlends[2,5], Pareto.front.investmentBlends[2,6])
lbls2 <- paste(lbls, slices2) # add percents to labels
lbls2 <- paste(lbls2,"%",sep="")
pie(slices2, labels = lbls2, main="Medium Risk Investment")

slices3 <- c(Pareto.front.investmentBlends[3,4], Pareto.front.investmentBlends[3,5], Pareto.front.investmentBlends[3,6])
lbls3 <- paste(lbls, slices3) # add percents to labels
lbls3 <- paste(lbls3,"%",sep="")
pie(slices3, labels = lbls3, main="High Risk Investment")
```
Ans: *As per the pie charts above, if an investment portfolio has 60% of stocks, then this comes under high-risk category. In a medium risk investment stocks generally form 33% of the investment portfolio.* 

**Question 4 **
Examine the plot shown in Figure 3. This shows how the percentage of bonds, stocks and cash vary as you move along the pareto front from the least to greatest percentage return. Outline the approach (set of steps, algorithm, pseudo code, or R code...) that would be required to produce this figure given the output from nsga2.

```{r,echo=FALSE}
question4<-ggplot(Pareto.front.investmentBlends, aes(x= ROI)) +
    geom_line(aes(y=Percentage_of_Stock_Blend,colour="Stock_Blend")) +
    geom_line(aes(y=Percentage_of_Bond_Blend,colour= "Bond_Blend")) +
    geom_line(aes(y=Percentage_of_Cash_Blend, colour = "Cash_Blend"))
  
question4
```

```{r}
# For task 5- Adding correlation objective into portfolio genetic algorith
#################################
# Correlation is to be MINIMISED when considering the blend of investment portfolios
##############################################################
min_correlation <- function(x)
{
  correlation_sum<-0
  selected <- which(x >= minAMOUNT)
  sol.matrix<- as.data.frame(matrix(ncol= length(selected), nrow=length(selected)))
  for(i in 1:length(selected)){#create a matrix of correlation coefficients for the selected vector 
                      for(j in 1:length(selected)){
                        correlation_sum <- correlation_sum + (correlation_tab[(selected[i]),(selected[j])])
                          }
                        }
  
  return(correlation_sum) # we want to minimise the risk
}
###################################################
funs <- function(x)
{
	return(c(ROI(x),RISK(x),min_correlation(x)))
}
```

```{r, echo=FALSE}
###########################################
# ASSN 3
# invest.R
###########################################
# Build a multi-objective constrained model
# for an investment portfolio
###########################################
#install.packages("mco")
library(mco)

# The 30 investments that will be used to select
# a portfolio are in "invest.tab"
#
invest <- read.table("invest.tab")
#
# Determine number of options from the invest table
#
numberOptions <- nrow(invest)
#
# The format of invest is:
# Each row is an option
# Columns are : ROI (return on investment), Risk, Type
# 			    where Type = 1 (Stock), 2 (bond), 3 (cash)
#
##########################################
# CONSTRAINTS
##########################################
# The sum of the portfolios must be between
# 0.95 and 1.0
# Constraints are satisfied by being >= 0
#############################################
portfolioSUM <- function(x)
{
	selected <- which(x >= minAMOUNT)
	sumx = sum(x[selected])
	if ((sumx >=0.95) && (sumx <=1.0)) return(1) # ok
	return(-1) # Fails constraint
}
#############################################################
# Each option must either not be selected
# OR must be between an amount
#   minAMOUNT <= amount <= maxAMOUNT
#
# We will assume that anything < minAMOUNT
# is equivalent to zero (and therefore not included)
#############################################################
minAMOUNT = 0.05
maxAMOUNT = 0.2
#############################################################
# Set the lower and upper bounds for
# each investment option
#############################################################

# The lower bound is 0.
lower = rep(0,numberOptions)

# The upper bound is the maximum amount of an
# option, which is maxAMOUNT
upper = rep(maxAMOUNT,numberOptions)
#
###########################################################################
# Constraint - each selected option must be  >= minAMOUNT
#              and <= maxAMOUNT
# We ignore options with value < minAMOUNT because they
# aren't part of the final selection of investments.
###########################################################################
portfolioRANGE <- function(x)
{
	selected <- which(x >= minAMOUNT)  # Only count if at least minimum
	over <- which(x[selected] > maxAMOUNT)
	if (length(over) > 0) return(-1)
	return(1)
}

##################################################################
# Constraints on the minimum and maximum number of selected
# stocks/bonds/cash
##################################################################
minNumber = 8
maxNumber = 12

portfolioNUM <- function(x)
{
	numselected <- length(which(x >= minAMOUNT))
	if (numselected < minNumber) return(-1)
	if (numselected > maxNumber) return(-1)
	return(numselected)
}

############################################################
# Functions to be minimised/maximised
############################################################
###############################
# Return on Investion (ROI)
###############################
# This wants to be MAXIMISED
# Only include options that are greater than the minAMOUNT
#############################################################
ROI <- function(x)
{
	selected <- which(x >= minAMOUNT)
	roi <- sum(invest$ROI[selected]*x[selected])
	return(-roi) # Since nsgaII minimises we take the negative
}
#################################
# Risk (RISK)
#################################
# This is to be MINIMISED
# Only include options that are greater than the minAMOUNT
##############################################################
RISK <- function(x)
{
	selected <- which(x >= minAMOUNT)
	risk <- sum(invest$Risk[selected]*x[selected])  # Just the sum of risk
	return(risk) # we want to minimise the risk
}
##################################################

#################################
# Correlation
#################################
# This is to be MINIMISED
##############################################################
min_correlation <- function(x)
{
  correlation_sum<-0
  selected <- which(x >= minAMOUNT)
  sol.matrix<- as.data.frame(matrix(ncol= length(selected), nrow=length(selected)))
  for(i in 1:nrow(sol.matrix)){#create a matrix of correlation coefficients for the selected vector 
                      for(j in 1:ncol(sol.matrix)){
                        correlation_sum <- correlation_sum + (correlation_tab[(selected[i]),(selected[j])])
                          }
                        }
  
  return(correlation_sum) # we want to minimise the risk
}
###################################################
funs <- function(x)
{
	return(c(ROI(x),RISK(x),min_correlation(x)))
}
######################################################
# Here are the constraints
# Since nsga2 assumes a single constraint function, we
# call each constraint in turn, and return the results
# of all the constraints as a concatenated list
######################################################
constraintFNS <- function(x)
{
	psum = portfolioSUM(x)
	prange = portfolioRANGE(x)
	pnum = portfolioNUM(x)
	return(c(prange,pnum,psum))
}
# Set the lower and upper bounds for each investment option
# The lower bound is 0.
lower = rep(0,numberOptions)
# The upper bound is the maximum amount of an option, which is maxAMOUNT
upper = rep(maxAMOUNT,numberOptions)
#
###########################################################
# CALL nsga2 to find the pareto optimal solutions
###########################################################
portfolio <- nsga2(funs,
                   idim=numberOptions, #inputs for each option,
                   odim=3, #3 outputs (ROI,RISK,correlation_sum)
                   popsize=52,generations=500,
				           lower.bounds=lower,
				           upper.bounds=upper,
					         constraints = constraintFNS,
					         cdim=3) # 3 constraints
```

```{r, echo=FALSE}
###################A function to return the row number given a vector#############
Return.row<-function(df,x){
  return.vector<-c()
  for(i in 1:length(x)){
    return.vector[i]<-which(row.names(df)==x[i])
  }
  return(return.vector) 
}

#########################################################################################
solution1<-invest[which(portfolio$par[5,] > 0.05),] #minAMOUNT <= amount <= maxAMOUNT
sol1.matrix<- as.data.frame(matrix(ncol= 10, nrow=10))

for(i in 1:nrow(solution1)){
  for(j in 1:nrow(solution1)){
    corr.vector<-Return.row(df=invest,row.names(solution1))
    sol1.matrix[i,j]<- correlation_tab[corr.vector[i],corr.vector[j]]
    colnames(sol1.matrix)[j]<-c(row.names(solution1)[j])
  }
  rownames(sol1.matrix)[i]<-c(row.names(solution1)[i])
}

```

```{r,echo=FALSE}
###########Corrlation matrix for one of the solutions#######################
sol1.correlation<-ggcorrplot(sol1.matrix,
           type = "lower",
           outline.color = "white",
           lab= "TRUE") +
          labs(title = "Correlation matrix for one of the solutions on Pareto Front")

sol1.correlation # shows correlation matrix for one of the solutions on Pareto Front

question5.1<-ggplot(Pareto.front.investmentBlends, aes(x=portfolio$value[,3], y= portfolio$value[,1]))+
            geom_point() + labs(title = "Objective Space-ROI vs Correlation")+
  geom_point(aes(color=Risk_level)) +
  labs(y="ROI", x = "Correlation Sum")

question5.2<-ggplot(Pareto.front.investmentBlends, aes(x=portfolio$value[,3], y= portfolio$value[,2]))+
            geom_point() + labs(title = "Objective Space-Risk vs Correlation")

for(i in 1:52) {
  Pareto.front.investmentBlends[i,10]<-portfolio$value[i,3]
}

question5.3<-ggplot(Pareto.front.investmentBlends, aes(x=Correlation_Sum)) +
    geom_line(aes(y=Percentage_of_Stock_Blend, color="% Stock Blend")) +
    geom_line(aes(y=Percentage_of_Bond_Blend,colour= "% Bond Blend")) +
    geom_line(aes(y=Percentage_of_Cash_Blend, colour = "% Cash Blend"))+
    ylab("Percentage of different types of investment blend")+
    ggtitle("Percentage of stock blend vs Correlation")

low.risk.df<-Pareto.front.investmentBlends[which(Pareto.front.investmentBlends$Risk_level == "low risk"),]
medium.risk.df<-Pareto.front.investmentBlends[which(Pareto.front.investmentBlends$Risk_level == "medium risk"),]
high.risk.df<-Pareto.front.investmentBlends[which(Pareto.front.investmentBlends$Risk_level == "high risk"),]

plot1<-boxplot(low.risk.df$Correlation_Sum, medium.risk.df$Correlation_Sum, high.risk.df$Correlation_Sum, main = "Distribution of correlation sum across different risk levels", names = c("Low Risk", "Medium Risk", "High Risk"), ylab = "Correlation sum")

#Plot the graphs
question5.1
question5.3
plot1

```
**Question 5**
A matrix representing the estimated correlation between any two investments in terms of their return behaviour is given in the table corr.tab. For our example with 30 investments this is a 30x30 table. A good portfolio should aim for investments where correlation of behaviour is minimised, so that if some of the investments are decreasing, others in the portfolio are increasing. This reduces the risk of the portfolio losing money in a fluctuating market and (so theory goes) may result in more stable returns.

Extend the model in invest.R to have an additional objective which is to minimise the correlation for the investment blend.
a)Describe in words the approach you have chosen and explain the rational for the model. Include your R code for the function that calculates this objective.

Ans: *First, I tried looking at how correlation matrix is for one of the solutions in pareto front (check the correlation matrix embedded). This gave me an understanding as to how different correlation values are for each of the pairwise investment types. This meant that for each solutions on the pareto front will see different correlation matrix owing the differences in investment blends. One way to measure which of the blends provide us with the lowest correlation, is to simply sum the pairwise correlation for each of the solutions. This is then used as the third objective (i.e., to minimise the cumulative correlation for each of the blends).*


b)Produce a plot showing the objectives “correlation” versus “return” (x axis correlation, y axis return) for the pareto solutions, and

Ans: *See the graph titled "Objective Space-ROI vs Correlation"*

c)discuss what type of return is likely to be best for minimising correlation (and probably therefore the safest combination to consider).

Ans: *As per the graph titled "Percentage of stock blend vs Correlation", we see that when the percentage of investment type is distributed equally on the pareto front, then the overall pairwise correlation tends to be much less as compared to investement blends which have skewed proportion of a particular type of investment. Hence, we can conclude that an investment type which has a reasonably equal distribution of all the three types of investments tends to be the safest combination to consider.*
