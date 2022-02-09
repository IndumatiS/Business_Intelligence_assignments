---
title: "Assignment 1- INFO424"
author: "Indu Sharma (Student ID- 5910918)"
date: "02/03/2020"
output:
  html_document:
    df_print: paged
  pdf_document: default
  word_document: default
---

Part 1. Time Series Data: The SP500 index (15 Marks)

First load the required libraries

```{r}
library(xts)
library(DMwR)
library(quantmod)
library(ggplot2)
library(tibble) 
library(corrplot)
```

Read the csv file with sp500 data and convert to a time series object
```{r}
sp500 <- as.xts(read.zoo("sp500.csv",header=T))
```

Plot a candlechart for the last 3 months of the dataset

```{r }
chartSeries(last(sp500,"12 months"), theme='white')
```

1. Examine the upper part of the plot and describe what the candles are showing (HINT:
change the script to show just the last “1 months” and look at the data for the last “1
months”).

```{r}
#select the last month
chartSeries(last(sp500,"1 month"), theme='white')
```


2. The daily average price can be approximated by:
          Av (t) =( C(t) + H(t) + L(t))/3

where C(t), H(t) and L(t) are the close, high and low quotes for day t respectively.
Calculate the average price Av(t) for the sp500 data and add this to the sp500 table. Produce
a plot showing Av(t) and the R code used to calculate Av(t). (Hint: This can be done in a
single line – there is no need for a for loop)

```{r}
sp500$AverageDailyPrice<-(sp500$Close +sp500$High + sp500$Low)/3
#plot Av(t)
chartSeries((sp500$AverageDailyPrice), theme='white')

```

3. Daily returns are commonly used in stock market analysis. The daily return is defined as:

    return(t) = (C(t) - C(t-1))/ C(t-1)
    
** Make a copy of C(t) as a numeric vector and use this to determine return(t). Calculate the
return for the last 12 months of the sp500 data and compare the candleChart for the last 12 months
and the daily returns. Discuss the patterns in daily returns in relation to the sp500 price signal.
Include a figure showing the returns plot and R code to calculate returns.

````{r}
last_12months<- last(sp500, "12 months")# subset the last 12 months sp500 dataset
numericVector<-as.numeric(last_12months[,4])#Make a numeric vector
dailyReturns <- c() #make an empty vector
for (i in 2:length(numericVector)) {
  dailyReturns[i] <- (numericVector[i]- numericVector[i-1])/(numericVector[i-1])
} 
last_12months$dailReturns<-dailyReturns
chartSeries((last_12months$dailReturns), theme='white')
chartSeries((last_12months$AverageDailyPrice), theme='white')
# loop through every vector element to calculate the daily returns

```


Part 2. Degree Distribution: Networks (15 marks)

2.Produce a single figure showing 2 plots: the histogram of the degree, and a line plot sorted by degree. Comment on what this tells you about the network.

```{r}
library(ggplot2)

networkDF <- read.csv('network.csv', header = TRUE)
factor(networkDF$k)
library(plyr)

ggplot(networkDF, aes (x=k))+
  geom_histogram(breaks = seq (0, 50, by = 2))+
  xlim (c(0,50))+
  ylim(c(0,500))
  
```

3. Calculate P(k), which is the probability of observing a node with degree k, and produce a plot
of P(k) versus k (as shown below, Figure 1) using a log scale for both the x and y axes. In
your assignment include the R code for calculating P(k) and producing the plot.

```{r}
library(plyr)
nodeFrequency = count(networkDF, 'k')
nodeFrequency
nodeFrequency$frequency<- nodeFrequency$freq/sum(nodeFrequency$freq)
p <- ggplot(nodeFrequency, aes(x = k, y = frequency)) + 
  geom_point()+
  scale_x_continuous(trans = 'log2') +
  scale_y_continuous(trans = 'log2')+
  xlab("Degree(k)") + 
  ylab("P(k)")
p
```

4. Explain why the plot from step 4 (should be from step 3!) implies that the network is likely to be scale-free.

Ans: Scale-free networks can be scaled to accomodate more number of vertices yet maintaining the underlying structure. An important feature of a scale free network is that there are a few nodes with increased number of edges and the remaining majority of the nodes have very few edges. 
  The graph generated from question 3 shows that the number of nodes with high degree of edges occurs at very low probality, and that they are independent to the overall number of nodes present in this network.  

Part 3. DATA QUALITY AND STRUCTURE(20 MARKS)

1. Explain what the plot represents and why this might be a useful method for examining the
quality of a dataset in terms of its potential to be modelled.

Ans: The plot represents the euclidean distance between the variables (x-values) and their response (y-value). By looking at the data, we can understand that their is a positive linear relationship between each of the explanatory variables to that of response. Having this kind of linear relationship helps in modellig data and deriving meaningful information. Additionaly this also helps us to visiualise the data trends, and make predictions about the data when only one of the expalinatory variables are provided.  

```{r}
dist.table <- function(d, response.var = ncol(d))  
{
    d <- scale(d) # scale data
	d.dist <- dist(d[,-response.var])  # distance all X values	
	d.resp <- dist(d[,response.var])
	
	d.dist <- (d.dist-min(d.dist))/(max(d.dist)-min(d.dist))
	d.resp <- (d.resp-min(d.resp))/(max(d.resp)-min(d.resp))
	
	data.frame(cbind(d.dist,d.resp))
}
#
# Example with simple linear response, random X1 and X2 but no noise for response
#
X1 <- runif(100)
X2 <- runif(100)
Y <- X1 + X2
#
ex1 <- data.frame(cbind(X1,X2,Y))
d <- dist.table(ex1, response.var = 3)
plot(x=d$d.dist, y = d$d.resp,xlab="Normalised Distance in Feature Space",
                              ylab="Normalised Distance in Response Space",cex=0.5)

p<-ggplot(ex1, aes(y=ex1$Y)) + 
  geom_line(aes(x=ex1$X1, colour = "red"), size = 0.5) +
  geom_line(aes(x=ex1$X2, colour = "blue"), size=0.5)+
  scale_color_discrete(name = "Explanatory variables", labels = c("X1", "X2"))+
  xlab("Explanatory variables") + ylab("Response variable")

p

```


2. Create a dataset where there is NO relationship between the explanatory and response
variables and plot the distance relationship created by dist.table(…). Explain why you
observe this pattern. INCLUDE THE R CODE TO PRODUCE THE DATASET.

Ans:  To understand the dist.plot function, I plotted the raw data (i.e., before the converstion to normalised distance). It is clear from the graph below that the data set used to generate this graph has no relationship beteen the explanatory and response variables. 
After calculating the normalised distance, the distance plot shows that the normalised distance in response is independent to that of normalised distance in features space (i.e., no relationship between the two variables.)
Calculating the elucidean distance between variables, and subsequently plotting them provides us a clear indication as how explanatory variables and the response variables are interacting with each other. 
```{r}
#nonUniform data with no relationship
X1.nonuniform<-rnorm(1000,mean=10, sd=1)
X2.nonuniform<-rnorm(1000,mean=4, sd=1)
y1.nonuniform<-runif(1000)
y2.nonuniform<-runif(1000)
Y.nonuniform<- runif(y1.nonuniform + y2.nonuniform)

ex1.nonuniform <- data.frame(cbind(X1.nonuniform,X2.nonuniform,Y.nonuniform))
d.nonuniform <- dist.table(ex1.nonuniform, response.var = 3)
plot(x= d.nonuniform$d.dist, y=d.nonuniform$d.resp, ylab="normalised distance in response space",xlab="normalised distance in Feature Space", title("Data with no relationship between explanatory variables and response variables"))

p.nonuniform<-ggplot(ex1.nonuniform, aes(y=ex1.nonuniform$Y)) + 
  geom_line(aes(x=ex1.nonuniform$X1, color = "red"), size = 0.5) +
  geom_line(aes(x=ex1.nonuniform$X2, color = "blue"), size = 0.5) +
  scale_color_discrete(name = "Explanatory variables", labels = c("X1", "X2"))+
  xlab("Explanatory variables") + ylab("Response variable")

p.nonuniform
```

Question 3. Produce a figure with 2 plots using dist.table(...): one using the Boston housing dataset
(library(MASS); data(Boston); response variable medv) and one using the supplied table bioavailabilility dataset (bioavailability.txt; response variable last column (unlabelled)) . Explain which dataset you believe would be easier to model and why. Include a discussion on the relationship between the visualisation created by the function dist.table and how it might relate to a k-nearest neighbour model.



```{r}
library(MASS)
data("Boston")
bioavailabilityDF<-read.table("bioavailability.txt", header=FALSE)
#############################################################
Boston.dist <- dist.table(Boston, response.var = ncol(Boston))
bioavailabilityDF.dist<- dist.table(bioavailabilityDF, response.var = ncol(bioavailabilityDF))

BostonPlot<-plot(x=Boston.dist $d.dist, y = Boston.dist $d.resp)
BioavailabilityPlot<-plot(x=bioavailabilityDF.dist $d.dist, y = bioavailabilityDF.dist $d.resp)


#kNN algorithm tryies to find the distance of Angelina from each point of the
#reference data. 
#To find the distance KNN algorithm makes use of ecludian distance equation
#
```

Part 4. VISUALISATION AND CLUSTERING (25 MARKS)

Question 1. Briefly state the meaning of each variable for the countrystats data.
Ans:
population density (people per km2)- This tells us how crowded the country is. 
income per capita ($) - average personal income per person
purchasing power parity and 
  -compares the pricing between countries which have differnt currencies
  -If two countries have the same 
change in gross domestic product as a percentage (%change in GDP) 
  -This shows the growth in GDP. In a very broad sense 
  -higher GDP growth tends to reflect overall increase in coutry's development.
  
  
Question 2. Visualise the countrystats data to examine the relationships between (and within) countries. Which countries appear to be the similar to New Zealand? Which countries are the strongest/weakest? You will need to consider which features (explanatory variables) are important when doing this assessment, and why.
```{r}
countrystatsDF<-read.csv("countrystats.csv", header = TRUE)
library(tibble)
countrystatsDF<-column_to_rownames(countrystatsDF, "Country")
#Correlation matrix and pairwise plot
round(cor(countrystatsDF),2)
symnum(cor(countrystatsDF,use="complete.obs"))
corrplot(cor(countrystatsDF))

library(gclus)

country.cor <- abs(cor(countrystatsDF))  # Take absolute value of correlation
country.colors <- dmat.color(country.cor)
country.order <- order.single(cor(countrystatsDF))  # Order by correlation

cpairs(countrystatsDF,country.order,panel.color=country.colors,gap=0.5)
```

Question 3. Create two hierarchically clustered dendrograms for the country data after scaling the data to have a mean of zero and standard deviation of one (for each variable). Use the agglomeration methods “average” and “complete”. Produce figures showing each of these dendrograms, and explain why they are different. 

Ans:
```{r}
countrystatsDist<- dist(scale(countrystatsDF))
countrystatsDend.average<-hclust(countrystatsDist, method = "average")
countrystatsDend.complete<-hclust(countrystatsDist, method = "complete")

plot(countrystatsDend.average, hang=-0.5, cex=0.3)
plot(countrystatsDend.complete, hang=-0.5, cex=0.3, xlim = c(1,50) )
```

Q: Finally, using the “complete” linkage dendrogram, cut the dendrogram into 50 clusters (using cutree), and state the countries that are in the same cluster as New Zealand. Provide the R code to do this dendrogram/cutree and analysis to get the country names. 

```{r}
library(lattice)
clusters <-cutree(countrystatsDend.complete, k=50)
clusters
#New Zealand comes under cluster 15. So subsetting all the countrystatDF to all the country which come under cluster15 will give us the countries which are similar to New Zealand.
c1_data = countrystatsDF[clusters == 15,]
c1_data
```


Question 4. Use the dimensionality reduction method t-SNE to create a 2 dimensional plot of the country data. Visualise (plot) the result and compare the nearest 5 countries from New Zealand using the t-SNE mapping to the results in step 3 (HINT: You will need to find out which entry is NewZealand, and “zoom” the plot with text labels; of course you can always calculate this from the results produced by t-SNE). Comment on why they are the same (or different). Include the plot with your answer.

```{r}
library(tsne)
countrystatDist_tsne<-as.data.frame(tsne(countrystatsDist, k=2))
countrystatDist_tsne$countrystatDist_tsne_product<-countrystatDist_tsne$V1*countrystatDist_tsne$V2
head(countrystatDist_tsne)
str(countrystatDist_tsne)
install.packages("ggrepel")

countryNearNZ<-c(23,42,72, 143, 78,143,115)

countrystatDF[countryNearNZ,]
#library (ggplot2)
#library(ggrepel)
p<-ggplot(countrystatDist_tsne, aes(x=V1,y=V2)) + 
  geom_point()+
  geom_point(data=countrystatDist_tsne[115, ], colour = "green")+
  geom_point(data=countrystatDist_tsne[c(23,42,72, 143, 78), ], aes(x=V1, y=V2), colour="red")
  #geom_text_repel(
    #data = countrystatDist_tsne,
    #aes(V1, V2, label = "New Zealand"),
    #size = 5,
    #box.padding = unit(0.35, "lines"),
    #point.padding = unit(0.3, "lines") 
  #)
p



countrystatDist_tsne[115,1]
plot(countrystatDist_tsne)
text(row.names(countrystatsDF))
NZRowNum<-which(row.names(countrystatsDF)=="NewZealand")
NZRowNum
```


















