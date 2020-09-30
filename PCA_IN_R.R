# title: "Principal Component Analysis in R"
# author: "Arpan"
# date: "January 4, 2017"
# output: pdf_document
  


#Importing the library MASS for iris dataset 


library(MASS,quietly = TRUE)


#Storing the data set named "iris" into DataFrame named "DataFrame"

DataFrame <- iris


#Type help("iris") to know about the data set 
help("iris")


#Lets check out the structure of the data 
str(DataFrame)

#Check the dimension of this data frame
dim(DataFrame)


#Check first 3 rows
head(DataFrame,3)

#Check the summary of data 
summary(DataFrame)

#Check the number of unique values 
apply(DataFrame,2,function(x) length(unique(x)))

#Lets check the data set again
str(DataFrame)

#Let's do the principal companent analysis.Center=TRUE and scale=TRUE means we are scaling and centering the data before PCA.
modelPCA<-prcomp(x=DataFrame[,1:4],
center = TRUE,
scale. = TRUE)

#Plot the variance explained by principal components
plot(modelPCA,type = "l",
main="Variance explained by PCA"
)

#Let's find the variance explained by the first two Principal components(PC's)
sum(modelPCA$sdev[1:2]^2)/sum(modelPCA$sdev^2)

#We can see from above that only first two principal components alone can explain 95.8 % variance in the data 
                                                                       
#Let's check the complete summary of PCA.It shows the standard deviance and  variance explained by each of the PCA components.Cumulative proportion of PC3 is 0.994 which means if we use first three components together in the data then these three components alltogether explains 99.4 % variablility in the data set.
                                                                         
                                                                         
summary(modelPCA)
 
#We can see from above that sum of proportion of variance explained by first two  principal components is 95.8 % (0.7296+0.2285).
                                                                        
#Let's plot the biplot showing first two PC's and the original feature vectors in this 2D space i.e original feature vectors as linear combination of first two PC's
biplot(modelPCA)
                                                                        
#Let's try to do data visualization .We will use the principal component feature vectors instead of actual feature vectors like sepal-width,petal-width,etc.We will then color the data points based on Species variable.It is very easy to see that our PCA has worked!! Just based on two principal components we can see three clusters of setosa,versicolor,virginica in the data which are clearly separate.
library(ggplot2)
ggplot(as.data.frame(modelPCA$x[,1:2]))+geom_point(aes(x=PC1,y=PC2),                                        col=colors()[c(55,150,300)][as.factor(DataFrame$Species)])
                                                                        