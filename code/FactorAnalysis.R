library(qgraph)
library(GPArotation)


data <- read.csv("result.csv",header=TRUE,sep=",")

## choose one part as an example
## in fact, there are many NA values in this data-set

test <- data[c(1:13),c(4:8)]

summary(test)

## choose the statistical model

fa_no_rot <- factanal(test, factors=2, rotation="none")

## see the 

fa_no_rot$loadings 

fa_no_rot_graph <- qgraph(fa_no_rot$loadings,filetype="jpg",
                          filename="population_eco_loadings_no_rot", 
                          title="Loadings of population -- No rotation")



#### Choose three cities and province, Beijing, Shanghai and Guangdong

Beijing = data[which(data$Region=='Beijing'& data$year>2006),c(3:8)]
Shanghai = data[which(data$Region=='Shanghai'& data$year>2006),c(3:8)]
Guangdong = data[which(data$Region=='Guangdong'& data$year>2006),c(3:8)]


#### Test for High correlation, eg. Beijing
library('psych')
pairs.panels(Beijing)


#### Get the needed number of factors. Around two factors seem necessary.

Beijing.Result<-fa.parallel(Beijing, fm="ml")
Shanghai.Result <- fa.parallel(Shanghai,fm='ml')
Guangdong.Result <- fa.parallel(Guangdong,fm='ml')

#### Build the model

Beijing.Factor<-fa(r=Beijing, nfactors=2, rotate="promax", fm="ml")

#### draw factor plots

fa_no_rot <- factanal(Beijing, factors=2, rotation="none")

fa_no_rot_graph <- qgraph(fa_no_rot$loadings,filetype="jpg",
                          filename="population_eco_loadings_no_rot", 
                          title="Loadings of population -- No rotation")

