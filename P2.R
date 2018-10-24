############################################### references#################################################
# for entropy
#https://www-users.cs.umn.edu/~kumar001/dmbook/ch4.pdf
#https://www.slideshare.net/marinasantini1/lecture-4-decision-trees-2-entropy-information-gain-gain-ratio-55241087
#:https://math.stackexchange.com/questions/712407/correct-algorithm-for-shannon-entropy-with-r
#Example of how entropy was calculated for feature V2
#
#e    p     total 
#d 1160  778    1160+778 = 1938
#g  824  428    824+428  = 1252
#l  146  359    146+359 =  505 
#m  150   24    150+24  = 174
#p   85  617    85+617  = 702
#u   54  166    54+166  = 220
#w  116    0    116+0   = 116
#sum     4907
#
#information gain(V2) - sum of
#infogain(d) - > infogain(1160,778)=entropy(1160/1938,778/1938) = -1160/1938*log(1160/1938) - 778/1938*log(778/1938) = 0.67
#infogain(g) - > infogain(824,428)=entropy(824/1252,428/1252) = so on....
#infogain(l) - > infogain(146,359)=entropy(146/505,359/505)
#infogain(m) - > infogain(150,24)=entropy(150/174,24/174)
#infogain(p) - > infogain(85,617)=entropy(85/702,617/702)
#infogain(u) - > infogain(54,166)=entropy(54/220,166/220)
#infogain(w) - > infogain(116,0)=entropy(116/116,0/116)

#expected infogain(V2) - 
#  1938/4907 * 0.67 + ........
# entropy = 1- expected infogain(V2)
#total information gain  = parent (node) - expected infogain(V2)

############################################################################################


library(MASS)
library(ggplot2)
library(scales)
library(gridExtra)
library(modelr)
library(ineq)

mushroom <- read.csv(file="C:\\SAProject\\Mushroom\\train.csv",header=TRUE,stringsAsFactors = TRUE)

names(mushroom)
head(mushroom$V1)
unique(mushroom$V1)

output.mush <- matrix(ncol=5, nrow=ncol(mushroom)-2)

colnames(output.mush)<-c("Feature","featureAccuracy","featureGini","1-Entropy")

for(t in 2:24)
{ 
  
    featureAccuracy=1
    featureGini  = 1
    
    ########## change feature to calculate values#########################
    tt<-table( mushroom[,t],mushroom$V1)
    #tt<-table( mushroom$V2,mushroom$V1)
    tt
    
    # Accuracy of feature , pick max of each partition
    prob.featureAccuracy<-prop.table(tt,1)
    lptt<-length(prob.featureAccuracy)/2
    
    
      for (j in 1:lptt)
      {
        max.feature <- max(prob.featureAccuracy[j,1],prob.featureAccuracy[j,2])
        #print(max.feature)
        featureAccuracy <-featureAccuracy*(max.feature)
      }
    
    print('feature')
    print('V2')
    print('Accuracy')
    print(featureAccuracy)
    
    # gini index
      for (i in 1:lptt)
      {
        sumptt<-prob.featureAccuracy[i,1]*prob.featureAccuracy[i,1]+ prob.featureAccuracy[i,2]*prob.featureAccuracy[i,2]
        featureGini<-featureGini*(sumptt)
      }
      
      print('Gini Index')
      print(featureGini)
      
    
    #Entropy  
 
    length.tt = length(tt)/2
    length.tt
    lambda = 1 # laplace smoothing factor
    
    for ( r in length.tt)
    {
      
    tt[r,1] <- tt[r,1] + lambda
    tt[r,2] <- tt[r,2] + lambda
    }
    tt
    
    prop.tt<- prop.table(tt,1)
    total.tt<-sum(tt)
    total.tt
    
    total.expected.entropy.feature = 0
    for (k in length.tt)
    {
    
      entropy.tt          <-  - prop.tt[k,1] * log(prop.tt[k,1],base=2)  -  prop.tt[k,2] * log(prop.tt[k,2],base=2)
      print(entropy.tt)
      sum.tt              <- tt[k,1] + tt[k,2]
      print(sum.tt)
      expected.entropy.tt <- entropy.tt * (sum.tt/total.tt)
      print(expected.entropy.tt)
      total.expected.entropy.feature <- total.expected.entropy.feature + expected.entropy.tt
      print(total.expected.entropy.feature)
    }
    
    
    
    print('Entropy')
    print(total.expected.entropy.feature)
    
    print(round(cbind(featureAccuracy, featureGini,1-total.expected.entropy.feature),5))
    
    
    print(colnames(mushroom)[t])  
    output.mush[t-2,1]<-colnames(mushroom)[t]
    output.mush[t-2,2]<-featureAccuracy
    output.mush[t-2,3]<-featureGini
    output.mush[t-2,4]<-1-total.expected.entropy.feature
    output.mush[t-2,5]<-round(featureAccuracy*(1-total.expected.entropy.feature),4)
    
}
colnames(output.mush)<-c("Feature Name","Feature Accuracy","Gini Index","1- Entropy", "Importance(Descending order)")
MyData <- output.mush[order(output.mush[,5], decreasing = TRUE),]

write.csv(MyData, file = "C:\\SAProject\\MyData.csv")












