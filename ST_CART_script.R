##################################################
##        Blood in the water - CART tree        ##
##################################################

#packages
pkgs = c("dplyr","tidyverse","caTools","rpart","rpart.plot","partykit")
for (pkg in pkgs){
  if (!require(pkg, character.only = T)){
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
  else {library(pkg, character.only = TRUE)}
}

#path where CSV file is located
my_path<-file.path("N:","SharkTank","data sharing package")

#raw data
raw_ST_data <- read.csv(file.path(my_path,"ST_cleaned_data.csv"),header=TRUE)

#Transform variables to factors
data <- transform(
  raw_ST_data,
  Badidea=as.factor(Badidea),
  Offer=as.factor(Offer),
  Accepted=as.factor(Accepted),
  Offeredandrejected=as.factor(Offeredandrejected),
  Licensing=as.factor(Licensing),
  PatentStatus=as.factor(PatentStatus),
  BulkOrders=as.factor(BulkOrders),
  Education=as.factor(Education),
  PastExperience=as.factor(PastExperience),
  TimeDevoted=as.factor(TimeDevoted),
  Endorsement=as.factor(Endorsement),
  MoreThan1SharkBid=as.factor(MoreThan1SharkBid),
  PatentStatus_v2=as.factor(PatentStatus_v2),
  female=as.factor(female),
  mixed_team=as.factor(mixed_team),
  patent1=as.factor(patent1),
  patent2=as.factor(patent2),
  patent3=as.factor(patent3)
)

#Variables relevant for the tree analysis
data_offer <- data[,c("season", "episode_season", "Badidea", "OpenInvestment_mio", "OpenEquity",
                      "Offer", "Licensing", "Duration", "CurrentSales_mio",
                      "CumulativeSales_mio","ProjectedSales_mio", "PatentStatus_v2", "BulkOrders", "TeamSize",
                      "Education", "PastExperience", "TimeDevoted", "Endorsement",
                      "female", "mixed_team")]


data_offer$Offer <- factor(data_offer$Offer,
                           levels = c(0,1),
                           labels = c("No offer", "Offer"))


data_offer$PatentStatus_v2 <- factor(data_offer$PatentStatus_v2,
                                     levels = c(0,0.5,1),
                                     labels = c("No patent effort","Patent filed","Patent obtained"))

data_offer$Education <- factor(data_offer$Education,
                               levels = c(0,1,2,3,4),
                               labels = c("Not disclosed",
                                          "High School",
                                          "Undergraduate",
                                          "Graduate",
                                          "PhD"))

data_offer$TimeDevoted <- factor(data_offer$TimeDevoted,
                                 levels = c(0,1),
                                 labels = c("Part-time","Full-time"))


data_offer<- rename(data_offer, PatentStatus = PatentStatus_v2,
                    Episode= episode_season)


#seed
set.seed(4321)

names(data_offer)[names(data_offer) == 'OpenEquity'] <- 'InitialEquity'

#split sample
sample.offer = sample.split(data_offer, SplitRatio = .8)
train.offer = subset(data_offer, sample.offer == TRUE)
test.offer  = subset(data_offer, sample.offer == FALSE)


### Regression tree (CART)
rtree.fit <- rpart(Offer ~ ., 
                   data=train.offer,
                   method="class", # "anova" for regression tree; "class" for classification tree
                   control=rpart.control(minsplit=30,cp=0.001)) #to control tree growth

#table of fits across cp (complexity parameter) values
printcp(rtree.fit) # display the results

# detailed results including surrogate splits
summary(rtree.fit) 

# plot decision tree 
prp(rtree.fit, type = 3,
    extra = 101, 
    leaf.round = 1, 
    varlen = 0, 
    cex=0.55)


##Prune the tree
# prune the tree based on minimim xerror
pruned.rtree.fit<- prune(rtree.fit, cp= rtree.fit$cptable[which.min(rtree.fit$cptable[,"xerror"]),"CP"])

# plot the pruned tree using prp() in the rpart.plot package 
prp(pruned.rtree.fit, 
    main="Pruned Classification Tree 1 for Likelihood to Receive an Offer",
    extra = 101, 
    type=3)


# prune the tree based on 1 SE error 
pruned2.rtree.fit<- prune(rtree.fit, cp=.01)

# plot the pruned tree using prp() in the rpart.plot package
prp(pruned2.rtree.fit, 
    main="Pruned Classification Tree 2 for Likelihood to Receive an Offer",
    extra = 101, 
    type=3)


#original tree
pred1<-rtree.fit %>% predict(test.offer, type="class")
#pruned tree #1
pred2<-pruned.rtree.fit %>% predict(test.offer, type="class")
#pruned tree #2
pred3<-pruned2.rtree.fit %>% predict(test.offer, type="class")

mean(pred1 == test.offer$Offer)
mean(pred2 == test.offer$Offer)
mean(pred3 == test.offer$Offer)


summary(pruned2.rtree.fit)

###FINAL plot
split.fun <- function(x, labs, digits, varlen, faclen)
{
  # replace commas with spaces (needed for strwrap)
  labs <- gsub(",", ", ", labs)
  for(i in 1:length(labs)) {
    # split labs[i] into multiple lines
    labs[i] <- paste(strwrap(labs[i], width = 25), collapse = "\n")
  }
  labs
}

prp(pruned2.rtree.fit, 
    main=expression(paste("Pruned Classification Tree: ", italic("Likelihood to Receive an Offer"))),
    extra = 101, 
    under=TRUE,
    fallen.leaves = FALSE,
    split.fun = split.fun,
    faclen = 0,
    varlen =0,
    type=5)


#### Conditional inference method
ctree.fit <- ctree(Offer ~ ., 
                   data=train.offer,
                   control=ctree_control(maxdepth=Inf))


print(ctree.fit) # display the results 
plot(ctree.fit,
     main="Classification Condition Inference Tree \n Likelihood to Receive an Offer")


pred4<-ctree.fit %>% predict(test.offer)
mean(pred4 == test.offer$Offer)









