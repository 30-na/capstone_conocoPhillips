#1. Add lat long as independent variables in the model. Do not scale them.
#2. Add the correlated features (LL, Stages, Fluid, Prop) and do scale them.
#3. Increase cv value from 3 to 5.
#4. Add max tree depth from 15 to around 25.
#5. Increase test proportion from .25 to .3
#6. Increase n_estimators for RF to around 500, 600, 700, 800, upto 1000. Remove n_estimators less than 200.
#7. Fit the resultant rf and dt on the training data and add the predicted column to the data. 
#8. Sort the rows according to the predicted values in descending order and store the top 500 rows in a new dataframe. Show me the work in a ipynb.




# load library
library(dplyr)
library(ggplot2)
library(randomForest)
library(caret)
library(ggpubr)
library(tidyr)

# load data
conocoPhillips = read.csv("conocoPh.csv")
names(conocoPhillips)
str(conoco)
# scale
conocoScale = conocoPhillips %>%
    dplyr::select(frac_design,
                  stage_length,
                  SPACING_CAPPED) %>%
    mutate(frac_design = ifelse(is.infinite(frac_design), 
                                NA,
                                frac_design)) %>%
    scale()


# select features
conoco = conocoPhillips %>%
    dplyr::select(ComplType_new,
                  X12.month.Cum.Prod,
                  Latitude,
                  Longitude,
                  Best1.Mo.BOPD,
                  Best3.Mo.BOPD,
                  Best6.Mo.BOPD,
                  Best9.Mo.BOPD,
                  Best12.Mo.BOPD) %>%
    mutate(frac_design = conocoScale[,1],
           stage_length = conocoScale[,2],
           SPACING_CAPPED = conocoScale[,3]) %>%
    filter(ComplType_new != "No Data",
           is.finite(frac_design)) %>%
    mutate_if(is.character, as.factor)




# define one-hot encoding function
dummy <- dummyVars(" ~ .", data = conoco)

#perform one-hot encoding on data frame
dummy_conoco <- data.frame(predict(dummy, newdata = conoco))

names(dummy_conoco)

set.seed(489)
# train and test
inTraining = createDataPartition(dummy_conoco$X12.month.Cum.Prod, p = .70, list = FALSE)
training = dummy_conoco[ inTraining,]
testing  = dummy_conoco[-inTraining,]


rf = function(y){
    df = training %>%
        rename("y" = y)
    
    set.seed(489)
    
    # Random Forest
    # Create the forest.
    output.forest <- randomForest(X12.month.Cum.Prod ~  
                                      frac_design +
                                      stage_length +
                                      SPACING_CAPPED +
                                      ComplType_new.OH +
                                      ComplType_new.others +
                                      ComplType_new.P...P + 
                                      ComplType_new.P...P..cmt.. + 
                                      ComplType_new.Sleeves + 
                                      Latitude+
                                      Longitude+
                                      SPACING_CAPPED,
                                  data = dummy_conoco,
                                  na.action = na.omit,
                                  ntree=500,
                                  maxnodes=25,
                                  importance=TRUE)
    
    # View the forest results.
    print(output.forest)
    
    # Importance of each predictor.
    pdf(file=paste("IV_" ,y , ".pdf"))
    varImpPlot(output.forest, pch = 18,
               main = paste("Importance of Variables for" , y))
    dev.off()

}


response = c("X12.month.Cum.Prod",
             "Best1.Mo.BOPD",
             "Best3.Mo.BOPD",
             "Best6.Mo.BOPD",
             "Best9.Mo.BOPD",
             "Best12.Mo.BOPD")
for(i in 1:length(response)){
    rf(response[i])
}








names(conocoPhillips)
data = conocoPhillips %>%
    na.omit(stage_length)
g = ggplot(data = data,
       aes(x=Latitude,
           y=Longitude,
           color=stage_length)) +
    geom_point(alpha=.9, size=.9)+
    scale_color_viridis_c(direction = -1)+
    theme_classic()
ggsave("map_stageLength.jpg",
       g,
       height=4,width=8,scale=1.2)


g = ggplot(data = data,
       aes(x=stage_length,
           y=X12.month.Cum.Prod)) +
    geom_point(alpha=.9, size=.9)+
    theme_classic()+
    geom_smooth(method="lm", col="blue")+
    stat_regline_equation(label.x = 20000, label.y = 500)+
    stat_cor(method = "pearson", label.x = 20000, label.y = 450)
ggsave("reg_stageLength_prof.jpg",
       g,
       height=4,width=8,scale=1.2)

# neural network and xgboost
# tune for xgboost 