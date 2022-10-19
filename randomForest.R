# load library
library(dplyr)
library(ggplot2)
library(randomForest)
library(caret)


# load data
conocoPhillips = read.csv("conocoPh.csv")


# select features
conoco = conocoPhillips %>%
    dplyr::select(frac_design,
                  stage_length,
                  ComplType_new,
                  SPACING_CAPPED,
                  X12.month.Cum.Prod,
                  Best1.Mo.BOPD,
                  Best3.Mo.BOPD,
                  Best6.Mo.BOPD,
                  Best9.Mo.BOPD,
                  Best12.Mo.BOPD) %>%
    filter(ComplType_new != "No Data",
           is.finite(frac_design)) %>%
    mutate_if(is.character, as.factor)




# define one-hot encoding function
dummy <- dummyVars(" ~ .", data = conoco)

#perform one-hot encoding on data frame
dummy_conoco <- data.frame(predict(dummy, newdata = conoco))


rf = function(y){
    df = dummy_conoco %>%
        rename("y" = y)
    
    set.seed(489)
    # Random Forest
    # Create the forest.
    output.forest <- randomForest(y ~  
                                      frac_design +
                                      stage_length +
                                      SPACING_CAPPED +
                                      ComplType_new.OH +
                                      ComplType_new.others +
                                      ComplType_new.P...P + 
                                      ComplType_new.P...P..cmt.. + 
                                      ComplType_new.Sleeves + 
                                      SPACING_CAPPED, 
                                  data = df,
                                  na.action = na.omit)
    
    # View the forest results.
    #print(output.forest) 
    
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
dev.off()
names(conocoPhillips)
ggplot(data = na.omit(conocoPhillips),
       aes(x=Latitude,
           y=Longitude,
           color=stage_length)) +
    geom_point(alpha=.9, size=.5)+
    scale_color_viridis_c(direction = -1)+
    theme_classic()

ggplot(data = na.omit(conocoPhillips),
       aes(x=stage_length,
           y=X12.month.Cum.Prod)) +
    geom_point(alpha=.9, size=.5)+
    scale_color_viridis_c(direction = -1)+
    theme_classic()
