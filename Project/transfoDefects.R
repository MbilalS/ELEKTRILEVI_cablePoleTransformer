# JAOTUSALAJAAM -> Breakdown

library(readr)
defectsTrafo <- read_csv("~/Downloads/example_data/elektrilevi/cables_poles_trafos/defectsTrafo.csv")


defectsTrafo[defectsTrafo == '-'] <- NA

trnsfo.grouped <- data.frame(
  manuType = character(),
  age = numeric(),
  transfoID = numeric(),
  name = character(),
  description = character(),
  stringsAsFactors = FALSE
)



defectsTrafo.types <-
  defectsTrafo[!duplicated(defectsTrafo$MANUFTYPE), ]


for (j in 1:nrow(defectsTrafo.types)){
  
  type <- defectsTrafo.types[[8]][j]
  
  if(!is.na(type)){
    transformer.type <-
      defectsTrafo[defectsTrafo$MANUFTYPE == type, ]
    
    
    
    
    for(i in 1:nrow(transformer.type)) {
      
      replDate <- transformer.type[[3]][i]
      conDate <- transformer.type[[5]][i]
      obsDate <- transformer.type[[10]][i]
      
      
      if(!is.na(replDate) | !is.na(conDate) ) {
        
        if(!is.na(replDate)){
          installYear <-  as.numeric(substr(replDate,8,9))
        } else if(!is.na(conDate)) {
          installYear <- as.numeric(substr(conDate,8,9))
        } 
        
        if(installYear >= 0 & installYear <= 20){
          installYear = 2000 + installYear
        } else {
          installYear = 1900 + installYear
        }
        
        obsYear <- as.numeric(substr(obsDate,8,9))
        
        if(obsYear >= 0 & obsYear <= 20){
          obsYear = 2000 + obsYear
        } else {
          obsYear = 1900 + obsYear
        }
        
        transformer.age <- obsYear - installYear
        
        if(transformer.age > 0 & !is.na(transformer.age)){
          
          trnsfo.grouped[nrow(trnsfo.grouped) + 1,] <-
            c(
              transformer.type[[8]][i],
              transformer.age,
              transformer.type[[1]][i],
              transformer.type[[11]][i],
              transformer.type[[13]][i]
            )
          
        }
      }
    }
  }
}


library(dplyr)


trnsfo.grouped <-
  transform(trnsfo.grouped, age = as.numeric(trnsfo.grouped$age))

#grouped_dataaa = group_by(trnsfo.grouped, manuType_final=trnsfo.grouped$manuType, age_final=trnsfo.grouped$age,name_final=trnsfo.grouped$name, description_final=trnsfo.grouped$description)

#df <- summarise(grouped_dataaa, age = mean(age), totalObservations = n())


grouped_data = group_by(trnsfo.grouped, trnsfo.grouped$manuType, trnsfo.grouped$description, trnsfo.grouped$name)

sample.dff <- summarise(grouped_data, age = mean(age), totalObservations = n())


tranfoType <- defectsTrafo.types[[8]][1] # change 1 to n to get new transfer type max 326

print.df <-  sample.dff[sample.dff$`trnsfo.grouped$manuType` == transfoType, ]

library(ggplot2)

trnsfo.grouped <-
  transform(trnsfo.grouped, age = round(as.numeric(trnsfo.grouped$age),digits = 2))


ggplot(print.df, aes(x = factor(print.df$`trnsfo.grouped$description`), y = factor(print.df$age), group = factor(print.df$`trnsfo.grouped$name`))) + 
  geom_bar(stat="identity", position = "dodge") + 
  scale_fill_brewer(palette = "Set1")


