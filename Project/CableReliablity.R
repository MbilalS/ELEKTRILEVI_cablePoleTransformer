library(readr)
cableReliability <-
  read_csv(
    "~/Downloads/example_data/elektrilevi/cables_poles_trafos/cableReliability.csv"
  )
final.df <- data.frame(
  ClassID = numeric(),
  CableNo = character(),
  CYear = numeric(),
  totalJoints = numeric(),
  jointYears = character(),
  firstJoint = numeric(),
  firstJointYear = numeric(),
  lastJointYear = numeric(),
  averageJointAfterYear = numeric(),
  stringsAsFactors = FALSE
)
result.df <- data.frame(
  ClassID = numeric(),
  AverageFirstJointAfterYears = numeric(),
  MaximumYear = numeric(),
  MinimumYear = character(),
  totalObservations = numeric(),
  stringsAsFactors = FALSE
)
average.joint.df <- data.frame(
  ClassID = numeric(),
  CableNo = character(),
  AverageYear = numeric(),
  stringsAsFactors = FALSE
)
counter <- 0
cableReliability[cableReliability == 0] <- NA
year <- ''
#get all types of cables
cableClass <-
  cableReliability[!duplicated(cableReliability$CLASSID), ]
#show it in the report
cableTypes <- cableClass[, c(3, 5)]
for (i in 1:nrow(cableClass)) {
  # ID of the class (cable)
  classID <-
    cableTypes[[1]][i] # change second brackets to i - counter
  
  if (!is.na(classID)) {
    #All cables belonging to that class
    tmp <-
      cableReliability[which(cableReliability$CLASSID == classID),]
    
    # All cables numbers
    tmpCableNumber <- tmp[!duplicated(tmp$CABLENO), 4]
    
    for (j in 1:nrow(tmpCableNumber)) {
      #Getting cable numbers from 1 to n
      cableNo <-
        tmpCableNumber[[1]][j]  # change second brackets to i - counter
      if (!is.na(cableNo)) {
        # All data about 1 cable
        tmpCable <- tmp[which(tmp$CABLENO == cableNo), ]
        
        #if(nrow(tmpCable[!duplicated(tmpCable$CYEAR),])==1){
        installationYear <-  min(tmpCable[, 10])
        #}
        
        totalJoints <- nrow(tmpCable[!duplicated(tmpCable$JID), ])
        
        x <- tmpCable[!duplicated(tmpCable$JID), ][, 9]
        x <- x[order(x$JYEAR),]
        for (k in 1:nrow(x)) {
          
          year <- paste(year, x[[1]][k], sep = " ")
        }
        
        
        
        someValue <- min(x) - installationYear
        
        if (someValue > -1 & !is.na(someValue)) {
          final.df[nrow(final.df) + 1,] <-
            c(
              classID,
              cableNo,
              installationYear,
              totalJoints,
              year,
              min(x),
              min(x) - installationYear,
              max(x) - installationYear,
              (max(x) - installationYear) / totalJoints
            )
          
        }
        year <- ''
      }
    }
  }
  else {
    counter <- counter+1
  }
}
print(counter)
#get all types of cables - here some cables might be filtered out by the previous process so re calculate it
cid <-
  final.df[!duplicated(final.df$ClassID), ]
for (l in 1:nrow(cid)) {
  cid.id <-
    cid[[1]][l]
  
  if (!is.na(cid.id)) {
    #All cables belonging to that class
    cid.tmp <-
      final.df[which(final.df$ClassID == cid.id),]
    
    cid.tmp <-
      transform(cid.tmp, firstJointYear = as.numeric(cid.tmp$firstJointYear))
    
    cid.tmp <-
      transform(cid.tmp, lastJointYear = as.numeric(cid.tmp$lastJointYear))
    
    average.first.join <- sum(cid.tmp$firstJointYear) / nrow(cid.tmp)
    
    
    max.year <- max(cid.tmp$firstJointYear)
    
    min.year <- min(cid.tmp$firstJointYear)
    
    result.df[nrow(result.df) + 1,] <-
      c(
        cid.id,
        average.first.join,
        max.year,
        min.year,
        nrow(cid.tmp)
      )
    
  }
  
}
library(dplyr)
final.df <-
  transform(final.df, averageJointAfterYear = as.numeric(final.df$averageJointAfterYear))
grouped_data = group_by(final.df, classID = final.df$ClassID)
df <- summarise(grouped_data, average_life = round(mean(averageJointAfterYear)), totalObservations = n())
# Converting the feilds to numeric so that we can sort them out later to get the best/worst cables etc
result.df <-5
transform(result.df, AverageFirstJointAfterYears = as.numeric(result.df$AverageFirstJointAfterYears))
result.df <-
  transform(result.df, totalObservations = as.numeric(result.df$totalObservations))









top <-  filter(df, df$classID == 3527 | df$classID == 3530 | df$classID == 1569
               | df$classID == 1570 | df$classID == 3526)

a <- data.frame(0, 0, 0)
names(a)<-c("classID", "average_life", "totalObservations")
top <- rbind(a, top)
Type_of_cable <- top$classID

Life <- as.factor(top$average_life)
number_of_time_used <- as.factor(top$totalObservations)

library(plotly)

top %>% 
  plot_ly() %>%
  layout(yaxis = list(range = c(0, 6))) %>%
  add_trace(x = ~Type_of_cable, y = ~Life, type = 'bar')





