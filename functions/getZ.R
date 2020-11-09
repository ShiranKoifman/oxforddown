# [MyList] <- getZ(df,CutOff)
# The function calculates z-scores based on a control group
# following Ramus et al., (2003) Multiple case study approach
# SK, August 2020
# To do: make the function more general by indicating the parameters in the function input rather than a data frame.

# Inpuut:
# df: must be a data frame with the following coumns:
# - group (TD/APD)
# - CondCode
# - uRevs
# CutOff: the deviance cut-off for normal scores (e.g., 1.96 or 1.65)
#
# Output:
# 'MyList': a list of data frames, e.g., MyList[[1]] = df_normed; MyList[[2]] = label_TD'.
# 'df_normed'
# 'label_TD'
# 'label_APD'
# 'zScores_Sum'
# 'zScores_Sum_TD'

### 1. Calculate z-residuals based on TD subjects only --------------------------------------------------- 
getZ <- function(df,CutOff){
  
  # set the (=/-) two-tailed cut-off value for TD trimming
  CutOff <- CutOff
  
  # Quick fix for now! $$$$$$$$$$
  for (n in 1:length(colnames(df))) {
  if("group" %in% colnames(df)[n])
  {
    colnames(df)[n] <- "Group"
  }
  }
  
for (nCond in 1:length(levels(df$CondCode))) {
  #print(paste0("Calculating z-scores for: ", levels(df$CondCode)[nCond],""))
  # get all subjects' scores in a single condition 
  nCondData <- df[which(df$CondCode == levels(df$CondCode)[nCond]),]
  # get only TD scores for that condition
  nCondData_TD <- df[which(df$CondCode == levels(df$CondCode)[nCond] & df$Group=="TD"),]
  
  # -----
  # apply lm
  r = lm(uRevs ~ Age, data=nCondData_TD)
  sdRes <- sd(r$residuals)
  slope <- r$coefficients[2]
  intcpt <- r$coefficients[1]
  
  # now calculate z scores for all subjects
  nCondData$predicted <- nCondData$Age * slope + intcpt
  # sigma(r) function in r equals: sqrt(sum((r$residuals^2)/(N_TD-2)))
  # sqrt(sum((r$residuals^2)/(length(unique(nCondData_TD$listener))-2)))
  
  nCondData$zScores <- (nCondData$uRevs-nCondData$predicted)/sigma(r) # [SK, July 2020] 
  # nCondData$zScores <- (nCondData$uRevs-nCondData$predicted)/sdRes
  # -----
  
  # nCondData <- getZscores(nCondData_TD,nCondData)
  
  # add all the data into a single data frame (ugly solution but works..)
  if (nCond==1){
    df_normed <- nCondData
  } else {
    df_normed <- rbind(df_normed,nCondData)
  }
}


### 2. find outliers in the TD group with z-residuals outside +/-1.65 sd and remove them -----------------

for (nCond in 1:length(levels(df$CondCode))) {
  nCondData_TD <- df_normed[which(df_normed$CondCode == levels(df_normed$CondCode)[nCond] & df_normed$Group=="TD"),]
    Data.TD_NoOutl <- subset(nCondData_TD, nCondData_TD$Group=="APD" | nCondData_TD$Group=="TD" & abs(nCondData_TD$zScores) < CutOff)
  
  # add all the data into a single data frame (ugly solution but works..) 
  if (nCond==1){
    df_normed_NoOutl <- Data.TD_NoOutl
  } else {
    df_normed_NoOutl <- rbind(df_normed_NoOutl,Data.TD_NoOutl)
  }
}

### 3. Calculate z-residuals AGAIN based on the trimmed TD subjects --------------------------------------

for (nCond in 1:length(levels(df$CondCode))) {
  nCondData <- df[which(df$CondCode == levels(df$CondCode)[nCond]),]
  # get TD scores for that condition
  nCondData_TD <- df_normed_NoOutl[which(df_normed_NoOutl$CondCode == levels(df_normed_NoOutl$CondCode)[nCond] & df_normed_NoOutl$Group=="TD"),]
  
  # -----
  # apply lm 
  r = lm(uRevs ~ Age, data=nCondData_TD) 
  sdRes <- sd(r$residuals)
  slope <- r$coefficients[2]
  intcpt <- r$coefficients[1]
  
  # now calculate z scores for all subjects
  nCondData$predicted_trimmed <- nCondData$Age * slope + intcpt
  #### SK, July 2020 -------------------------------------------------
  # all calculation below were made based on z_trim
  nCondData$z_trim <- (nCondData$uRevs-nCondData$predicted_trimmed)/sigma(r)
  ###### -------------------------------------------------------------
  # nCondData$z_trim <- (nCondData$uRevs-nCondData$predicted_trimmed))/sdRes
  # -----
  # nCondData <- getZscores(nCondData_TD,nCondData)
  
  # add all the data into a single data frame (ugly solution but works..) 
  if (nCond==1){
    df_normed2 <- nCondData
  } else {
    df_normed2 <- rbind(df_normed2,nCondData)
  }
}

# add trimmed zscores to main data frame
df_normed$z_trim <- df_normed2$z_trim

### 4. get averaged z-residuals before and after trimming by CondCode & group --------------------------

if(!require(doBy)){install.packages("doBy")}
library(doBy)

# get summary for z-residuals BEFORE (zScores) and AFTER trimming (z_trim)
# ! The trimmed mean is used for the figures !
zScores_Sum <- summaryBy(uRevs + zScores + z_trim ~ CondCode*Group, data = df_normed,
                         FUN = function(x) { c(m = mean(x,na.rm=TRUE), s = sd(x,na.rm=TRUE)) } )

# filter for TD only for the figures
zScores_Sum_TD <- zScores_Sum %>% filter(Group=="TD") %>% droplevels() 


# Get statistics about outliers in each group for the plot below
# ----------------------------------------------------------------

# Find any outliers for kids with scores below or above the norms:
# df_normed$Outlier <- ifelse(df_normed$z_trim > 1.65, 1,0)
# df_normed$NoOutlier <- ifelse(df_normed$z_trim < 1.65, 1,0)

for (i in 1:dim(df_normed)[1]){
  if (df_normed$zDirection[i]<0){
    df_normed$Outlier[i] <- ifelse(df_normed$z_trim[i] > CutOff, 1,0)
    } else {
      df_normed$Outlier[i] <- ifelse(df_normed$z_trim[i] < -CutOff, 1,0)
      }
}

for (i in 1:dim(df_normed)[1]){
  if (df_normed$zDirection[i]<0){
    df_normed$NoOutlier[i] <- ifelse(df_normed$z_trim[i] < CutOff, 1,0)
  } else {
    df_normed$NoOutlier[i] <- ifelse(df_normed$z_trim[i] > -CutOff, 1,0)
  }
}

# ----------------------------------------------------------------

# get percentage
df_normed_outlr <- ddply(df_normed,~CondCode*Group,summarise,nConde= length(listener),nOutlier=sum(Outlier, na.rm=TRUE),prcntOutlier=round((sum(Outlier,na.rm=TRUE)/length(listener)*100),1))

# prepare lables for the plots below
df_normed_outlr$label <- sprintf("%s%%",df_normed_outlr$prcntOutlier)
label_TD <- df_normed_outlr[which(df_normed_outlr$Group=="TD"),]
label_APD <- df_normed_outlr[which(df_normed_outlr$Group=="APD"),]

# -------------------------------------------------------------------------------------------------------------------------------

# gather the data frames into a list for the function output
MyList <- list(df_normed,label_TD,label_APD,zScores_Sum,zScores_Sum_TD)

return(MyList)

}
