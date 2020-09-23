## ----setup, include=FALSE--------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = FALSE)


## ----TD_Age, echo=FALSE,results='hide',message=FALSE,warning=FALSE---------------------------------------------------------------------------------
if(!require(ggplot2)){install.packages("ggplot2")}
if(!require(ggplot2)){install.packages("plyr")}
if(!require(ggplot2)){install.packages("psych")}
if(!require(ggplot2)){install.packages("reshape2")}
if(!require(FSA)){install.packages("FSA")}
if(!require(ez)){install.packages("ez")}
if(!require(kableExtra)){install.packages("kableExtra")}

require(tidyr)
require(ggplot2)
require(plyr)
require(dplyr)
require(psych)
library(reshape2)
library(ggbeeswarm)
library(ez)
library(FSA)
library(lme4)
library(lmerTest)
library(sjPlot)
library(car)
library(effects)

## Initialisation ----------------------------------------------------------------------------------------------------
date <-Sys.Date()
FileDir <- getwd()

# Filter buttons------------------------------------------------------------------------------------------------------
# 1. Remove NA's and observations with bad adaptive tracks 
CleanData <- 0 # On=1/Off=0 button

# 2. Remove specifc subjects 
RmvSubj <- 1 # On=1/Off=0 button
Subj2Remove <- "APD14"  # c("TD11") 

# 3. Remove subjects based on their quality evaluation made by the examiner on the testing day 
QualityCtrl <- 0 # On=1/Off=0 button
rmvEval <- c("Bad") #  "Good" / "Maybe" / "Bad"

# 4. Remove APD subjects based on their diagnosis 
DiagCtrl <- 0 # On=1/Off=0 button
rmvDiag <- c("LiD","susAPD") #  "APD" / "LiD" (i.e., AP deficit and not a DISORDER) / "susAPD"

# 5. Removce APD subjects WITHOUT SPD patterns 
APDsubTypCtrl <- 0 # On=1/Off=0 button
rmvAPDType <- c(NA, "MissingReoprt") #  "SPD"
# --------------------------------------------------------------------------------------------------------------------

# get demographics ---------------------------------------------------------------------------------------------------
d<- read.csv(file.path('..','Files','AllListenersDemographics_SK.csv'),header=T) 

# Clean data
d[is.na(d),]
d <- na.omit(d) # remove rows with missing data

# calculate age from DOB and testing day
if(!require(eeptools)){install.packages("eeptools")}
library(eeptools)

d$Age <- age_calc(as.Date(d$DOB,"%d/%m/%Y"),
                   as.Date(d$TestDate,"%d/%m/%Y"),
                   units = "years", precise = TRUE)
d$Age <- round(d$Age,1)

d <- d[,-match(c("Group"),names(d))]

# get additional demographics -------------------------------------------------------------------------------------- 
d_Info <- read.csv(file.path('..','Files','BackgroundInfo.csv'),header=T)
d_Info <- d_Info[,-match(c("DOB", "TestingDay","Age", "Sex", "Group","X"),names(d_Info))]

# merge data frames  -----------------------------------------------------------------------------------------------
d <- merge(d_Info,d,by=c("listener"))

# cols <- c("AuditoryTraining","EarProblems","EarProblemsDur","SLT","Grommets","MusicalTraining","FMuse","Otoscopy","Normal.speech.understanding")
# d[cols] <- lapply(d[cols], factor)

## ------------------------------------------------------------------------------------------------------------------
# Some filtering: 
## ------------------------------------------------------------------------------------------------------------------
# Remove certain subjects 
if (RmvSubj==1){d <- d[ ! d$listener %in% Subj2Remove, ] %>% droplevels()}

# Quality control: Include / Subjects based on the quality of their testing 
if (QualityCtrl==1){d <- d[ ! d$ExpEval %in% rmvEval, ] %>% droplevels()}

# Remove APD subjects based on their diagnosis
if (DiagCtrl==1){d <- d[ ! d$Diagnosis %in% rmvDiag, ] %>% droplevels()}

# Include only APD subjects with SPD patterns 
if (APDsubTypCtrl==1){d <- d[ ! d$Subtype %in% rmvAPDType, ] %>% droplevels()}
# -------------------------------------------------------------------------------------------------------------------

# Load LiSN-S data --------------------------------------------------------------------------------------------------
d2<- read.csv(file.path('..','Files','LiSNS_2020-08-19.csv'),header=T) 

# merge the two dataframes
d2 <- merge(d2,d,by=c("listener"))

# Convert Long2Wide by uRevs and zScores
d_LiSNS_w  <- d2  %>%
  pivot_wider(
    id_cols = c("listener","Age","Group"),
    names_from = "CondCode",
    values_from = c("uRevs")) %>%
  ungroup()

# get SRM by listener
d_LiSNS_w$SRM <-  d_LiSNS_w$`LiSNS-S0N0` - d_LiSNS_w$`LiSNS-S0N90`

# change format from wide2long
d_LiSNS <- d_LiSNS_w %>%
  pivot_longer(
    cols = c("SpchInNz","LiSNS-S0N0","LiSNS-S0N90","SRM"), 
    names_to = "CondCode", 
    names_ptypes = list(CondCode = factor()),
    values_to = "uRevs") 

# change conditions order for the plot
d_LiSNS$CondCode <- factor(d_LiSNS$CondCode,levels=c("SpchInNz", "LiSNS-S0N0", "LiSNS-S0N90", "SRM"))
#levels(d_LiSNS$Group)
d_LiSNS$Group <- factor(d_LiSNS$Group,levels=c("TD", "APD"))

# summarise condition across listeners 
ddply(d_LiSNS,~CondCode*Group,summarise,mean=mean(uRevs),sd=sd(uRevs), min=min(uRevs),max=max(uRevs)) 

n_d_LiSNS<- dim(d_LiSNS)[1]

# Filter data by test versions ----------------------------------------------------------------------------------------
# SpchInNz <- d3_s %>% filter(CondCode=="SpchInNz") %>% droplevels() 
# SpchInNz_TD <- d3_s %>% filter(Group=="TD" & CondCode=="SpchInNz") %>% droplevels() 
# SpchInNz_APD <- d3_s %>% filter(Group=="APD" & CondCode=="SpchInNz") %>% droplevels() 

# Filter by Group -----------------------------------------------------------------------------------------------------
d_LiSNS_TD <- d_LiSNS %>% filter(Group=="TD") %>% droplevels() 
d_LiSNS_APD <- d_LiSNS %>% filter(Group=="APD") %>% droplevels() 

# Clean data ----------------------------------------------------------------------------------------------------------

# Remove subjects with no uRev:
d_LiSNS[is.na(d_LiSNS$uRevs),] # APD10: Run 2 & 3
# remove rows with missing data
d_LiSNS <- drop_na(d_LiSNS, uRevs)

# Remove observations with funny adaptive track:
# if (CleanData==1){
#   ## ASL --------------------------------------
#   RmvData_SubjName <- c("APD12","APD13","APD13", "APD14", "APD14", "TD15", "TD15")
#   RmvData_CondCode <- c("ENG_F-d_LiSNS-Alt","ENG_F-d_LiSNS-Alt", "MDR_F-d_LiSNS-Alt", "ENG_F-d_LiSNS-Alt", "MDR_F-d_LiSNS-Alt","Q-d_LiSNS-Alt","AMSSN-d_LiSNS-Alt")
#   RemoveData <- NULL
#   for (i in 1:7){
#     RemoveTemp <- (d_LiSNS[which(d_LiSNS$listener==RmvData_SubjName[i] & d_LiSNS$CondCode==RmvData_CondCode[i]),])
#     RemoveData[i] <- as.numeric(rownames(RemoveTemp))
#   }
#   RemoveData <- sort(RemoveData, decreasing = TRUE)
#   d_LiSNS <- d_LiSNS[ ! d_LiSNS$X %in% RemoveData, ] %>% droplevels()
# }

length(unique(d_LiSNS_TD$listener))
length(unique(d_LiSNS_APD$listener))


## ---- echo=FALSE, results='asis'-------------------------------------------------------------------------------------------------------------------
if(RmvSubj==1){
  cat("* The following subjects were removed: ", Subj2Remove, sep = ", ")
  cat("  \n")}
if(CleanData==1){
  cat("* NAs & observations with bad adaptive tracks were removed.")
  cat("  \n")}
if(QualityCtrl==1){
  cat("* Subjects with the following data quality evaluation were removed: ", rmvEval, sep = ", ")
  cat("  \n")}
if(DiagCtrl==1){
  cat("* Not all APD subjects were selected, subjects with the following diagnosis were removed: ", rmvDiag, sep = ", ")
  cat("  \n")}
if(APDsubTypCtrl==1){cat("* Only listeners with SPD pattern were selected.")}
if(RmvSubj==0 & CleanData==0 & QualityCtrl==0 & DiagCtrl==0 & APDsubTypCtrl==0){cat("* The complete dataset was selected (no filtering)")}


## ----LMEM1,fig.width=15----------------------------------------------------------------------------------------------------------------------------
# mixed-effects model
# * fixed factors: Group, CondCode, Age
# * random intercept: nListener 

model1 <- lmer(uRevs~Group + CondCode + Age + Group : CondCode + Group : Age + CondCode : Age + Group : CondCode : Age + (1|listener), d_LiSNS, REML=FALSE)
summary(model1)

# Option 1
if(!require(cAIC4)){install.packages("cAIC4")}
model1_step <- cAIC4::stepcAIC(model1, direction = "backward", trace = TRUE, data = Pastes)
model4 <- lmer(uRevs~Group + CondCode + Age + (1|listener), d_LiSNS, REML=FALSE)

cat("*Model 4:")
tab_model(model4)

library(effects)
e <- allEffects(model4)
print(e)
plot(e)

# Option 2:
model2 <- lmer(uRevs~Group + CondCode + Age + Group : CondCode + Group : Age + (1|listener), d_LiSNS, REML=FALSE)
summary(model2)
anova(model1,model2)
# ==> drop model 1
model3 <- lmer(uRevs~Group + CondCode + Age + Group : CondCode + (1|listener), d_LiSNS, REML=FALSE)
summary(model3)
anova(model2,model3)
# ==> drop model 2
model4 <- lmer(uRevs~Group + CondCode + Age + (1|listener), d_LiSNS, REML=FALSE)
summary(model4)
anova(model3,model4)
# ==> drop model 3

model5 <- lmer(uRevs~Group + CondCode + (1|listener), d_LiSNS, REML=FALSE)
summary(model5)
anova(model4,model5) # -> model 4 gives the best fit!



## ---- echo=TRUE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------
## calculate z-scores for each Group and condition
#-----------------------------------------------------
#    using un-transformed values 
#    [using scale() for z-score transformation]

d_LiSNS <- d_LiSNS %>%
  group_by(CondCode, Group) %>%
  mutate(z = scale(uRevs))
# Plot
t <- plot(abs(d_LiSNS$z), pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 2, col="red")  # add cutoff line
#text(x=1:length(d_LiSNS$z)+1, y=d_LiSNS$z, labels=ifelse(abs(d_LiSNS$z)>2,names(d_LiSNS$z),""), col="red")

# check for outliers
OutliersZ <- which(abs(d_LiSNS$z) >2) 
# Results: found outliers for |z-scores| > 2: 
# look at the observations:
#e.g.: d_ST[4,] 

library(kableExtra)
knitr::kable(d_LiSNS[OutliersZ,]) %>%
  kable_styling() %>%
  scroll_box(width = "100%", height = "400px")

rmvOutlrs = 0
if (rmvOutlrs==1){
# Remove outliers
d_LiSNS <- d_LiSNS[- which(abs(d_LiSNS$z)>2),] %>% droplevels()
}



## ---- echo=TRUE, results='hide', message=FALSE, warning=FALSE--------------------------------------------------------------------------------------
source("functions/getZ.R")

Output <- getZ(d_LiSNS)

df_normed      <- Output[[1]]
label_TD       <- Output[[2]]
label_APD      <- Output[[3]]
zScores_Sum    <- Output[[4]]
zScores_Sum_TD <- Output[[5]]

# 
# # APD group
# # True positive (TP)
# TP.SpchInNz <- sum(df_normed$Outlier[which(df_normed$Group=="APD" & df_normed$CondCode=="SpchInNz")])
# 
# # False negative (FN)
# FN.SpchInNz <- sum(df_normed$NoOutlier[which(df_normed$Group=="APD" & df_normed$CondCode=="SpchInNz")])
#   
# # TD group
# # False positive (FP)
# FP.SpchInNz <- sum(df_normed$Outlier[which(df_normed$Group=="TD" & df_normed$CondCode=="SpchInNz")])
#   
# # True negetive (TN)  
# TN.SpchInNz <- sum(df_normed$NoOutlier[which(df_normed$Group=="TD" & df_normed$CondCode=="SpchInNz")])
#   
# # Sensitivity (True positive fraction,TPF): TP/(TP+FN)
# TPF.SpchInNz <- TP.SpchInNz/(TP.SpchInNz+FN.SpchInNz)
# 
# # Specificity (False positive fraction, FPF): FP/(FP+TN)
# FPF.SpchInNz <- FP.SpchInNz/(FP.SpchInNz+TN.SpchInNz)
# 
# 
# 
# sum(df_normed$Outlier[which(df_normed$CondCode=="LiSNS-S0N0")])/length(df_normed$Outlier[which(df_normed$CondCode=="LiSNS-S0N0")])
# sum(df_normed$NoOutlier[which(df_normed$CondCode=="LiSNS-S0N0")])/length(df_normed$NoOutlier[which(df_normed$CondCode=="LiSNS-S0N0")])
# 
# plot.roc(df_normed$Outlier[which(df_normed$CondCode=="LiSNS-S0N0")], df_normed$z_trim[which(df_normed$CondCode=="LiSNS-S0N0")],  
#          main="Confidence interval of a threshold", 
#          percent=TRUE,  ci=TRUE, of="thresholds", # compute AUC (of threshold)  
#          thresholds="best", # select the (best) threshold  
#          print.thres="best") # also highlight this threshold on the plot 
# 
# 
# plot.roc(df_normed$Group[which(df_normed$CondCode=="SpchInNz")], df_normed$z_trim[which(df_normed$CondCode=="SpchInNz")],          # data
#          percent = TRUE,                    # show all values in percent
#          partial.auc=c(100, 90), 
#          partial.auc.correct=TRUE,          # define a partial AUC (pAUC)
#          print.auc=TRUE,                    
#          #display pAUC value on the plot with following options:
#          print.auc.pattern = "Corrected pAUC (100-90%% SP):\n%.1f%%",
#          print.auc.col = "#1c61b6",
#          auc.polygon = TRUE, 
#          auc.polygon.col = "#1c61b6",       # show pAUC as a polygon
#          max.auc.polygon = TRUE, 
#          max.auc.polygon.col = "#1c61b622", # also show the 100% polygon
#          main = "Partial AUC (pAUC)")
# 
# plot.roc(df_normed$Group[which(df_normed$CondCode=="SpchInNz")], df_normed$z_trim[which(df_normed$CondCode=="SpchInNz")],
#          percent = TRUE, 
#          add = TRUE, 
#          type = "n",                        # add to plot, but don't re-add the ROC itself (useless)
#          partial.auc = c(100, 90), 
#          partial.auc.correct = TRUE,
#          partial.auc.focus = "se",          # focus pAUC on the sensitivity
#          print.auc = TRUE, 
#          print.auc.pattern = "Corrected pAUC (100-90%% SE):\n%.1f%%", 
#          print.auc.col = "#008600",
#          print.auc.y = 40,                  # do not print auc over the previous one
#          auc.polygon = TRUE, 
#          auc.polygon.col = "#008600",
#          max.auc.polygon = TRUE, 
#          max.auc.polygon.col = "#00860022")



## ---- echo=FALSE,results='hide',message=FALSE,warning=FALSE,fig.width=12, fig.height=4-------------------------------------------------------------
# Plot --------------------------------------------------------------------------------------------------------------------------------------------
# Add performance annotation for certain facets (ugly but works..)
CondCode_labels <- c("SpchInNz"="SpchInNz", 
                     "LiSNS-S0N0"="LiSNS-S0N0",
                     "LiSNS-S0N90"="LiSNS-S0N90",
                     "SRM"="SRM")

ann_text <- data.frame(Group = "TD",z_trim = -4,lab = "d_LiSNS",
                       CondCode = factor("SpchInNz",
                                         levels = c("SpchInNz","LiSNS-S0N0","LiSNS-S0N90","SRM")))
ann_text2 <- data.frame(Group = "TD",zScore = -4,lab = "d_LiSNS",
                       CondCode = factor("SRM",
                                         levels = c("SpchInNz","LiSNS-S0N0","LiSNS-S0N90","SRM")))

t<- ggplot(df_normed, aes(x=interaction(Group,CondCode),y=z_trim))+ 
  #geom_point(shape=1)+ 
  geom_jitter(width = 0.25, shape=1)+ 
  geom_hline(data=zScores_Sum_TD, aes(yintercept=0),colour="black",show.legend = TRUE)+
  geom_hline(data=zScores_Sum_TD, aes(yintercept=1.65),colour="red",linetype="dashed",show.legend = TRUE)+
  geom_hline(data=zScores_Sum_TD, aes(yintercept=-1.65),colour="red",linetype="dashed",show.legend = TRUE)+
  #geom_text(label=df_normed$listener)+
  labs(y = "standardised residual (z-score)" ,x = NULL)+ 
  #geom_text(data = ann_text,label = "d_LiSNS", size=6, nudge_x = 0.4, fontface = "bold") +
  scale_y_continuous(limits = c(-8,8),breaks=seq(-8,8,2))+
  theme_bw()+
  theme(axis.text = element_text(size = 12, face="bold",colour = "black"),
        axis.title.x = element_text(size=14, face="bold"),
        axis.title.y = element_text(size=14, face="bold"),
        legend.title = element_text(size=12, face="bold"),
        legend.text  = element_text(size=12, face="bold"))

t + facet_grid(. ~ CondCode, scales = "free", switch = "y", labeller = labeller(CondCode = CondCode_labels)) +
    geom_text(size    = 5, colour = "red", data    = label_TD, mapping = aes(x = 1, y = 7, label = label), hjust   = 0.5, 
              vjust   = 1.5, check_overlap = TRUE)+
      geom_text(size    = 5,colour = "red", data    = label_APD,mapping = aes(x = 2, y = 7, label = label),hjust   = 0.5,
                vjust   = 1.5, check_overlap = TRUE)+
    geom_text(data = ann_text,label = sprintf("\u2190 better performance"), size = 4,angle = 90, x = 0.5, y = -3) +
    geom_text(data = ann_text2,label = sprintf("better performance \u2192"), size = 4,angle = 90, x = 0.5, y = -3) +
  #annotate("text", y = 4, label = paste(df_normed_outlr$prcntOutlier))+
  #ggtitle("ASL - residuals") + 
  scale_x_discrete(labels = c("TD","APD", "TD","APD", "TD","APD", "TD","APD"))+
  theme(panel.spacing.x = unit(0,"line"),
        strip.background = element_rect(colour="white", fill="white"),
        strip.text = element_text(size = 14,face="bold"),
        plot.title = element_text(size=20,face="bold"),
        legend.position = "bottom")  

#ggsave(paste0('ASL_normed', date, '.png'), width = 8, height = 4, units = c("in"), dpi = 300)


## ---- echo=TRUE, results='hide', message=FALSE, warning=FALSE--------------------------------------------------------------------------------------
# Convert Long2Wide by uRevs and zScores
df_normed_w <- df_normed %>%
  pivot_wider(
    id_cols = c("listener"),
    names_from = "CondCode",
    values_from = c("Group","z_trim","Outlier")) %>%
  ungroup()

#colnames(df_normed_w)[2:11] <- gsub("-", "_", colnames(df_normed_w[,c(2:11)]))
colnames(df_normed_w)[2:length(df_normed_w)] <- gsub("-", "_", colnames(df_normed_w[,c(2:length(df_normed_w))]))


#install.packages("pROC")
library(pROC)  

plot.roc(df_normed_w$Group_SpchInNz,
         df_normed_w$z_trim_SpchInNz,
         main="Confidence interval of a threshold - SSN", print.auc=TRUE,percent=TRUE,  ci=TRUE, of="thresholds",
         thresholds="best",print.thres="best",best.method="youden",boot.n=2000,conf.level=0.95)

plot.roc(df_normed_w$Group_LiSNS_S0N0,
         df_normed_w$z_trim_LiSNS_S0N0,
         main="Confidence interval of a threshold - S0N0", print.auc=TRUE,percent=TRUE,  ci=TRUE, of="thresholds",
         thresholds="best",print.thres="best",best.method="youden",boot.n=2000,conf.level=0.95)

plot.roc(df_normed_w$Group_LiSNS_S0N90,
         df_normed_w$z_trim_LiSNS_S0N90,
         main="Confidence interval of a threshold - S0N90", print.auc=TRUE,percent=TRUE,  ci=TRUE, of="thresholds",
         thresholds="best",print.thres="best",best.method="youden",boot.n=2000,conf.level=0.95)

plot.roc(df_normed_w$Group_SRM,
         df_normed_w$z_trim_SRM,
         main="Confidence interval of a threshold - SRM", print.auc=TRUE,percent=TRUE,  ci=TRUE, of="thresholds",
         thresholds="best",print.thres="best",best.method="youden",boot.n=2000,conf.level=0.95)



## ----LMEM------------------------------------------------------------------------------------------------------------------------------------------
# mixed-effects model
# * fixed factors: Group, CondCode
# * random intercept: nListener 

df_normed_2 <- df_normed %>% filter(CondCode!="SpchInNz" & CondCode!="SRM") %>% droplevels() 

model1 <- lmer(z_trim~Group*CondCode + (1|listener), df_normed_2, REML=FALSE) 

if(!require(cAIC4)){install.packages("cAIC4")}
model1_step <- cAIC4::stepcAIC(model1, direction = "backward", trace = TRUE, data = Pastes)

BestModel <- lmer(z_trim~Group + CondCode + (1|listener), df_normed_2, REML=FALSE) 
summary(BestModel)

cat("*Best model:")
tab_model(BestModel)

Anova(BestModel,type="II",test.statistic="Chisq")

# library(emmeans)
# (ref1 <- lsmeans(model2,~CondCode + Group))
# comps <- contrast(ref1,alpha=0.05,method="pairwise",adjust="bonferroni") #,adjust=NULL) 
# summary(comps)


## ---- echo=TRUE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------
##  Wilcoxon Rank-Sum Test Mann-Whitney U test for independent 2 samples in non-parametric data (when normality assumption is violated)
## ** Without permutation **
if(!require(rstatix)){install.packages("rstatix")}
library(rstatix)
df_normed %>%
  group_by(CondCode) %>%
  wilcox_test(data =., z_trim ~ Group) %>%
  adjust_pvalue(method = "bonferroni") %>%
  add_significance("p.adj")

detach("package:rstatix", unload=TRUE)

# --------------------------------------------------------------------------------------------------
##  Wilcoxon Rank-Sum Test Mann-Whitney U test for independent 2 samples in non-parametric data (when normality assumption is violated)
## ** With permutation **

library(coin)

results_p <- numeric(length(levels(df_normed$CondCode))) 
effectSize <- data.frame()
for (nCond in 1:length(levels(df_normed$CondCode))) {
  #print(paste0("Calculating z-scores for: ", levels(df_normed$CondCode)[nCond],""))
  # get all subjects' scores in a single condition 
  nCondData <- df_normed[which(df_normed$CondCode == levels(df_normed$CondCode)[nCond]),]
  results_p[nCond] <- pvalue(coin::wilcox_test(z_trim ~ Group, data = nCondData,
                                         p.adjust.method ="bonferroni", na.rm=TRUE, paired = FALSE,
                                         distribution=approximate(nresample=999999)))[1]
  # optional: get effect size (r):
  #effectSize[nCond,1:7] <- rstatix::wilcox_effsize(z_trim ~ Group, data = nCondData)
  }

significance <- ifelse(results_p < .05, "sig.","n.s")
knitr::kable(cbind(levels(df_normed$CondCode),round(results_p,5),significance))
#effectSize

# --------------------------------------------------------------------------------------------------
## Permutation test based on "coin" package: 
## Fisher– Pitman permutation test for independent samples (two-sample case)
# See for general description (p408): https://journals.sagepub.com/doi/pdf/10.1177/1536867X0700700307 
# See implementation in r: https://mac-theobio.github.io/QMEE/permutation_examples.html 

results_p <- numeric(length(levels(df_normed$CondCode))) 
for (nCond in 1:length(levels(df_normed$CondCode))) {
  #print(paste0("Calculating z-scores for: ", levels(df_normed$CondCode)[nCond],""))
  # get all subjects' scores in a single condition 
  nCondData <- df_normed[which(df_normed$CondCode == levels(df_normed$CondCode)[nCond]),]
  results_p[nCond] <- pvalue(coin::oneway_test(z_trim ~ Group, data = nCondData,
                                         p.adjust.method ="bonferroni", na.rm=TRUE,
                                         distribution=approximate(nresample=999999)))[1]
}

significance <- ifelse(results_p < .05, "sig.","n.s")
knitr::kable(cbind(levels(df_normed$CondCode),round(results_p,5),significance))

# --------------------------------------------------------------------------------------------------
# d_LiSNS %>%
#   group_by(CondCode,Group) %>%
#   identify_outliers("uRevs")

# df_normed_Q_NoAlt <- df_normed %>% filter(CondCode == "Q-d_LiSNS-NoAlt") %>% droplevels()
# N <- length(df_normed_Q_NoAlt$z_trim)
# z_trim_TD <- subset(df_normed_Q_NoAlt, select=z_trim, Group=="TD", drop=T)
# z_trim_APD <- subset(df_normed_Q_NoAlt, select=z_trim, Group=="APD", drop=T)
# N1 <- length(z_trim_TD)
# 
# 
# observed.d_LiSNS <- median(z_trim_TD)-median(z_trim_APD) 
# observed.d_LiSNS
# ## [1] -1.242261 
# nperms <- 9999
# set.seed(4132)
# result <- numeric(nperms) 
# for(i in 1:nperms){
# index <- sample(N, size=N1, replace = FALSE)
# result[i] <- median(df_normed_Q_NoAlt$z_trim[index]) - median(df_normed_Q_NoAlt$z_trim[-index])
# }
# (sum(result >= observed.d_LiSNS)+1)/(nperms + 1)
# 
# hist(result)

