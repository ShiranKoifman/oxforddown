# ST models for Age effect without APD siblings (03/02/2021)

```{r,label='ST-zAssumptions', message=FALSE, warning=FALSE, include=FALSE, results='hide'}
# remove CCRM_F condition
CCRM_s <- CCRM %>% filter(CondCode!="CCRM_F-CCRM-Alt") %>% 
  dplyr::select(., listener, Group, APDsibling, material, CondCode, z_trim) %>% droplevels()

# remove material affiliation in conditions name
CCRM_s$CondCode <- revalue(CCRM_s$CondCode, c("Q-CCRM-NoAlt"="Q-NoAlt","Q-CCRM-Alt"="Q-Alt",
                                              "AMSSN-CCRM-Alt"="AMSSN-Alt","MDR_F-CCRM-Alt"="MDR_F-Alt",
                                              "ENG_F-CCRM-Alt"="ENG_F-Alt"))
ASLN_s <- ASLN %>% dplyr::select(., listener,APDsibling, Group, material, CondCode, z_trim) %>% droplevels()
ASLN_s$CondCode <- revalue(ASLN_s$CondCode, c("Q-ASLN-NoAlt"="Q-NoAlt","Q-ASLN-Alt"="Q-Alt",
                                              "AMSSN-ASLN-Alt"="AMSSN-Alt","MDR_F-ASLN-Alt"="MDR_F-Alt",
                                              "ENG_F-ASLN-Alt"="ENG_F-Alt"))

ST <- rbind(ASLN_s, CCRM_s)
ST <- data.frame(ST)

ST$material <- factor(ST$material,levels=c("ASLN","CCRM"))
ST$listener <- factor(ST$listener)
# str(ST)
# levels(ST$material)
# levels(ST$Group)
# levels(ST$CondCode)

########################### Test assumptions #########################################
# For ST combined data (z-scores)

# Normality: not met
# homoscedasticity: met

# linear model
w1 <- lm(z_trim ~ CondCode * Group * material, data = ST)

#### normality (shapiro-wilk) ###########
# Do all the points fall approximately along the reference line? If YES we can assume normality.
# ggqqplot(residuals(w1))

# Option 1:
# linearity is met if p>.05
shapiro.test(residuals(w1))
# W = 0.94461, p-value = 1.375e-11 --> NOT normaly distr.

# Option 2: by fixed factors: --> mostly normal..
NormTest <- ST %>%
  group_by(CondCode, Group, material) %>%
  rstatix::shapiro_test(z_trim)

#### Homogeneity of variance (Levene's test) ###########
# homogeneity is met if p>.05

# Option 1: Is the lm model fully covered here?
leveneTest(residuals(w1) ~ CondCode * Group * material, data = ST)
# -> homogeneity of the full model is met

# Levene's Test for Homogeneity of Variance (center = median)
#        Df F value Pr(>F)
# group  19  0.8729 0.6174
#       410    

# Option 2: by fixed factors
# per condition
# VarTest <- ST %>%
#   rstatix::group_by(CondCode) %>%
#   levene_test(z_trim ~ Group*material)

# VarTest <- ST %>%
#   rstatix::group_by(Group) %>%
#   levene_test(z_trim ~ CondCode*material)

# VarTest <- ST %>%
#   rstatix::group_by(material) %>%
#   levene_test(z_trim ~ CondCode*Group)

# Option 2: calculate Levene's test by hand, using the model.
# Based on: https://ademos.people.uic.edu/Chapter18.html [see 6.2]

ST$BestModel.Res<- residuals(w1) #extracts the residuals and places them in a new column in our original data table
ST$Abs.BestModel.Res <-abs(ST$BestModel.Res) #creates a new column with the absolute value of the residuals
ST$BestModel.Res2 <- ST$Abs.BestModel.Res^2 #squares the absolute values of the residuals to provide the more robust estimate
Levene.BestModel <- lm(BestModel.Res2 ~ listener, data=ST) #ANOVA of the squared residuals
anova(Levene.BestModel) #displays the results --> homogeneity of variance is marginally significant (p=0.04), i.e., homogeneity is not met..

boxplot(residuals(w1) ~ CondCode * Group * material,data = ST)

plot(BestModel)

t1 <- ggplot(ST, aes(x=interaction(CondCode,Group,material), y=BestModel.Res)) +
  geom_point(size=2,alpha=1) +
  # geom_text(label=ASLN$listener, size=3)+
  # labs(y = "SRdTs (proportion of duty cycle)",x = "Age (in years)") + 
  # scale_y_continuous(limits = c(-0.05,1.05),breaks=seq(0,1,0.1)) +
  # scale_x_continuous(breaks=seq(7,14,1),labels=c("7","8","9","10","11","12","13","18-35"))+
  theme_bw() +
  theme(axis.text = element_text(size = 10, face="bold",colour = "black"),
        axis.title.x = element_text(size=11, face="bold"),
        axis.title.y = element_text(size=11, face="bold"),
        legend.title = element_text(size=11, face="bold"),
        legend.text  = element_text(size=10, face="bold"),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"))
```

```{r,label='ST-AgeEffect', echo=FALSE,results='hide',message=FALSE,warning=FALSE, include=FALSE}
##### Age effect? ################################################################# 

TD_trimmed.CCRM_s <- TD_trimmed.CCRM %>% filter(CondCode!="CCRM_F-CCRM-Alt" & Group=="TD") %>% 
  dplyr::select(., listener, Age,Group, material, CondCode, uRevs,APDsibling) %>% droplevels()

# remove material affiliation in conditions name
TD_trimmed.CCRM_s$CondCode <- revalue(TD_trimmed.CCRM_s$CondCode, c("Q-CCRM-NoAlt"="Q-NoAlt","Q-CCRM-Alt"="Q-Alt",
                                                                    "AMSSN-CCRM-Alt"="AMSSN-Alt","MDR_F-CCRM-Alt"="MDR_F-Alt",
                                                                    "ENG_F-CCRM-Alt"="ENG_F-Alt"))
TD_trimmed.CCRM_s <- data.frame(TD_trimmed.CCRM_s)
TD_trimmed.ASL_s <- ASLN %>% filter(Group=="TD") %>% dplyr::select(., listener, Age,Group, material, CondCode, uRevs,APDsibling) %>% droplevels()
TD_trimmed.ASL_s$CondCode <- revalue(TD_trimmed.ASL_s$CondCode, c("Q-ASLN-NoAlt"="Q-NoAlt","Q-ASLN-Alt"="Q-Alt",
                                                                  "AMSSN-ASLN-Alt"="AMSSN-Alt","MDR_F-ASLN-Alt"="MDR_F-Alt",
                                                                  "ENG_F-ASLN-Alt"="ENG_F-Alt"))
TD_trimmed.ASL_s <- data.frame(TD_trimmed.ASL_s)

ST_trimmed <- rbind(TD_trimmed.ASL_s,TD_trimmed.CCRM_s)
ST_trimmed <- data.frame(ST_trimmed)

######## lmer model ##########################################
# !!! Model with +(1|APDsibling) as random intercept resulted in singular fit!

## Find Best fit 
# start with a saturated model (3-way interaction with age)
model1 <- lmer(uRevs~CondCode*material*Age+(1|listener), ST_trimmed, REML=FALSE)
summary(model1)

(model2<-update(model1,  ~ . - CondCode:material:Age))
anova(model1,model2)
summary(model2)
# model2: uRevs ~ CondCode + material + Age + (1 | listener) + CondCode:material + 
# model2:     CondCode:Age + material:Age
# model1: uRevs ~ CondCode * material * Age + (1 | listener)
#        npar     AIC     BIC logLik deviance  Chisq Df Pr(>Chisq)
# model2   18 -412.75 -351.34 224.37  -448.75                     
# model1   22 -409.33 -334.27 226.66  -453.33 4.5795  4     0.3332

(model3<-update(model2,  ~ . - material:Age))
anova(model2,model3)
summary(model3)
# model3: uRevs ~ CondCode + material + Age + (1 | listener) + CondCode:material + 
# model3:     CondCode:Age
# model2: uRevs ~ CondCode + material + Age + (1 | listener) + CondCode:material + 
# model2:     CondCode:Age + material:Age
#        npar     AIC     BIC logLik deviance  Chisq Df Pr(>Chisq)  
# model3   17 -410.96 -352.96 222.48  -444.96                       
# model2   18 -412.75 -351.34 224.37  -448.75 3.7857  1    0.05169 .

(model4<-update(model2,  ~ . - CondCode:material))
anova(model2,model4)
summary(model4)
# model4: uRevs ~ CondCode + material + Age + (1 | listener) + CondCode:Age
# model3: uRevs ~ CondCode + material + Age + (1 | listener) + CondCode:material + 
# model3:     CondCode:Age
#        npar     AIC     BIC logLik deviance  Chisq Df Pr(>Chisq)  
# model4   13 -409.89 -365.54 217.94  -435.89                       
# model3   17 -410.96 -352.96 222.48  -444.96 9.0698  4    0.05938 .

(model5<-update(model4,  ~ . - CondCode:Age))
anova(model4,model5)
summary(model5)
# model5: uRevs ~ CondCode + material + Age + (1 | listener)
# model4: uRevs ~ CondCode + material + Age + (1 | listener) + CondCode:Age
#        npar     AIC     BIC logLik deviance  Chisq Df Pr(>Chisq)   
# model5    9 -403.73 -373.02 210.87  -421.73                        
# model4   13 -409.89 -365.54 217.94  -435.89 14.161  4     0.0068 **

# best mode --> model 2: uRevs ~ CondCode + material + Age + 
#                                CondCode:material + CondCode:Age + material:Age + (1 | listener) 
tab_model(model2)
BestModel <- model2
# report(model2)

# Does 'APDsibling' as a random intercept helps model prediction? -> nope..
# model_a <- lmer(uRevs~CondCode+material+Age+(1|listener)+(1|APDsibling), ST_trimmed, REML=FALSE)
# model_b <- lmer(uRevs~CondCode+material+Age+(1|listener), ST_trimmed, REML=FALSE)
# anova(model_a,model_b)

# ---------------------------------------------------------------------------------
# test main effects:
rstatix::Anova(model2,type="II",test.statistic="Chisq")

# CondCode:Age
(BestModel.1 <- update(BestModel,  . ~ . -CondCode:Age))
CondAge.LMEM <- anova(BestModel,BestModel.1)
summary(BestModel.1)
# BestModel.1: uRevs ~ CondCode + material + Age + (1 | listener) + CondCode:material + 
# BestModel.1:     material:Age
# BestModel: uRevs ~ CondCode + material + Age + (1 | listener) + CondCode:material + 
# BestModel:     CondCode:Age + material:Age
#             npar     AIC     BIC logLik deviance  Chisq Df Pr(>Chisq)   
# BestModel.1   14 -405.83 -358.06 216.91  -433.83                        
# BestModel     18 -412.75 -351.34 224.37  -448.75 14.919  4   0.004871 **

# material:Age
(BestModel.2 <- update(BestModel,  . ~ . -material:Age))
MatAge.LMEM <- anova(BestModel,BestModel.2)
summary(BestModel.2)
# BestModel.2: uRevs ~ CondCode + material + Age + (1 | listener) + CondCode:material + 
# BestModel.2:     CondCode:Age
# BestModel: uRevs ~ CondCode + material + Age + (1 | listener) + CondCode:material + 
# BestModel:     CondCode:Age + material:Age
#             npar     AIC     BIC logLik deviance  Chisq Df Pr(>Chisq)  
# BestModel.2   17 -410.96 -352.96 222.48  -444.96                       
# BestModel     18 -412.75 -351.34 224.37  -448.75 3.7857  1    0.05169 .

# CondCode:material
(BestModel.3 <- update(BestModel,  . ~ . - CondCode:material))
CondMat.LMEM <- anova(BestModel,BestModel.3)
summary(BestModel.3)
# BestModel.3: uRevs ~ CondCode + material + Age + (1 | listener) + CondCode:Age + 
# BestModel.3:     material:Age
# BestModel: uRevs ~ CondCode + material + Age + (1 | listener) + CondCode:material + 
# BestModel:     CondCode:Age + material:Age
#             npar     AIC     BIC logLik deviance  Chisq Df Pr(>Chisq)  
# BestModel.3   14 -411.36 -363.60 219.68  -439.36                       
# BestModel     18 -412.75 -351.34 224.37  -448.75 9.3848  4    0.05217 .


# ------------------------------------------------------------------------------
# Post hoc -> individual lm's by condition.
# Since there is no material x condition interaction, observations from 
# the two speech materials were combined.
# --> use nls model used to for z-scores. This should be better method, especially for MDR_F-ASL 
# data where segmented line is needed.

# # get post-hoc test for single regression models (shown in the figure)
# anova.ST2 <- data.table::rbindlist(anova.ST) %>% data.frame(.) %>% round(.,3)
# colnames(anova.ST2)[2:4] <- c("SE","t-value","p-value")
# anova.ST2$`p-value` = ifelse(anova.ST2$`p-value`<.05,sprintf("\\textbf{%0.3f}",anova.ST2$`p-value`),anova.ST2$`p-value`)
# anova.ST2 <- cbind("Condition"=c("Quiet-NoAlt","Quiet-Alt","AMSSN-Alt","MDR\\_F-Alt","ENG\\_F-Alt","CCRM\\_F"),
#               rbind(anova.ST2[1:5,],c("","","","")),
#               anova.ST2[6:11,])


# -----------------------------------------------------------------------------------------------------
# Post-hoc paired comparison
# -----------------------------------------------------------------------------------------------------
# CondCode x Age:
# (ref1 <- emmeans::lsmeans(BestModel,~ CondCode*Age))
# comps <- emmeans::contrast(ref1,alpha=0.05,method="pairwise",adjust=NULL) #,adjust=NULL) adjust="bonferroni"
# summary(comps)

# Material x Age:
(ref1 <- emmeans::lsmeans(BestModel,~ material*Age))
comps <- emmeans::contrast(ref1,alpha=0.05,method="pairwise",adjust=NULL) #,adjust=NULL) adjust="bonferroni"
summary(comps)

# MatAge.postLMEM <- data.frame(comps[c(1)])
# MatAge.CI <- confint(comps[c(1)])
# MatAge.CI <- MatAge.CI  %>% mutate( CI = sprintf("%s - %s",
#           round(lower.CL,2),
#           round(upper.CL,2)))
# MatAge.postLMEM <- cbind(MatAge.postLMEM,"CI"=MatAge.CI$CI)

# Condition x Material:
# visual inspection
# emmeans::emmip(model2, CondCode ~ material)
(ref1 <- emmeans::lsmeans(BestModel,~ CondCode*material))
comps <- emmeans::contrast(ref1,alpha=0.05,method="pairwise",adjust=NULL) #,adjust=NULL) adjust="bonferroni"
summary(comps)

CondMat.postLMEM <- data.frame(comps[c(5,14,22,29,35)])
CondMat.CI <- confint(comps[c(5,14,22,29,35)])

CondMat.CI <- CondMat.CI  %>% mutate( CI = sprintf("%s - %s",
                                                   round(lower.CL,2),
                                                   round(upper.CL,2)))

CondMat.postLMEM <- cbind(CondMat.postLMEM,"CI"=CondMat.CI$CI)
postLMEM <- CondMat.postLMEM
# postLMEM <- rbind(CondMat.postLMEM,MatAge.postLMEM)

colnames(postLMEM)[1:7] <- c("ASL - CCRM","Estimate","SE","Df","t-value","p-value",sprintf("95\\%%-CI"))
row.names(postLMEM) <- NULL

# change contrasts names
postLMEM[1] <-  c("Quiet-NoAlt","Quiet-Alt","AMSSN-Alt", sprintf("MDR\\_F-Alt"),sprintf("ENG\\_F-Alt"))
# sprintf("(ENG\\_F-Alt ASL) - (ENG\\_F-Alt CCRM)"), "ASL - CCRM")

# contrast                            estimate     SE  df t.ratio p.value
# (Q-NoAlt ASLN) - (Q-NoAlt CCRM)      0.19389 0.0254 216   7.621 <.0001 
# (Q-Alt ASLN) - (Q-Alt CCRM)          0.20112 0.0251 216   8.012 <.0001
# (AMSSN-Alt ASLN) - (AMSSN-Alt CCRM)  0.18495 0.0251 216   7.366 <.0001 
# (MDR_F-Alt ASLN) - (MDR_F-Alt CCRM)  0.24260 0.0251 216   9.662 <.0001 
# (ENG_F-Alt ASLN) - (ENG_F-Alt CCRM)  0.27378 0.0251 216  10.904 <.0001 

# -------------------------------------------------------------------------
# get table
# -------------------------------------------------------------------------
ST_AgeTab <- data.frame(rbind(
  c("Condition:Material",CondMat.LMEM$Df[2],CondMat.LMEM$Chisq[2],sprintf("%0.3f",CondMat.LMEM$`Pr(>Chisq)`[2])),
  c("Condition:Age",CondAge.LMEM$Df[2],CondAge.LMEM$Chisq[2],sprintf("\\textbf{%0.3f}",CondAge.LMEM$`Pr(>Chisq)`[2])),
  c("Material:Age",MatAge.LMEM$Df[2],MatAge.LMEM$Chisq[2],sprintf("%0.3f",MatAge.LMEM$`Pr(>Chisq)`[2]))))

ST_AgeTab[,c(2:3)]= apply(ST_AgeTab[,c(2:3)], 2, function(x) as.numeric(as.character(x)))

colnames(ST_AgeTab)[1:4] <- c("Main effects","Df",sprintf("$\\chi^{2}$"),"p")
# -------------------------------------------------------------------------

Final_ST.Tab1 <- ST_AgeTab
```

```{r,label='ST-AgeEffect2', echo=FALSE,results='hide',message=FALSE,warning=FALSE, include=FALSE,eval=FALSE}
##### Age effect? ################################################################# 

# CCRM data only
CCRM <- data.frame(CCRM)

######## lmer model ##########################################
# !!! Model with +(1|APDsibling) as random intercept resulted in singular fit!

## Find Best fit 
# start with a saturated model (3-way interaction with age)
model1 <- lmer(uRevs~CondCode*Age+(1|listener), CCRM, REML=FALSE)
summary(model1)

(model2<-update(model1,  ~ . - CondCode:Age))
anova(model1,model2)
summary(model2)
# model2: uRevs ~ CondCode + Age + (1 | listener)
# model1: uRevs ~ CondCode * Age + (1 | listener)
#        npar     AIC     BIC logLik deviance Chisq Df Pr(>Chisq)  
# model2    9 -320.61 -288.64 169.31  -338.61                      
# model1   14 -323.82 -274.08 175.91  -351.82 13.21  5    0.02149 *

tab_model(model1)

# Does 'APDsibling' as a random intercept helps model prediction? -> nope..
# model_a <- lmer(uRevs~CondCode+material+Age+(1|listener)+(1|APDsibling), ST_trimmed, REML=FALSE)
# model_b <- lmer(uRevs~CondCode+material+Age+(1|listener), ST_trimmed, REML=FALSE)
# anova(model_a,model_b)

# ---------------------------------------------------------------------------------
# test main effects:
rstatix::Anova(model1,type="II",test.statistic="Chisq")

BestModel <- model1
# report(model2)

# CondCode:Age
(BestModel.1 <- update(BestModel,  . ~ . -CondCode:Age))
CondAge.LMEM <- anova(BestModel,BestModel.1)
# summary(BestModel.1)
# BestModel.1: uRevs ~ CondCode + Age + (1 | listener)
# BestModel: uRevs ~ CondCode * Age + (1 | listener)
#             npar     AIC     BIC logLik deviance Chisq Df Pr(>Chisq)  
# BestModel.1    9 -320.61 -288.64 169.31  -338.61                      
# BestModel     14 -323.82 -274.08 175.91  -351.82 13.21  5    0.02149 *

# ------------------------------------------------------------------------------
# Post hoc -> individual lm's by condition.
# Since there is no material x condition interaction, observations from 
# the two speech materials were combined.
# --> use nls model used to for z-scores. This should be better method, especially for MDR_F-ASL 
# data where segmented line is needed.

# # get post-hoc test for single regression models (shown in the figure)
# anova.ST2 <- data.table::rbindlist(anova.ST) %>% data.frame(.) %>% round(.,3)
# colnames(anova.ST2)[2:4] <- c("SE","t-value","p-value")
# anova.ST2$`p-value` = ifelse(anova.ST2$`p-value`<.05,sprintf("\\textbf{%0.3f}",anova.ST2$`p-value`),anova.ST2$`p-value`)
# anova.ST2 <- cbind("Condition"=c("Quiet-NoAlt","Quiet-Alt","AMSSN-Alt","MDR\\_F-Alt","ENG\\_F-Alt","CCRM\\_F"),
#               rbind(anova.ST2[1:5,],c("","","","")),
#               anova.ST2[6:11,])


# -----------------------------------------------------------------------------------------------------
# Post-hoc paired comparison
# -----------------------------------------------------------------------------------------------------
# CondCode x Age:
# (ref1 <- emmeans::lsmeans(BestModel,~ CondCode*Age))
# comps <- emmeans::contrast(ref1,alpha=0.05,method="pairwise",adjust=NULL) #,adjust=NULL) adjust="bonferroni"
# summary(comps)

# Material x Age:
(ref1 <- emmeans::lsmeans(BestModel,~ CondCode*Age))
comps <- emmeans::contrast(ref1,alpha=0.05,method="pairwise",adjust=NULL) #,adjust=NULL) adjust="bonferroni"
summary(comps)

# MatAge.postLMEM <- data.frame(comps[c(1)])
# MatAge.CI <- confint(comps[c(1)])
# MatAge.CI <- MatAge.CI  %>% mutate( CI = sprintf("%s - %s",
#           round(lower.CL,2),
#           round(upper.CL,2)))
# MatAge.postLMEM <- cbind(MatAge.postLMEM,"CI"=MatAge.CI$CI)

# Condition x Material:
# visual inspection
# emmeans::emmip(model2, CondCode ~ material)
# (ref1 <- emmeans::lsmeans(BestModel,~ CondCode*material))
# comps <- emmeans::contrast(ref1,alpha=0.05,method="pairwise",adjust=NULL) #,adjust=NULL) adjust="bonferroni"
# summary(comps)
# 
# CondMat.postLMEM <- data.frame(comps[c(5,14,22,29,35)])
# CondMat.CI <- confint(comps[c(5,14,22,29,35)])
# 
# CondMat.CI <- CondMat.CI  %>% mutate( CI = sprintf("%s - %s",
#           round(lower.CL,2),
#           round(upper.CL,2)))
# 
# CondMat.postLMEM <- cbind(CondMat.postLMEM,"CI"=CondMat.CI$CI)
# postLMEM <- CondMat.postLMEM
# # postLMEM <- rbind(CondMat.postLMEM,MatAge.postLMEM)
# 
# colnames(postLMEM)[1:7] <- c("Contrasts","Estimate","SE","Df","t-value","p-value",sprintf("95\\%%-CI"))
# row.names(postLMEM) <- NULL
# 
# # change contrasts names
# postLMEM$Contrasts <-  c("(Quiet-NoAlt ASL) - (Quiet-NoAlt CCRM)", 
#                          "(Quiet-Alt ASL) - (Quiet-Alt CCRM)",
#                          "(AMSSN-Alt ASL) - (AMSSN-Alt CCRM)", 
#                          sprintf("(MDR\\_F-Alt ASL) - (MDR\\_F-Alt CCRM)"),
#                         sprintf("(ENG\\_F-Alt ASL) - (ENG\\_F-Alt CCRM)"))
# sprintf("(ENG\\_F-Alt ASL) - (ENG\\_F-Alt CCRM)"), "ASL - CCRM")

# contrast                            estimate     SE  df t.ratio p.value
# (Q-NoAlt ASLN) - (Q-NoAlt CCRM)      0.19389 0.0254 216   7.621 <.0001 
# (Q-Alt ASLN) - (Q-Alt CCRM)          0.20112 0.0251 216   8.012 <.0001
# (AMSSN-Alt ASLN) - (AMSSN-Alt CCRM)  0.18495 0.0251 216   7.366 <.0001 
# (MDR_F-Alt ASLN) - (MDR_F-Alt CCRM)  0.24260 0.0251 216   9.662 <.0001 
# (ENG_F-Alt ASLN) - (ENG_F-Alt CCRM)  0.27378 0.0251 216  10.904 <.0001 

# -------------------------------------------------------------------------
# get table
# -------------------------------------------------------------------------
# ST_AgeTab <- data.frame(rbind(
#    c("Condition:Material",CondMat.LMEM$Df[2],CondMat.LMEM$Chisq[2],sprintf("\\textbf{%0.3f}",CondMat.LMEM$`Pr(>Chisq)`[2])),
#   c("Condition:Age",CondAge.LMEM$Df[2],CondAge.LMEM$Chisq[2],sprintf("\\textbf{%0.3f}",CondAge.LMEM$`Pr(>Chisq)`[2])),
#   c("Material:Age",MatAge.LMEM$Df[2],MatAge.LMEM$Chisq[2],sprintf("\\textbf{%0.3f}",MatAge.LMEM$`Pr(>Chisq)`[2]))))
# 
# ST_AgeTab[,c(2:3)]= apply(ST_AgeTab[,c(2:3)], 2, function(x) as.numeric(as.character(x)))
# 
# colnames(ST_AgeTab)[1:4] <- c("Main effects","Df",sprintf("$\\chi^{2}$"),"p")
# # -------------------------------------------------------------------------
```