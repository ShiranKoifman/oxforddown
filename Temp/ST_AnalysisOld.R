```{r,label='ST-AgeEffectAPDsibASL', echo=FALSE,results='hide',message=FALSE,warning=FALSE, include=FALSE,eval=FALSE}
##### Age effect? ################################################################# 

### ASL ONLY

# Normality: ~ met (7/10 pairs are normally distr.)
# Homogeneity: met  

ST_trimmed_ASL <- subset(ST_trimmed,material=="ASLN") %>% droplevels()

######## lmer model ##########################################

## Find Best fit 
# start with a saturated model (3-way interaction with age)

model1 <- lmer(uRevs~CondCode*Age*APDsibling+(1|listener), ST_trimmed_ASL, REML=FALSE)
summary(model1)

(model2<-update(model1,  ~ . - CondCode:Age:APDsibling))
anova(model1,model2)
summary(model2)
# model2: uRevs ~ CondCode + Age + APDsibling + (1 | listener) + CondCode:Age + 
# model2:     CondCode:APDsibling + Age:APDsibling
# model1: uRevs ~ CondCode * Age * APDsibling + (1 | listener)
#        npar     AIC     BIC logLik deviance  Chisq Df Pr(>Chisq)
# model2   18 -191.28 -141.87 113.64  -227.28                     
# model1   22 -186.47 -126.08 115.24  -230.47 3.1919  4     0.5262

(model3<-update(model2,  ~ . - Age:APDsibling))
anova(model2,model3)
summary(model3)
# model3: uRevs ~ CondCode + Age + APDsibling + (1 | listener) + CondCode:Age + 
# model3:     CondCode:APDsibling
# model2: uRevs ~ CondCode + Age + APDsibling + (1 | listener) + CondCode:Age + 
# model2:     CondCode:APDsibling + Age:APDsibling
#        npar     AIC     BIC logLik deviance  Chisq Df Pr(>Chisq)
# model3   17 -192.44 -145.78 113.22  -226.44                     
# model2   18 -191.28 -141.87 113.64  -227.28 0.8387  1     0.3598

(model4<-update(model3,  ~ . -  CondCode:APDsibling))
anova(model3,model4)
summary(model4)
# model4: uRevs ~ CondCode + Age + APDsibling + (1 | listener) + CondCode:Age
# model3: uRevs ~ CondCode + Age + APDsibling + (1 | listener) + CondCode:Age + 
# model3:     CondCode:APDsibling
#        npar     AIC     BIC logLik deviance  Chisq Df Pr(>Chisq)
# model4   13 -198.83 -163.15 112.41  -224.83                     
# model3   17 -192.44 -145.78 113.22  -226.44 1.6123  4     0.8066

(model5<-update(model4,  ~ . - CondCode:Age))
anova(model4,model5)
summary(model5)
# model5: uRevs ~ CondCode + Age + APDsibling + (1 | listener)
# model4: uRevs ~ CondCode + Age + APDsibling + (1 | listener) + CondCode:Age
#        npar     AIC     BIC logLik deviance  Chisq Df Pr(>Chisq)
# model5    9 -199.83 -175.12 108.92  -217.83                     
# model4   13 -198.83 -163.15 112.41  -224.83 6.9995  4     0.1359

(model6<-update(model5,  ~ . - APDsibling))
anova(model5,model6)
summary(model6)
# model6: uRevs ~ CondCode + Age + (1 | listener)
# model5: uRevs ~ CondCode + Age + APDsibling + (1 | listener)
#        npar     AIC     BIC logLik deviance  Chisq Df Pr(>Chisq)
# model6    8 -201.79 -179.83 108.89  -217.79                     
# model5    9 -199.83 -175.12 108.92  -217.83 0.0391  1     0.8433

(model7<-update(model6,  ~ . - Age))
anova(model6,model7)
summary(model7)
# model7: uRevs ~ CondCode + (1 | listener)
# model6: uRevs ~ CondCode + Age + (1 | listener)
#        npar     AIC     BIC logLik deviance  Chisq Df Pr(>Chisq)   
# model7    7 -192.97 -173.75 103.48  -206.97                        
# model6    8 -201.79 -179.83 108.89  -217.79 10.824  1   0.001002 **

# best model --> model 6: uRevs ~ CondCode + Age + (1 | listener)
BestModel <- model6
tab_model(BestModel)
# visreg::visreg(model6)
# report::report(model7)
# ---------------------------------------------------------------------------------
# test main effects:
rstatix::Anova(BestModel,type="II",test.statistic="Chisq")


# Age
(BestModel.1 <- update(BestModel,  . ~ . -Age))
Age.LMEM <- anova(BestModel,BestModel.1)
summary(BestModel.1)
# BestModel.1: uRevs ~ CondCode + (1 | listener)
# BestModel: uRevs ~ CondCode + Age + (1 | listener)
#             npar     AIC     BIC logLik deviance  Chisq Df Pr(>Chisq)   
# BestModel.1    7 -192.97 -173.75 103.48  -206.97                        
# BestModel      8 -201.79 -179.83 108.89  -217.79 10.824  1   0.001002 **

# CondCode
(BestModel.2 <- update(BestModel,  . ~ . -CondCode))
Cond.LMEM <- anova(BestModel,BestModel.2)
summary(BestModel.2)
# BestModel.2: uRevs ~ Age + (1 | listener)
# BestModel: uRevs ~ CondCode + Age + (1 | listener)
#             npar      AIC      BIC  logLik deviance  Chisq Df Pr(>Chisq)    
# BestModel.2    4  -55.265  -44.286  31.633  -63.265                         
# BestModel      8 -201.790 -179.831 108.895 -217.790 154.52  4  < 2.2e-16 ***

# -------------------------------------------------------------------------
# get table
# -------------------------------------------------------------------------
ST_AgeTab_ASL <- data.frame(rbind(
  c("Age",Age.LMEM$Df[2],Age.LMEM$Chisq[2],sprintf("\\textbf{%0.3f}",Age.LMEM$`Pr(>Chisq)`[2])),
  c("Condition",Cond.LMEM$Df[2],Cond.LMEM$Chisq[2],sprintf("\\textbf{%0.3f}",Cond.LMEM$`Pr(>Chisq)`[2]))
))

ST_AgeTab_ASL[,c(2:3)]= apply(ST_AgeTab_ASL[,c(2:3)], 2, function(x) as.numeric(as.character(x)))

colnames(ST_AgeTab_ASL)[1:4] <- c("Main effects","Df",sprintf("$\\chi^{2}$"),"p")
# -------------------------------------------------------------------------

################################ Assumptions testing ################################ 

# Normality: ~ met (7/10 pairs are normally distr.)
# Homogeneity: met 

# linear model
w1 <- lm(uRevs~ CondCode + Group2, data = ST_trimmed_ASL)

# 1. Normality (Shapiro-Wilk test) --> NOT met!
# data is normally distributed if p >.05

# QQ plot of residuals
qqPlot(residuals(w1))

#jpeg('Q-Q Plot.png', width = 10, height = 6, units = 'in', res = 300)
par(mfrow=c(1,2))    # set the plotting area into a 1*2 array
qqnorm(ST_trimmed_ASL$uRevs, pch = 1, frame = FALSE,main = "SRdT - Normal Q-Q Plot")
qqline(ST_trimmed_ASL$uRevs, col = "red", lwd = 2)
qqnorm(rstandard(w1), pch = 1, frame = FALSE,main = "Residuals - Normal Q-Q Plot")
qqline(rstandard(w1), col = "red", lwd = 2)
#dev.off()

# Option 1:
shapiro.test(residuals(w1))

# Option 2 by conditions:
NormTest <- ST_trimmed_ASL %>%
  group_by(CondCode, Group2) %>%
  rstatix::shapiro_test(uRevs)

# 2. Homoggeneity of variance (Levene's test) --> NOT met!
# homogeneity is met if p>.05

# Option 1:
car::leveneTest(uRevs ~ CondCode * Group2, data=ST_trimmed_ASL,center=median)

# Option 2: 
DescTools::LeveneTest(lm(uRevs~ CondCode * Group2, data = ST_trimmed_ASL))

# Option 3:
# per condition
# VarTest <- ASLN %>%
#   rstatix::group_by(Group) %>%
#   levene_test(uRevs ~ CondCode)

```

```{r,label='ST-AgeEffectAPDsibCCRM', echo=FALSE,results='hide',message=FALSE,warning=FALSE, include=FALSE,eval=FALSE}
##### Age effect? ################################################################# 

### CCRM ONLY

# Normality: met
# Homogeneity: NOT met (p=0.02182)

ST_trimmed_CCRM <- subset(ST_trimmed,material=="CCRM") %>% droplevels()

######## lmer model ##########################################

## Find Best fit 
# start with a saturated model (3-way interaction with age)

model1 <- lmer(uRevs~CondCode*Age*APDsibling+(1|listener), ST_trimmed_CCRM, REML=FALSE)
summary(model1)

(model2<-update(model1,  ~ . - CondCode:Age:APDsibling))
anova(model1,model2)
summary(model2)
# model2: uRevs ~ CondCode + Age + APDsibling + (1 | listener) + CondCode:Age + 
# model2:     CondCode:APDsibling + Age:APDsibling
# model1: uRevs ~ CondCode * Age * APDsibling + (1 | listener)
#        npar     AIC     BIC logLik deviance  Chisq Df Pr(>Chisq)
# model2   18 -190.01 -141.56 113.00  -226.01                     
# model1   22 -183.49 -124.28 113.74  -227.49 1.4776  4     0.8306

(model3<-update(model2,  ~ . - Age:APDsibling))
anova(model2,model3)
summary(model3)
# model3: uRevs ~ CondCode + Age + APDsibling + (1 | listener) + CondCode:Age + 
# model3:     CondCode:APDsibling
# model2: uRevs ~ CondCode + Age + APDsibling + (1 | listener) + CondCode:Age + 
# model2:     CondCode:APDsibling + Age:APDsibling
#        npar     AIC     BIC logLik deviance  Chisq Df Pr(>Chisq)
# model3   17 -191.97 -146.22 112.99  -225.97                     
# model2   18 -190.01 -141.56 113.00  -226.01 0.0339  1     0.8539

(model4<-update(model3,  ~ . -  CondCode:APDsibling))
anova(model3,model4)
summary(model4)
# model4: uRevs ~ CondCode + Age + APDsibling + (1 | listener) + CondCode:Age
# model3: uRevs ~ CondCode + Age + APDsibling + (1 | listener) + CondCode:Age + 
# model3:     CondCode:APDsibling
#        npar     AIC     BIC logLik deviance  Chisq Df Pr(>Chisq)
# model4   13 -196.82 -161.84 111.41  -222.82                     
# model3   17 -191.97 -146.22 112.99  -225.97 3.1485  4     0.5333

(model5<-update(model4,  ~ . - CondCode:Age))
anova(model4,model5)
summary(model5)
# model5: uRevs ~ CondCode + Age + APDsibling + (1 | listener)
# model4: uRevs ~ CondCode + Age + APDsibling + (1 | listener) + CondCode:Age
#        npar     AIC     BIC logLik deviance  Chisq Df Pr(>Chisq)   
# model5    9 -191.52 -167.29 104.76  -209.52                        
# model4   13 -196.82 -161.84 111.41  -222.82 13.308  4   0.009865 **


# best model --> model 4: uRevs ~ CondCode + Age + APDsibling + CondCode:Age + (1 | listener)
BestModel <- model4
tab_model(BestModel)
# visreg::visreg(model4)
# report::report(model4)

# ---------------------------------------------------------------------------------
# test main effects:
rstatix::Anova(BestModel,type="II",test.statistic="Chisq")

# CondCode x Age:
(BestModel.1 <- update(BestModel,  . ~ . -CondCode:Age))
CondAge.LMEM <- anova(BestModel,BestModel.1)
summary(BestModel.1)
# BestModel.1: uRevs ~ CondCode + Age + APDsibling + (1 | listener)
# BestModel: uRevs ~ CondCode + Age + APDsibling + (1 | listener) + CondCode:Age
#             npar     AIC     BIC logLik deviance  Chisq Df Pr(>Chisq)   
# BestModel.1    9 -191.52 -167.29 104.76  -209.52                        
# BestModel     13 -196.82 -161.84 111.41  -222.82 13.308  4   0.009865 **

# APDsibling
(BestModel.2 <- update(BestModel,  . ~ . -APDsibling))
APDsib.LMEM <- anova(BestModel,BestModel.2)
summary(BestModel.2)
# BestModel.2: uRevs ~ CondCode + Age + (1 | listener) + CondCode:Age
# BestModel: uRevs ~ CondCode + Age + APDsibling + (1 | listener) + CondCode:Age
#             npar     AIC     BIC logLik deviance  Chisq Df Pr(>Chisq)
# BestModel.2   12 -196.49 -164.20 110.25  -220.49                     
# BestModel     13 -196.82 -161.84 111.41  -222.82 2.3304  1     0.1269

# -------------------------------------------------------------------------
# get table
# -------------------------------------------------------------------------
ST_AgeTab_CCRM <- data.frame(rbind(
  c("APDsibling",APDsib.LMEM$Df[2],APDsib.LMEM$Chisq[2],sprintf("%0.3f",APDsib.LMEM$`Pr(>Chisq)`[2])),
  c("Condition:Age",CondAge.LMEM$Df[2],CondAge.LMEM$Chisq[2],sprintf("\\textbf{%0.3f}",CondAge.LMEM$`Pr(>Chisq)`[2]))
))

ST_AgeTab_CCRM[,c(2:3)]= apply(ST_AgeTab_CCRM[,c(2:3)], 2, function(x) as.numeric(as.character(x)))

colnames(ST_AgeTab_CCRM)[1:4] <- c("Main effects","Df",sprintf("$\\chi^{2}$"),"p")
# -------------------------------------------------------------------------


################################ Assumptions testing ################################ 

# Normality: met
# Homogeneity: NOT met (p=0.02182)

# linear model
w1 <- lm(uRevs~ CondCode + Group2, data = ST_trimmed_CCRM)

# 1. Normality (Shapiro-Wilk test) --> met
# data is normally distributed if p >.05

# QQ plot of residuals
qqPlot(residuals(w1))

#jpeg('Q-Q Plot.png', width = 10, height = 6, units = 'in', res = 300)
par(mfrow=c(1,2))    # set the plotting area into a 1*2 array
qqnorm(ST_trimmed_CCRM$uRevs, pch = 1, frame = FALSE,main = "SRdT - Normal Q-Q Plot")
qqline(ST_trimmed_CCRM$uRevs, col = "red", lwd = 2)
qqnorm(rstandard(w1), pch = 1, frame = FALSE,main = "Residuals - Normal Q-Q Plot")
qqline(rstandard(w1), col = "red", lwd = 2)
#dev.off()

# Option 1:
shapiro.test(residuals(w1))

# Option 2 by conditions:
NormTest <- ST_trimmed_CCRM %>%
  group_by(CondCode, Group2) %>%
  rstatix::shapiro_test(uRevs)

# 2. Homoggeneity of variance (Levene's test) --> NOT met!
# homogeneity is met if p>.05

# Option 1:
car::leveneTest(uRevs ~ CondCode * Group2, data=ST_trimmed_CCRM,center=median)

# Option 2: 
DescTools::LeveneTest(lm(uRevs~ CondCode * Group2, data = ST_trimmed_CCRM))

# Option 3:
# per condition
# VarTest <- CCRM %>%
#   rstatix::group_by(Group) %>%
#   levene_test(uRevs ~ CondCode)

```

<!-- DELETE? -->
  ```{r,label='ST-zLMEM', echo=FALSE,results='hide',message=FALSE,warning=FALSE, include=FALSE, eval=FALSE}
##### Age effect? ################################################################# 

# Try robust LMEM: robustlmm::rlmer()
# model1 <- robustlmm::rlmer(z_trim~material*CondCode*Group2+(1|listener), ST, REML=FALSE)
# summary(model1)
# 
# (model2<-update(model1,  ~ . - material:CondCode:Group2))
# robustlmm::compare(model1,model2, show.rho.functions = FALSE)
# summary(model2)

######## lmer model ##########################################

## Find Best fit 
# start with a saturated model (3-way interaction with age)

model1 <- lmer(z_trim~material*CondCode*Group2+(1|listener), ST, REML=FALSE)
summary(model1)

(model2<-update(model1,  ~ . - material:CondCode:Group2))
anova(model1,model2)
summary(model2)
# Models:
# model2: z_trim ~ material + CondCode + Group2 + (1 | listener) + material:CondCode + 
# model2:     material:Group2 + CondCode:Group2
# model1: z_trim ~ material * CondCode * Group2 + (1 | listener)
#        npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
# model2   24 1517.3 1614.8 -734.63   1469.3                     
# model1   32 1529.5 1659.6 -732.76   1465.5 3.7275  8     0.8808

(model3<-update(model2,  ~ . - CondCode:Group2))
anova(model2,model3)
summary(model3)
# model3: z_trim ~ material + CondCode + Group2 + (1 | listener) + material:CondCode + 
# model3:     material:Group2
# model2: z_trim ~ material + CondCode + Group2 + (1 | listener) + material:CondCode + 
# model2:     material:Group2 + CondCode:Group2
#        npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
# model3   16 1511.2 1576.2 -739.59   1479.2                     
# model2   24 1517.3 1614.8 -734.63   1469.3 9.9278  8     0.2701

(model4<-update(model3,  ~ . -  material:Group2))
anova(model3,model4)
summary(model4)
# model4: z_trim ~ material + CondCode + Group2 + (1 | listener) + material:CondCode
# model3: z_trim ~ material + CondCode + Group2 + (1 | listener) + material:CondCode + 
# model3:     material:Group2
#        npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)  
# model4   14 1512.1 1569.0 -742.04   1484.1                       
# model3   16 1511.2 1576.2 -739.59   1479.2 4.8901  2    0.08672 .

(model5<-update(model4,  ~ . - material:CondCode))
anova(model4,model5)
summary(model5)
# model5: z_trim ~ material + CondCode + Group2 + (1 | listener)
# model4: z_trim ~ material + CondCode + Group2 + (1 | listener) + material:CondCode
#        npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
# model5   10 1510.5 1551.1 -745.25   1490.5                     
# model4   14 1512.1 1569.0 -742.04   1484.1 6.4256  4     0.1695

(model6<-update(model5,  ~ . - Group2))
anova(model5,model6)
summary(model6)
# model6: z_trim ~ material + CondCode + (1 | listener)
# model5: z_trim ~ material + CondCode + Group2 + (1 | listener)
#        npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)   
# model6    8 1516.3 1548.8 -750.13   1500.3                        
# model5   10 1510.5 1551.1 -745.25   1490.5 9.7574  2   0.007607 **


# best mode --> model5: z_trim ~ material + CondCode + Group2 + (1 | listener)

tab_model(model5)
# visreg::visreg(model7)
# visreg::visreg(model7,"APDsibling","CondCode")
# visreg::visreg(model5,"Group2")

BestModel <- model5
# report::report(model5)

# ---------------------------------------------------------------------------------
# test main effects:
rstatix::Anova(BestModel,type="II",test.statistic="Chisq")

# Group2
(BestModel.1 <- update(BestModel,  . ~ . -Group2))
Group2.LMEM <- anova(BestModel,BestModel.1)
summary(BestModel.1)
# BestModel.1: uRevs ~ CondCode + material + Age + APDsibling + (1 | listener) + 
# BestModel.1:     CondCode:material + CondCode:Age + material:Age + CondCode:APDsibling
# BestModel: uRevs ~ CondCode + material + Age + APDsibling + (1 | listener) + 
# BestModel:     CondCode:material + CondCode:Age + material:Age + CondCode:APDsibling + 
# BestModel:     material:APDsibling
#             npar     AIC     BIC logLik deviance  Chisq Df Pr(>Chisq)  
# BestModel.1   23 -405.86 -327.39 225.93  -451.86                       
# BestModel     24 -408.79 -326.91 228.40  -456.79 4.9273  1    0.02643 *

# CondCode
(BestModel.2 <- update(BestModel,  . ~ . -CondCode))
Cond.LMEM <- anova(BestModel,BestModel.2)
summary(BestModel.2)
# BestModel.1: uRevs ~ CondCode + material + Age + APDsibling + (1 | listener) + 
# BestModel.1:     CondCode:material + CondCode:Age + material:Age + material:APDsibling
# BestModel: uRevs ~ CondCode + material + Age + APDsibling + (1 | listener) + 
# BestModel:     CondCode:material + CondCode:Age + material:Age + CondCode:APDsibling + 
# BestModel:     material:APDsibling
#             npar     AIC     BIC logLik deviance Chisq Df Pr(>Chisq)
# BestModel.1   20 -414.07 -345.83 227.03  -454.07                    
# BestModel     24 -408.79 -326.91 228.40  -456.79 2.724  4      0.605

# material
(BestModel.3 <- update(BestModel,  . ~ . -material))
Mat.LMEM <- anova(BestModel,BestModel.3)
summary(BestModel.3)
# BestModel.3: uRevs ~ CondCode + material + Age + APDsibling + (1 | listener) + 
# BestModel.3:     CondCode:material + CondCode:Age + CondCode:APDsibling + 
# BestModel.3:     material:APDsibling
# BestModel: uRevs ~ CondCode + material + Age + APDsibling + (1 | listener) + 
# BestModel:     CondCode:material + CondCode:Age + material:Age + CondCode:APDsibling + 
# BestModel:     material:APDsibling
#             npar     AIC     BIC logLik deviance  Chisq Df Pr(>Chisq)
# BestModel.3   23 -408.72 -330.25 227.36  -454.72                     
# BestModel     24 -408.79 -326.91 228.40  -456.79 2.0726  1       0.15

# -----------------------------------------------------------------------------------------------------
# Post-hoc paired comparison
# -----------------------------------------------------------------------------------------------------

# CondCode
# (ref1 <- emmeans::lsmeans(BestModel,~ CondCode))
# comps <- emmeans::contrast(ref1,alpha=0.05,method="pairwise",adjust=NULL) #,adjust=NULL) adjust="bonferroni"
# summary(comps)

# Group2:
(ref1 <- emmeans::lsmeans(BestModel,~ Group2))
comps <- emmeans::contrast(ref1,alpha=0.05,method="pairwise",adjust=NULL) #,adjust=NULL) adjust="bonferroni"
summary(comps)

Group2.postLMEM <- data.frame(comps)
Group2.CI <- confint(comps)
Group2.CI <- Group2.CI  %>% mutate( CI = sprintf("%s - %s",
                                                 round(lower.CL,2),
                                                 round(upper.CL,2)))
Group2.postLMEM <- cbind(Group2.postLMEM,"CI"=Group2.CI$CI)

postLMEM <- Group2.postLMEM
# postLMEM <- rbind(CondMat.postLMEM,MatAge.postLMEM)

colnames(postLMEM)[1:7] <- c("contrast","Estimate","SE","Df","t-value","p-value",sprintf("95\\%%-CI"))
row.names(postLMEM) <- NULL

# change contrasts names
postLMEM[1] <-  c("APD - TD","APD - TD\\_{APDsibling}","TD - TD\\_{APDsibling}")

postLMEM$`p-value` = ifelse(postLMEM$`p-value`<.05,sprintf("\\textbf{%0.3f}",postLMEM$`p-value`),sprintf("%0.3f",postLMEM$`p-value`))

# -------------------------------------------------------------------------
# get table
# -------------------------------------------------------------------------
ST_zTab <- data.frame(rbind(
  c("Group",Group2.LMEM$Df[2],Group2.LMEM$Chisq[2],sprintf("\\textbf{%0.3f}",Group2.LMEM$`Pr(>Chisq)`[2])),
  c("Material",Mat.LMEM$Df[2],Mat.LMEM$Chisq[2],sprintf("%0.3f",Mat.LMEM$`Pr(>Chisq)`[2])),
  c("Condition",Cond.LMEM$Df[2],Cond.LMEM$Chisq[2],"\\textbf{< 0.0001}")
  # c("Condition",Cond.LMEM$Df[2],Cond.LMEM$Chisq[2],sprintf("\\textbf{%0.3f}",Cond.LMEM$`Pr(>Chisq)`[2]))
))

ST_zTab[,c(2:3)]= apply(ST_zTab[,c(2:3)], 2, function(x) as.numeric(as.character(x)))

colnames(ST_zTab)[1:4] <- c("Main effects","Df",sprintf("$\\chi^{2}$"),"p")
# -------------------------------------------------------------------------


########################### Test assumptions #########################################

# For ST combined data (z-scores) ---------------------------------------------------

# Normality: shapiro is sig, but only 7/30 pairs are not normally distr..
# homoscedasticity: met

# linear model:
w1 <- lm(z_trim ~ material * CondCode * Group2, data = ST)

#### normality (shapiro-wilk) ###########
# Do all the points fall approximately along the reference line? If YES we can assume normality.
# ggqqplot(residuals(w1))

# Option 1:
# linearity is met if p>.05
shapiro.test(residuals(w1))
# W = 0.94461, p-value = 1.375e-11 --> NOT normaly distr.

shapiro.test(residuals(BestModel))

# plot & test the residual
qqnorm(resid(w1))
qqline(resid(w1))

# Option 2: by fixed factors: --> mostly normal..
NormTest <- ST %>%
  group_by(material, CondCode, Group2) %>%
  rstatix::shapiro_test(z_trim)

#### Homogeneity of variance (Levene's test) ###########
# homogeneity is met if p>.05

# Option 1: Is the lm model fully covered here?
leveneTest(residuals(w1) ~ CondCode * material * Group2 , data = ST)

leveneTest(residuals(BestModel) ~ CondCode * material * Group2 , data = ST)
# -> homogeneity of the full model is met

```

<!-- An LMEM model was used to test the effect of Material (ASL / CCRM) and Condition (), and Group () as fixed factors and random intercepts for subjects on performance in the task with z-scores as a dependent variable (reference levels: Material = ASLN; Condition: Quiet-NoAlt; Group = APD). As before, the full model did not include data measured for the CCRM condition with CCRM-type sentences as distractor (CCRM_F). Parametric assumption inspection of normal distribution was rejected (p < 0.001), whereas the assumption of homogeneity of variance was met (p > 0.05). However, an inspection of the distribution by groups, material and condition revlead that in most cases (23/30), normallity could not be rejected.  -->
  
  <!-- DELETE? -->
  ```{r,label='ST-zLMEM2', echo=FALSE,results='hide',message=FALSE,warning=FALSE, include=FALSE, eval=FALSE}
##### Age effect? ################################################################# 

######## lmer model ##########################################

## Find Best fit 
# start with a saturated model (3-way interaction with age)

model1 <- robustlmm::rlmer(z_trim~material*CondCode*Group2+(1|listener), ST, REML=FALSE)
summary(model1)

(model2<-update(model1,  ~ . - material:CondCode:Group2))
robustlmm::compare(model1,model2, show.rho.functions = FALSE)
summary(model2)
# Models:
# model2: z_trim ~ material + CondCode + Group2 + (1 | listener) + material:CondCode + 
# model2:     material:Group2 + CondCode:Group2
# model1: z_trim ~ material * CondCode * Group2 + (1 | listener)
#        npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
# model2   24 1517.3 1614.8 -734.63   1469.3                     
# model1   32 1529.5 1659.6 -732.76   1465.5 3.7275  8     0.8808

(model3<-update(model2,  ~ . - CondCode:Group2))
anova(model2,model3)
summary(model3)
# model3: z_trim ~ material + CondCode + Group2 + (1 | listener) + material:CondCode + 
# model3:     material:Group2
# model2: z_trim ~ material + CondCode + Group2 + (1 | listener) + material:CondCode + 
# model2:     material:Group2 + CondCode:Group2
#        npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
# model3   16 1511.2 1576.2 -739.59   1479.2                     
# model2   24 1517.3 1614.8 -734.63   1469.3 9.9278  8     0.2701

(model4<-update(model3,  ~ . -  material:Group2))
anova(model3,model4)
summary(model4)
# model4: z_trim ~ material + CondCode + Group2 + (1 | listener) + material:CondCode
# model3: z_trim ~ material + CondCode + Group2 + (1 | listener) + material:CondCode + 
# model3:     material:Group2
#        npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)  
# model4   14 1512.1 1569.0 -742.04   1484.1                       
# model3   16 1511.2 1576.2 -739.59   1479.2 4.8901  2    0.08672 .

(model5<-update(model4,  ~ . - material:CondCode))
anova(model4,model5)
summary(model5)
# model5: z_trim ~ material + CondCode + Group2 + (1 | listener)
# model4: z_trim ~ material + CondCode + Group2 + (1 | listener) + material:CondCode
#        npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
# model5   10 1510.5 1551.1 -745.25   1490.5                     
# model4   14 1512.1 1569.0 -742.04   1484.1 6.4256  4     0.1695

(model6<-update(model5,  ~ . - Group2))
anova(model5,model6)
summary(model6)
# model6: z_trim ~ material + CondCode + (1 | listener)
# model5: z_trim ~ material + CondCode + Group2 + (1 | listener)
#        npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)   
# model6    8 1516.3 1548.8 -750.13   1500.3                        
# model5   10 1510.5 1551.1 -745.25   1490.5 9.7574  2   0.007607 **


# best mode --> model5: z_trim ~ material + CondCode + Group2 + (1 | listener)

tab_model(model5)
# visreg::visreg(model7)
# visreg::visreg(model7,"APDsibling","CondCode")
visreg::visreg(model5,"Group2")

BestModel <- model5
# report::report(model5)

# ---------------------------------------------------------------------------------
# test main effects:
rstatix::Anova(BestModel,type="II",test.statistic="Chisq")

# Group2
(BestModel.1 <- update(BestModel,  . ~ . -Group2))
Group2.LMEM <- anova(BestModel,BestModel.1)
summary(BestModel.1)
# BestModel.1: uRevs ~ CondCode + material + Age + APDsibling + (1 | listener) + 
# BestModel.1:     CondCode:material + CondCode:Age + material:Age + CondCode:APDsibling
# BestModel: uRevs ~ CondCode + material + Age + APDsibling + (1 | listener) + 
# BestModel:     CondCode:material + CondCode:Age + material:Age + CondCode:APDsibling + 
# BestModel:     material:APDsibling
#             npar     AIC     BIC logLik deviance  Chisq Df Pr(>Chisq)  
# BestModel.1   23 -405.86 -327.39 225.93  -451.86                       
# BestModel     24 -408.79 -326.91 228.40  -456.79 4.9273  1    0.02643 *

# CondCode
(BestModel.2 <- update(BestModel,  . ~ . -CondCode))
Cond.LMEM <- anova(BestModel,BestModel.2)
summary(BestModel.2)
# BestModel.1: uRevs ~ CondCode + material + Age + APDsibling + (1 | listener) + 
# BestModel.1:     CondCode:material + CondCode:Age + material:Age + material:APDsibling
# BestModel: uRevs ~ CondCode + material + Age + APDsibling + (1 | listener) + 
# BestModel:     CondCode:material + CondCode:Age + material:Age + CondCode:APDsibling + 
# BestModel:     material:APDsibling
#             npar     AIC     BIC logLik deviance Chisq Df Pr(>Chisq)
# BestModel.1   20 -414.07 -345.83 227.03  -454.07                    
# BestModel     24 -408.79 -326.91 228.40  -456.79 2.724  4      0.605

# material
(BestModel.3 <- update(BestModel,  . ~ . -material))
Mat.LMEM <- anova(BestModel,BestModel.3)
summary(BestModel.3)
# BestModel.3: uRevs ~ CondCode + material + Age + APDsibling + (1 | listener) + 
# BestModel.3:     CondCode:material + CondCode:Age + CondCode:APDsibling + 
# BestModel.3:     material:APDsibling
# BestModel: uRevs ~ CondCode + material + Age + APDsibling + (1 | listener) + 
# BestModel:     CondCode:material + CondCode:Age + material:Age + CondCode:APDsibling + 
# BestModel:     material:APDsibling
#             npar     AIC     BIC logLik deviance  Chisq Df Pr(>Chisq)
# BestModel.3   23 -408.72 -330.25 227.36  -454.72                     
# BestModel     24 -408.79 -326.91 228.40  -456.79 2.0726  1       0.15

# -----------------------------------------------------------------------------------------------------
# Post-hoc paired comparison
# -----------------------------------------------------------------------------------------------------

# CondCode
# (ref1 <- emmeans::lsmeans(BestModel,~ CondCode))
# comps <- emmeans::contrast(ref1,alpha=0.05,method="pairwise",adjust=NULL) #,adjust=NULL) adjust="bonferroni"
# summary(comps)

# Group2:
(ref1 <- emmeans::lsmeans(BestModel,~ Group2))
comps <- emmeans::contrast(ref1,alpha=0.05,method="pairwise",adjust=NULL) #,adjust=NULL) adjust="bonferroni"
summary(comps)

Group2.postLMEM <- data.frame(comps)
Group2.CI <- confint(comps)
Group2.CI <- Group2.CI  %>% mutate( CI = sprintf("%s - %s",
                                                 round(lower.CL,2),
                                                 round(upper.CL,2)))
Group2.postLMEM <- cbind(Group2.postLMEM,"CI"=Group2.CI$CI)

postLMEM <- Group2.postLMEM
# postLMEM <- rbind(CondMat.postLMEM,MatAge.postLMEM)

colnames(postLMEM)[1:7] <- c("contrast","Estimate","SE","Df","t-value","p-value",sprintf("95\\%%-CI"))
row.names(postLMEM) <- NULL

# change contrasts names
postLMEM[1] <-  c("APD - TD","APD - TD\\_{APDsibling}","TD - TD\\_{APDsibling}")

postLMEM$`p-value` = ifelse(postLMEM$`p-value`<.05,sprintf("\\textbf{%0.3f}",postLMEM$`p-value`),sprintf("%0.3f",postLMEM$`p-value`))

# -------------------------------------------------------------------------
# get table
# -------------------------------------------------------------------------
ST_zTab <- data.frame(rbind(
  c("Group",Group2.LMEM$Df[2],Group2.LMEM$Chisq[2],sprintf("\\textbf{%0.3f}",Group2.LMEM$`Pr(>Chisq)`[2])),
  c("Material",Mat.LMEM$Df[2],Mat.LMEM$Chisq[2],sprintf("%0.3f",Mat.LMEM$`Pr(>Chisq)`[2])),
  c("Condition",Cond.LMEM$Df[2],Cond.LMEM$Chisq[2],"\\textbf{< 0.0001}")
  # c("Condition",Cond.LMEM$Df[2],Cond.LMEM$Chisq[2],sprintf("\\textbf{%0.3f}",Cond.LMEM$`Pr(>Chisq)`[2]))
))

ST_zTab[,c(2:3)]= apply(ST_zTab[,c(2:3)], 2, function(x) as.numeric(as.character(x)))

colnames(ST_zTab)[1:4] <- c("Main effects","Df",sprintf("$\\chi^{2}$"),"p")
# -------------------------------------------------------------------------

```
<!-- DELETE? -->
  ```{r,label='ST-zLMEMTab',echo=FALSE,warning=FALSE, message=FALSE, eval=FALSE}
kbl(ST_zTab, booktabs = T,escape = F, linesep = "",caption = "ST: z-scores statistical analysis using 3x2x5 LMEM for the effects of Material, Condition, and Group as fixed factors and random intercepts for subjects. Groups comprised of three levels: APD children, and TD children with/without APD siblings. Note: data for the CCRM condition with CCRM-type sentences as distractor (CCRM_F) was not included in the model. Reference levels: Group = APD; Material = ASL; Condition = Quiet-NoAlt.",
    align = c("lccc"),format = "latex",digits = 3) %>% 
  add_header_above(c("z ~ material + Condition + Group + (1 | Subjects)" = 4)) %>%
  add_footnote(c("significant p-values (p < 0.05) are shown in bold."), notation = "symbol") %>%
  column_spec(4, italic = T) 

```
<!-- DELETE? -->
  ```{r, label='ST-zLMEMpost', echo=FALSE, eval=FALSE}

# (ref:tabCap3) ST: z-scores - post-hoc paired comparison t-test for Group. The test was performed on the fitted LMEM model and included adjusted least-squared-mean for the random intercept (subjects) using lsmeans package [emmeans package; @emmeansPackageR].

kbl(postLMEM,booktabs = T, escape = F, linesep = "",caption = '(ref:tabCap3)',
    align = c("lcccccc"),format = "latex",digits = 2) %>%
  kable_styling(latex_options = c("scale_down")) %>%
  add_footnote(c("significant p-values (p < 0.05) are shown in bold."), notation = "symbol") %>%
  column_spec(6, italic = T) %>%
  kable_styling() 
# %>%
# pack_rows("Condition x Material", 1, 5) %>%
# pack_rows("Material", 6,6)

# use this if you want to fore latex to print the table in an exact location!
# latex_options = c("hold_position")
```


