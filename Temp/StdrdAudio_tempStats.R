```{r,label='RegAud-PTAstats', message=FALSE, warning=FALSE, include=FALSE, echo=FALSE,results='hide'}

# pairwise.t.test(d_RS$ScaledScore, d_RS$Group2, paired = FALSE, p.adjust.method = "bonf")

ttest.strdPTA <- d_PTA_L %>% rstatix::group_by(PTA) %>%
  pairwise_t_test(data = ., 
                  dBHL ~ Group, pool.sd = FALSE, detailed = TRUE
  ) %>% adjust_pvalue(method = "bonferroni") %>% 
  as.data.frame() %>% 
  mutate(CI = sprintf("%s - %s",
                      round(conf.low,2),
                      round(conf.high,2))) %>%
  mutate_if(is.numeric, round, 2) %>%
  select(estimate, df, statistic, p.adj, CI) %>%
  dplyr::rename("p-value"=p.adj, "Df"=df, "Estimate"=estimate)
# dplyr::rename("95\\%%-CI"=CI, "p-value"=p.adj, "Df"=df, "Estimate"=estimate)

ttest.strdPTA$`p-value` <- ifelse(ttest.strdPTA$`p-value` <.001,"\\textbf{< 0.001}", 
                                  ifelse(ttest.strdPTA$`p-value` <.05, sprintf("\\textbf{%.02f}",ttest.strdPTA$`p-value`),
                                         ttest.strdPTA$`p-value`))

colnames(ttest.strdPTA)[5] = sprintf("95\\%%-CI")

strdPTA_d <- d_PTA_L %>% rstatix::group_by(PTA) %>%
  rstatix::cohens_d(dBHL ~ Group, data=., ci = TRUE) %>% as.data.frame() %>%
  mutate_if(is.numeric, round, 2) %>%
  select(effsize,magnitude) %>%
  dplyr::rename("d"=effsize)

ttest.strdPTA <- cbind(PTA_tab,ttest.strdPTA,strdPTA_d)

colnames(ttest.strdPTA)[1] = ""

# remove attributes as it causes issues with kbl:
ttest.strdPTA$Df <- labelled::remove_attributes(ttest.strdPTA$Df, "Df")
ttest.strdPTA$statistic <- labelled::remove_attributes(ttest.strdPTA$statistic, "t")
ttest.strdPTA$d <- labelled::remove_attributes(ttest.strdPTA$d, "Cohen's d")
ttest.strdPTA$magnitude <- as.character(ttest.strdPTA$magnitude)
```

```{r, label='RegAud-PTAttest', echo=FALSE}
kbl(ttest.strdPTA,booktabs = T, escape = F, linesep = "",caption = "Standard audiometry: Paired-comparison t-tests for PTA x Group. Bonferroni correction was applied for multiple comparisons.", align = c("lccccccccccccccccc"),format = "latex",digits = 2) %>%
  kable_styling(latex_options = c("scale_down")) %>%
  add_header_above(c(" " = 1, "APD" = 5, "TD" = 5,"paired t-tests" = 7)) %>%
  add_footnote(c("significant p-values (p < 0.05) are shown in bold."), notation = "symbol") %>%
  add_footnote(c("PTA: average detection threshold (dB HL) at 0.5, 1, 2, & 4 kHz.","BE: PTA at the better ear."), notation = "none")%>%
  column_spec(c(8,13),border_left = T) %>%
  column_spec(16, italic = T)

# use this if you want to fore latex to print the table in an exact location!
# latex_options = c("hold_position")

```


```{r,label='stdAud-AgeLMEM',eval=FALSE,include=FALSE}
# Age effect?

model1 <- lmer(dBHL~Freq * Ear * Group * Age +(1|listener), data=d_L, REML=FALSE)
summary(model1)

# 4-way interaction
(model2<-update(model1,  ~ . - Freq:Ear:Group:Age))
anova(model1,model2)
summary(model2)
# model2: dBHL ~ Freq + Ear + Group + Age + (1 | listener) + Freq:Ear + 
# model2:     Freq:Group + Ear:Group + Freq:Age + Ear:Age + Group:Age + 
# model2:     Freq:Ear:Group + Freq:Ear:Age + Freq:Group:Age + Ear:Group:Age
# model1: dBHL ~ Freq * Ear * Group * Age + (1 | listener)
#        npar    AIC    BIC  logLik deviance Chisq Df Pr(>Chisq)
# model2   45 3131.6 3322.7 -1520.8   3041.6                    
# model1   50 3136.9 3349.3 -1518.5   3036.9 4.688  5     0.4551

# Ear:Group:Age interaction
(model3<-update(model2,  ~ . - Ear:Group:Age))
anova(model2,model3)
summary(model3)
# model3: dBHL ~ Freq + Ear + Group + Age + (1 | listener) + Freq:Ear + 
# model3:     Freq:Group + Ear:Group + Freq:Age + Ear:Age + Group:Age + 
# model3:     Freq:Ear:Group + Freq:Ear:Age + Freq:Group:Age
# model2: dBHL ~ Freq + Ear + Group + Age + (1 | listener) + Freq:Ear + 
# model2:     Freq:Group + Ear:Group + Freq:Age + Ear:Age + Group:Age + 
# model2:     Freq:Ear:Group + Freq:Ear:Age + Freq:Group:Age + Ear:Group:Age
#        npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)  
# model3   44 3132.4 3319.2 -1522.2   3044.4                       
# model2   45 3131.6 3322.7 -1520.8   3041.6 2.7603  1    0.09663 .

# Freq:Group:Age interaction
(model4<-update(model3,  ~ . - Freq:Group:Age))
anova(model3,model4)
summary(model4)
# model4: dBHL ~ Freq + Ear + Group + Age + (1 | listener) + Freq:Ear + 
# model4:     Freq:Group + Ear:Group + Freq:Age + Ear:Age + Group:Age + 
# model4:     Freq:Ear:Group + Freq:Ear:Age
# model3: dBHL ~ Freq + Ear + Group + Age + (1 | listener) + Freq:Ear + 
# model3:     Freq:Group + Ear:Group + Freq:Age + Ear:Age + Group:Age + 
# model3:     Freq:Ear:Group + Freq:Ear:Age + Freq:Group:Age
#        npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
# model4   39 3125.2 3290.8 -1523.6   3047.2                     
# model3   44 3132.4 3319.2 -1522.2   3044.4 2.7839  5     0.7333

# Freq:Ear:Age interaction
(model5<-update(model4,  ~ . - Freq:Ear:Age))
anova(model4,model5)
summary(model5)
# model5: dBHL ~ Freq + Ear + Group + Age + (1 | listener) + Freq:Ear + 
# model5:     Freq:Group + Ear:Group + Freq:Age + Ear:Age + Group:Age + 
# model5:     Freq:Ear:Group
# model4: dBHL ~ Freq + Ear + Group + Age + (1 | listener) + Freq:Ear + 
# model4:     Freq:Group + Ear:Group + Freq:Age + Ear:Age + Group:Age + 
# model4:     Freq:Ear:Group + Freq:Ear:Age
#        npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
# model5   34 3120.0 3264.4 -1526.0   3052.0                     
# model4   39 3125.2 3290.8 -1523.6   3047.2 4.8457  5      0.435

# Freq:Ear:Group interaction
(model6<-update(model5,  ~ . - Freq:Ear:Group))
anova(model5,model6)
summary(model6)
# model6: dBHL ~ Freq + Ear + Group + Age + (1 | listener) + Freq:Ear + 
# model6:     Freq:Group + Ear:Group + Freq:Age + Ear:Age + Group:Age
# model5: dBHL ~ Freq + Ear + Group + Age + (1 | listener) + Freq:Ear + 
# model5:     Freq:Group + Ear:Group + Freq:Age + Ear:Age + Group:Age + 
# model5:     Freq:Ear:Group
#        npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
# model6   29 3115.4 3238.6 -1528.7   3057.4                     
# model5   34 3120.0 3264.4 -1526.0   3052.0 5.3969  5     0.3694

# Group:Age interaction
(model7<-update(model6,  ~ . - Group:Age))
anova(model6,model7)
summary(model7)
# model7: dBHL ~ Freq + Ear + Group + Age + (1 | listener) + Freq:Ear + 
# model7:     Freq:Group + Ear:Group + Freq:Age + Ear:Age
# model6: dBHL ~ Freq + Ear + Group + Age + (1 | listener) + Freq:Ear + 
# model6:     Freq:Group + Ear:Group + Freq:Age + Ear:Age + Group:Age
#        npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
# model7   28 3113.6 3232.5 -1528.8   3057.6                     
# model6   29 3115.4 3238.6 -1528.7   3057.4 0.1536  1     0.6952

# Ear:Age interaction
(model8<-update(model7,  ~ . - Ear:Age))
anova(model7,model8)
summary(model8)
# model8: dBHL ~ Freq + Ear + Group + Age + (1 | listener) + Freq:Ear + 
# model8:     Freq:Group + Ear:Group + Freq:Age
# model7: dBHL ~ Freq + Ear + Group + Age + (1 | listener) + Freq:Ear + 
# model7:     Freq:Group + Ear:Group + Freq:Age + Ear:Age
#        npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
# model8   27 3111.6 3226.2 -1528.8   3057.6                     
# model7   28 3113.6 3232.5 -1528.8   3057.6 0.0145  1     0.9042

# Freq:Age interaction
(model9<-update(model8,  ~ . - Freq:Age))
anova(model8,model9)
summary(model9)
# model9: dBHL ~ Freq + Ear + Group + Age + (1 | listener) + Freq:Ear + 
# model9:     Freq:Group + Ear:Group
# model8: dBHL ~ Freq + Ear + Group + Age + (1 | listener) + Freq:Ear + 
# model8:     Freq:Group + Ear:Group + Freq:Age
#        npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
# model9   22 3110.7 3204.2 -1533.4   3066.7                     
# model8   27 3111.6 3226.2 -1528.8   3057.6 9.1431  5     0.1035

# Ear:Group interaction
(model10<-update(model9,  ~ . - Ear:Group))
anova(model9,model10)
summary(model10)
# model10: dBHL ~ Freq + Ear + Group + Age + (1 | listener) + Freq:Ear + 
# model10:     Freq:Group
# model9: dBHL ~ Freq + Ear + Group + Age + (1 | listener) + Freq:Ear + 
# model9:     Freq:Group + Ear:Group
#         npar    AIC    BIC  logLik deviance Chisq Df Pr(>Chisq)   
# model10   21 3115.6 3204.7 -1536.8   3073.6                       
# model9    22 3110.7 3204.2 -1533.4   3066.7 6.807  1    0.00908 **

# best mode --> model 1: dBHL~PTA*Group+(1|listener)
tab_model(model10)

# ---------------------------------------------------------------------------------
# test main effects:
BestModel <- model10
# report::report(model10)

# ---------------------------------------------------------------------------------
# test main effects:
rstatix::Anova(BestModel,type="II",test.statistic="Chisq")

# Freq:Group
(BestModel.1 <- update(BestModel,  . ~ . -Freq:Group))
FreqGroup.LMEM <- anova(BestModel,BestModel.1)
summary(BestModel.1)
# p=0.4319

# Freq:Ear
(BestModel.2 <- update(BestModel,  . ~ . -Freq:Ear))
FreqEar.LMEM <- anova(BestModel,BestModel.2)
summary(BestModel.2)
# p=0.498

# Age
(BestModel.3 <- update(BestModel,  . ~ . -Age))
Age.LMEM <- anova(BestModel,BestModel.3)
summary(BestModel.3)
# p=0.0594

# Age
(BestModel.3 <- update(BestModel,  . ~ . -Age))
Group.LMEM <- anova(BestModel,BestModel.3)
summary(BestModel.3)
# p=0.0594

```

```{r,label='RegAud-PTALMEM', message=FALSE, warning=FALSE, include=FALSE, echo=FALSE,results='hide',eval=FALSE}

# Normality: met
# homoscedasticity: met

######## lmer model ##########################################
## Find Best fit 
# start with a saturated model (3-way interaction)

model1 <- lmer(dBHL~PTA*Group+(1|listener), data=d_PTA_L, REML=FALSE)

summary(model1)

(model2<-update(model1,  ~ . - PTA:Group))
anova(model1,model2)
summary(model2)
# model2: dBHL ~ PTA + Group + (1 | listener)
# model1: dBHL ~ PTA * Group + (1 | listener)
#        npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)   
# model2    7 725.44 747.48 -355.72   711.44                        
# model1   10 719.17 750.65 -349.59   699.17 12.269  3   0.006516 **

# best mode --> model 1: dBHL~PTA*Group+(1|listener)
tab_model(model1)

# ---------------------------------------------------------------------------------
# test main effects:
BestModel <- model1

# Freq:Group
(BestModel.1 <- update(model1,  . ~ . -PTA:Group))
PTAGroup.LMEM <- anova(BestModel,BestModel.1)
summary(BestModel.1)

# The significant interaction between PTA and group implies a significant main effect of both factors. 

# ------------------------------------------------------------------------------
# Post-hoc paired comparison
# -----------------------------------------------------------------------------------------------------
# PTA x Group:
(ref1 <- emmeans::lsmeans(BestModel,~ PTA+Group))
comps <- emmeans::contrast(ref1,alpha=0.05,method="pairwise",adjust="bonferroni") #,adjust=NULL) adjust="bonferroni"
summary(comps)
# contrast                  estimate     SE  df t.ratio p.value
# PTA_R APD - PTA_R TD       0.799 0.942  59.8  0.848  0.4000 
# PTA_L APD - PTA_L TD       2.682 0.942  59.8  2.846  0.0061 
# PTA_RL APD - PTA_RL TD     1.740 0.942  59.8  1.846  0.0699
# PTA_BE APD - PTA_BE TD     1.242 0.942  59.8  1.318  0.1927

PTAGroup.postLMEM <- data.frame(comps[c(4,11,17,22)])
PTAGroup.CI <- confint(comps[c(4,11,17,22)])

PTAGroup.CI <- PTAGroup.CI  %>% mutate( CI = sprintf("%s - %s",
                                                     round(lower.CL,2),
                                                     round(upper.CL,2)))

PTAGroup.postLMEM <- cbind(PTAGroup.postLMEM,PTAGroup.CI$CI)
colnames(PTAGroup.postLMEM)[1:7] <- c("Contrasts","Estimate","SE","Df","t-value","p-value",sprintf("95\\%%-CI"))
row.names(PTAGroup.postLMEM) <- NULL

PTAGroup.postLMEM$`p-value` <- round(PTAGroup.postLMEM$`p-value`,3)


PTAGroup.postLMEM$`p-value` = ifelse(PTAGroup.postLMEM$`p-value`<.05,sprintf("\\textbf{%0.3f}",PTAGroup.postLMEM$`p-value`),
                                     PTAGroup.postLMEM$`p-value`)

RegAud_tab2 <- cbind(PTA_tab, PTAGroup.postLMEM[,2:7])
colnames(RegAud_tab2)[1] = ""
# -------------------------------------------------------------------------

########################### Test assumptions #########################################

## By PTA #######################

# Normality: met
# homoscedasticity: met

# linear model
w1 <- lm(dBHL ~ PTA * Group, data = d_PTA_L)

#### normality (shapiro-wilk) ###########

# Option 1:
# linearity is met if p>.05
shapiro.test(residuals(w1))
# W = 0.99106, p-value = 0.3599 --> normally distr.

# Option 2: by fixed factors: --> ALL normal..
NormTest <- d_PTA_L %>%
  group_by(PTA,Group) %>%
  rstatix::shapiro_test(dBHL)

#### Homogeneity of variance (Levene's test) ###########
# homogeneity is met if p>.05

# Option 1: Is the lm model fully covered here?
leveneTest(residuals(w1) ~ PTA * Group, data = d_PTA_L)
# -> homogeneity of the full model is met

# Levene's Test for Homogeneity of Variance (center = median)
#        Df F value Pr(>F)
# group   7  0.8126  0.578
#       164    

# Option 2: by fixed factors
# per condition
# VarTest <- d_PTA_L %>%
#   rstatix::group_by(Group) %>%
#   levene_test(dBHL ~ PTA)

# Homogeneity of variance is met

```

```{r,label='RegAud-PTALMEM2', message=FALSE, warning=FALSE, include=FALSE, echo=FALSE,results='hide',eval=FALSE}

# Normality: met
# homoscedasticity: met

######## lmer model ##########################################
## Find Best fit 
# start with a saturated model (3-way interaction)

model1 <- lmer(dBHL~PTA*Group2+(1|listener), data=d_PTA_L, REML=FALSE)

summary(model1)

(model2<-update(model1,  ~ . - PTA:Group2))
anova(model1,model2)
summary(model2)
# model2: dBHL ~ PTA + Group2 + (1 | listener)
# model1: dBHL ~ PTA * Group2 + (1 | listener)
#        npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)  
# model2    8 727.37 752.55 -355.68   711.37                       
# model1   14 726.32 770.39 -349.16   698.32 13.045  6    0.04233 *

# best mode --> model 1: dBHL~PTA*Group+(1|listener)
tab_model(model1)

# ---------------------------------------------------------------------------------
# test main effects:
BestModel <- model1

rstatix::Anova(BestModel,type="II",test.statistic="Chisq")

# PTA:Group2
# Freq:Group
(BestModel.1 <- update(BestModel,  . ~ . -PTA:Group2))
PTAGroup.LMEM <- anova(BestModel,BestModel.1)

summary(BestModel.1)
# BestModel.1: dBHL ~ PTA + Group2 + (1 | listener)
# BestModel: dBHL ~ PTA * Group2 + (1 | listener)
#             npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)  
# BestModel.1    8 727.37 752.55 -355.68   711.37                       
# BestModel     14 726.32 770.39 -349.16   698.32 13.045  6    0.04233 *

# The significant interaction between PTA and group implies a significant main effect of both factors. 

# ------------------------------------------------------------------------------
# Post-hoc paired comparison
# -----------------------------------------------------------------------------------------------------
# PTA x Group:
(ref1 <- emmeans::lsmeans(BestModel,~ PTA+Group2))
comps <- emmeans::contrast(ref1,alpha=0.05,method="pairwise",adjust=NULL) #,adjust=NULL) adjust="bonferroni"
summary(comps)
comps %>% data.frame(comps) %>% filter(p.value<=0.05) %>% mutate_if(is.numeric, round, 3)

# contrast                  estimate     SE  df t.ratio p.value
# PTA_R APD - PTA_R TD       0.799 0.942  59.8  0.848  0.4000 
# PTA_L APD - PTA_L TD       2.682 0.942  59.8  2.846  0.0061 
# PTA_RL APD - PTA_RL TD     1.740 0.942  59.8  1.846  0.0699
# PTA_BE APD - PTA_BE TD     1.242 0.942  59.8  1.318  0.1927

PTAGroup.postLMEM <- data.frame(comps[c(4,11,17,22)])
PTAGroup.CI <- confint(comps[c(4,11,17,22)])

PTAGroup.CI <- PTAGroup.CI  %>% mutate( CI = sprintf("%s - %s",
                                                     round(lower.CL,2),
                                                     round(upper.CL,2)))

PTAGroup.postLMEM <- cbind(PTAGroup.postLMEM,PTAGroup.CI$CI)
colnames(PTAGroup.postLMEM)[1:7] <- c("Contrasts","Estimate","SE","Df","t-value","p-value",sprintf("95\\%%-CI"))
row.names(PTAGroup.postLMEM) <- NULL

PTAGroup.postLMEM$`p-value` <- round(PTAGroup.postLMEM$`p-value`,3)


PTAGroup.postLMEM$`p-value` = ifelse(PTAGroup.postLMEM$`p-value`<.001,"\\textbf{< 0.001}", ifelse(PTAGroup.postLMEM$`p-value`<.05, sprintf("\\textbf{%.03f}",PTAGroup.postLMEM$`p-value`),PTAGroup.postLMEM$`p-value`))

RegAud_tab2 <- cbind(PTA_tab, PTAGroup.postLMEM[,2:7])
colnames(RegAud_tab2)[1] = ""
# -------------------------------------------------------------------------

########################### Test assumptions #########################################

## By PTA #######################

# Normality: met
# homoscedasticity: met

# linear model
w1 <- lm(dBHL ~ PTA * Group2, data = d_PTA_L)

#### normality (shapiro-wilk) ###########

# Option 1:
# linearity is met if p>.05
shapiro.test(residuals(w1))
# W = 0.99106, p-value = 0.3599 --> normally distr.

# Option 2: by fixed factors: --> ALL normal..
NormTest <- d_PTA_L %>%
  group_by(PTA,Group2) %>%
  rstatix::shapiro_test(dBHL)

#### Homogeneity of variance (Levene's test) ###########
# homogeneity is met if p>.05

# Option 1: Is the lm model fully covered here?
leveneTest(residuals(w1) ~ PTA * Group2, data = d_PTA_L)
# -> homogeneity of the full model is met

# Levene's Test for Homogeneity of Variance (center = median)
#        Df F value Pr(>F)
# group   7  0.8126  0.578
#       164    

# Option 2: by fixed factors
# per condition
# VarTest <- d_PTA_L %>%
#   rstatix::group_by(Group) %>%
#   levene_test(dBHL ~ PTA)

# Homogeneity of variance is met
```


<!-- (ref:tabCap1) Post-hoc paired-comparison t-tests for PTA x Group. The test was performed on the fitted LMEM model and included adjusted least-squared-mean for the random intercepts (subjects) using lsmeans package [@emmeansPackageR]. -->
  
  ```{r, label='RegAud-LMEMpost', echo=FALSE, eval=FALSE}
kbl(RegAud_tab2,booktabs = T, escape = F, linesep = "",caption = '(ref:tabCap1)',
    align = c("lcccccccccccccccc"),format = "latex",digits = 2) %>%
  kable_styling(latex_options = c("scale_down")) %>%
  add_header_above(c(" "=1, "APD" = 5, "TD" = 5,"post-hoc paired t-tests" = 6)) %>%
  add_footnote(c("significant p-values (p < 0.05) are shown in bold."), notation = "symbol") %>%
  add_footnote(c("PTA: average detection threshold (dB HL) at 0.5, 1, 2, & 4 kHz.","BE: PTA at the better ear."), notation = "none")%>%
  column_spec(c(7,12),border_left = T) %>%
  column_spec(16, italic = T)

# use this if you want to fore latex to print the table in an exact location!
# latex_options = c("hold_position")
```