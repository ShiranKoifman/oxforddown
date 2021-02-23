<!-- DELETE? -->
  ```{r,label='ENVASA-npIntFactRep', message=FALSE, warning=FALSE, results='hide',echo=FALSE, eval=FALSE}

# Normality: Not met (only 3/6 pairs were normally distr.)
# Homogeneity: Not met

# 2-way factorial design:
ENVASA_Total <- data.frame(ENVASA_Total)

# OPTION 1: nparLD
# ENVASA.nparLD_all <- nparLD(z_trim ~ CondCode * Group, data = ENVASA_Total, subject = "listener", description = TRUE, plot.CI=TRUE)
# 
# ENVASA.nparLD_all <- data.frame(round(ENVASA.nparLD_all$ANOVA.test,3))
# --> sig. effect for condcode. no difference between groups

#                Statistic   df p.value
# Group              2.482 1.00   0.115
# CondCode           4.239 1.23   0.031
# Group:CondCode     2.066 1.23   0.146

# OPTION 2: npIntFactRep
ENVASA_Total_w_all <- ENVASA_Total_w[,1:6]
ENVASA_Total_w_all <- data.frame(ENVASA_Total_w_all) 
colnames(ENVASA_Total_w_all)[1] = "subj"
npIntFactRep.ENVASA_all <- npIntFactRep::npIntFactRep(ENVASA_Total_w_all)
print(npIntFactRep.ENVASA_all)
# --> Sphericity is violated. use GG correction.
# --> Regular ranks shows sig. Group:CondCode interaction p=6.806667e-05 

# Apply correction for degree of freedom due to non-sphericity: multiple each df with it's corresponding epsilon
gg_e <- 0.5483168 # Greenhouse-Geisser GGe
df1 <- round(3 * gg_e,3)
df2 <- round(105 * gg_e,3)

############### Test parametric assumptions for z-scores ###############

# Normality: Not met (only 3/6 pairs were normally distr.)
# Homogeneity: Not met

# ------------------------------------------------
# 1. Normality (Shapiro test) 
# linear model

w1 <- lm(z_trim~ CondCode*Group, data = ENVASA_Total)

# Option 1:
shapiro.test(residuals(w1))
# p-value = 6.442e-05 ---> normal distr is rejected!

# Option 2:
NormTest <- ENVASA_Total %>%
  group_by(CondCode, Group) %>%
  rstatix::shapiro_test(z_trim)
# --> this is because of non-normal distr. in the APD group.. 

# ------------------------------------------------
# 2. Homogeneity of variance test (Levene's test)
# Interpretation: homogeneity is met if p>.05

# Option 1:
car::leveneTest(z_trim~ CondCode*Group, data=ENVASA_Total)
# Results: p value = 0.0326 * --> assumption of homogeneity is NOT met!
# Interpretation: test is significant, i.e., the null hypothesis that the variance is not equal cannot be accepted.
```

<!-- DELETE? -->
  ```{r,label='ENVASA-zTab',echo=FALSE,warning=FALSE,message=FALSE, eval=FALSE}
# --------------------------------------------------------------------------------------------------
# Compare differences between Groups by Condition
# --------------------------------------------------------------------------------------------------
source("functions/getPermWilcoxInd.R")

Output <- getPermWilcoxInd(ENVASA_Total,"z_trim","Group","CondCode")
ENVASA_Wilcox <- data.frame(Output[[2]])
ENVASA_Wilcox <- ENVASA_Wilcox[,- c(1,4)]
ENVASA_Wilcox[,c(2:3)]= apply(ENVASA_Wilcox[,c(2:3)], 2, function(x) as.numeric(as.character(x)))
ENVASA_Wilcox$p = ifelse(ENVASA_Wilcox$p<.05,sprintf("\\textbf{%.02f}",ENVASA_Wilcox$p),ENVASA_Wilcox$p)
colnames(ENVASA_Wilcox)[1] <- sprintf("95\\%%-CI")
# get table
ENVASA_tab <- ENVASA_Total %>% ddply(.,~CondCode*Group,summarise,N=length(z_trim),median=round(median(z_trim,na.rm=TRUE),2),sd=round(sd(z_trim,na.rm=TRUE),2), min=round(min(z_trim,na.rm=TRUE),2),max=round(max(z_trim,na.rm=TRUE),2),abnormal=paste0(sprintf("%0.2f",(sum(Outlier,na.rm=TRUE)/length(listener)*100)),"\\%")) %>% 
  arrange(., group_by = Group)

# accounting for trimmed TD children????
# ENVASA_tab$abnormal <- ifelse(ENVASA_tab$Group=="TD",sprintf("%s %s",ENVASA_tab$abnormal,
#                         c("(0.00\\%)","(0.00\\%)","(0.00\\%)")),ENVASA_tab$abnormal)
## Outliers: --------------------------------------------------------------
# Total_s:   nontrimOut=1, trimOut=1. nontrimOut-trimOut/23-N_trim = 0%
# Total_d:   nontrimOut=0, trimOut=0. nontrimOut-trimOut/23-N_trim = 0%
# Total:     nontrimOut=0, trimOut=0. nontrimOut-trimOut/23-N_trim = 0%

ENVASA_tab <- ENVASA_tab[,-grep("Group",colnames(ENVASA_tab))]
# APD first then TD
ENVASA_tab <- cbind(ENVASA_tab[1:3,1:ncol(ENVASA_tab)],
                    ENVASA_tab[4:nrow(ENVASA_tab),2:ncol(ENVASA_tab)],
                    ENVASA_Wilcox)
ENVASA_tab$CondCode <- c("Single","Dual","Combined")
colnames(ENVASA_tab)[1] = "background"

# prepare table using kbl()
# for more options see: https://cran.r-project.org/web/packages/kableExtra/vignettes/awesome_table_in_pdf.pdf
kbl(ENVASA_tab, booktabs = T,escape = F, linesep = "",caption = "ENVASA: Descriptive and statistics of the listeners age-independent standard residuals (z-scores) split by groups and test measures.",
    align = c("lcccccccccccc"),format = "latex",digits = 2) %>% 
  kable_styling(latex_options = c("scale_down")) %>%
  add_header_above(c(" " = 1, "APD" = 6, "TD" = 6, "Wilcoxon rank-sum test" = 4)) %>%
  column_spec(c(8,14),border_left = T) %>%
  add_footnote(c("significant p-values (p < 0.05) are shown in bold."), notation = "symbol") %>%
  column_spec(15, italic = T)
```