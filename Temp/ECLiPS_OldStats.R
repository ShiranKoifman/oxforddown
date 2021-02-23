<!-- DELETE? -->
  ```{r,label='ECLIPS-stats', echo=FALSE,warning=FALSE,message=FALSE,results='hide', eval=FALSE}
# --------------------------------------------------------------------------------------------------
# Assumptions:
ECLiPS_Total <- d_E_L %>% filter(Measure=="Total") %>% droplevels() 

# linear model
w1 <- lm(ScaledScore ~ Group, data = ECLiPS_Total)

# 1. Normality (Shapiro-Wilk test) --> ~is met for TD, but not for APD
# data is normally distributed if p >.05

# QQ plot of residuals
# qqPlot(residuals(w1))

#jpeg('Q-Q Plot.png', width = 10, height = 6, units = 'in', res = 300)
# par(mfrow=c(1,2))    # set the plotting area into a 1*2 array
# qqnorm(d_RS$ScaledScore, pch = 1, frame = FALSE,main = "SRdT - Normal Q-Q Plot")
# qqline(d_RS$ScaledScore, col = "red", lwd = 2)
# qqnorm(rstandard(w1), pch = 1, frame = FALSE,main = "Residuals - Normal Q-Q Plot")
# qqline(rstandard(w1), col = "red", lwd = 2)
#dev.off()

# Option 1:
shapiro.test(residuals(w1))

# Option 2 by conditions:
NormTest <- ECLiPS_Total %>%
  group_by(Group) %>%
  rstatix::shapiro_test(ScaledScore)

# 2. Homoggeneity of variance (Levene's test) --> is NOT met!
# homogeneity is met if p>.05
# Option 1:
car::leveneTest(ScaledScore ~ Group, data=ECLiPS_Total,center=median)
# p = 0.0001389 ***
# Option 2: 

# --------------------------------------------------------------------------------------------------
# ==> Nonparametric t-test:
source("functions/getPermWilcoxInd.R")

Output <- getPermWilcoxInd(ECLiPS_Total,"ScaledScore","Group","Measure")
ECLiPS_Wilcox <- data.frame(Output[[2]])
ECLiPS_Wilcox <- ECLiPS_Wilcox[,- c(1,4)]
ECLiPS_Wilcox[,c(2:3)]= apply(ECLiPS_Wilcox[,c(2:3)], 2, function(x) as.numeric(as.character(x)))
ECLiPS_Wilcox$p = ifelse(ECLiPS_Wilcox$p<.05,sprintf("\\textbf{< 0.001}"),ECLiPS_Wilcox$p)
colnames(ECLiPS_Wilcox)[1] <- sprintf("95\\%%-CI")

```