```{r eval=FALSE, include=FALSE}
# OPTION 3: ez::ezPerm

# Perm.ENVASA_all <- ez::ezPerm(
#     data = ENVASA_Total
#     , dv = z_trim
#     , wid = listener
#     , within = CondCode
#     , between = Group
#     , perms = 1e3
#     , parallel = FALSE
#     , alarm = FALSE
# )
# 
# print(Perm.ENVASA_all)
# --> sig. main effect for Group and CondCode
#           Effect     p p<.05
# 1          Group 0.017     *
# 2       CondCode 0.033     *
# 3 Group:CondCode 0.068  
```

```{r,label='ENVASA-nparLD2', message=FALSE, warning=FALSE, include=FALSE, results='hide'}
# --------------------------------------------------------------------------
# Try without combined measure:

# OPTION 1: nparLD
ENVASA_Total_ds <- ENVASA_Total %>% filter(CondCode!="Total") %>% droplevels()
ENVASA_Total_ds <- data.frame(ENVASA_Total_ds) 

ENVASA.nparLD_ds <- nparLD(z_trim ~ CondCode * Group, data = ENVASA_Total_ds, subject = "listener", description = TRUE, plot.CI=TRUE)

ENVASA.nparLD_ds <- data.frame(round(ENVASA.nparLD_ds$ANOVA.test,3))
# --> sig. effect for condcode. no difference between groups (p=0.057)
#                Statistic df p.value
# Group              3.631  1   0.057
# CondCode           4.116  1   0.042
# Group:CondCode     2.553  1   0.110


# OPTION 2: npIntFactRep
ENVASA_Total_w_ds <- ENVASA_Total_w[,1:5]
ENVASA_Total_w_ds <- data.frame(ENVASA_Total_w_ds) 
colnames(ENVASA_Total_w_ds)[1] = "subj"
npIntFactRep.ENVASA <- npIntFactRep::npIntFactRep(ENVASA_Total_w_ds)
print(npIntFactRep.ENVASA)
# --> sphericity is violated (use GG correction).
# --> Regular ranks shows sig. Group:CondCode interaction p=9.743992e-05 
```

```{r eval=FALSE, include=FALSE}
# OPTION 3: ez::ezPerm

# Perm.ENVASA_ds <- ez::ezPerm(
#     data = ENVASA_Total_ds
#     , dv = z_trim
#     , wid = listener
#     , within = CondCode
#     , between = Group
#     , perms = 1e3
#     , parallel = FALSE
#     , alarm = FALSE
# )
# 
# print(Perm.ENVASA_ds)
# --> sig. main effect for Group and CondCode
#           Effect     p p<.05
# 1          Group 0.022     *
# 2       CondCode 0.035     *
# 3 Group:CondCode 0.088      

```

```{r eval=FALSE, include=FALSE, label='ENVASA-Tab-nparLD'}
ENVASA.nparLD$p.value = ifelse(ENVASA.nparLD$p.value<.05,sprintf("\\textbf{%0.3f}",ENVASA.nparLD$p.value),ENVASA.nparLD$p.value)
colnames(ENVASA.nparLD)[3] <- "p-value"
row.names(ENVASA.nparLD) <- c("Group","Condition","Group:Condition")

kbl(ENVASA.nparLD,booktabs = T, escape = F, linesep = "",caption = "Statistical analysis for the effects of Group and Condition as well as their interaction (2x3 factorial design with repeated measures) tested with a robust rank-based method for analysis of nonparametric data using nparLD package (REF). Analysis was based on a f1-ld-f1 design ANOVA-type statistic (ATS) test, whereby the first f1 refers to an experimental design with one between-subjects factor (Group) and the seconf f1 refers to a single within-subjects factor (Condition).",
    align = c("lccc"),format = "latex",digits = 3) %>%
  add_footnote(c("significant p-values (p < 0.05) are shown in bold."), notation = "symbol") %>%
  column_spec(4, italic = T)

# use this if you want to fore latex to print the table in an exact location!
# latex_options = c("hold_position")
```
