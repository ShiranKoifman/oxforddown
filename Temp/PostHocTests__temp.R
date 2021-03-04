#######################################
############ t-test: ##################
#######################################


# pairwise.t.test(d_RS$ScaledScore, d_RS$Group2, paired = FALSE, p.adjust.method = "bonf")

# ttest.EHFPTA <- d_HF_PTA_L %>% rstatix::group_by(PTA) %>%
#   pairwise_t_test(data = ., 
#     dBHL ~ Group, pool.sd = FALSE, detailed = TRUE
#     ) %>% adjust_pvalue(method = "bonferroni") %>% 
#   as.data.frame() %>% 
#   mutate(CI = sprintf("%s - %s",
#                       round(conf.low,2),
#                       round(conf.high,2))) %>%
#   mutate_if(is.numeric, round, 2) %>%
#   select(estimate, df, statistic, p.adj, CI) %>%
#   dplyr::rename("95\\%%-CI"=CI, "p-value"=p.adj, "Df"=df, "Estimate"=estimate)
# 
# ttest.EHFPTA$`p-value` <- ifelse(ttest.EHFPTA$`p-value` <.001,"\\textbf{< 0.001}", 
#                              ifelse(ttest.EHFPTA$`p-value` <.05, sprintf("\\textbf{%.02f}",ttest.EHFPTA$`p-value`),
#                                     ttest.EHFPTA$`p-value`))




```{r, label='EHFAud-PTAttest', echo=FALSE}
kbl(ttest.EHFPTA,booktabs = T, escape = F, linesep = "",caption = 'EHF audiometry: Paired-comparison t-tests for PTA x Group. Bonferroni correction was applied for multiple comparisons',
    align = c("lcccccc"),format = "latex",digits = 2) %>%
  kable_styling(latex_options = c("scale_down")) %>%
  add_footnote(c("significant p-values (p < 0.05) are shown in bold."), notation = "symbol") %>%
  add_footnote(c("PTA: average detection threshold (dB HL) at 8, 11, & 16 kHz.","BE: PTA at the better ear."), notation = "none") %>%
  column_spec(4, italic = T)

# use this if you want to fore latex to print the table in an exact location!
# latex_options = c("hold_position")
```

#######################################
############ Wilcox: ##################
#######################################


```{r,label='EHFAud-PTAPostHoc', message=FALSE, warning=FALSE, include=FALSE, echo=FALSE,results='hide', eval=FALSE}

EHFPTA_Wilcox <- d_HF_PTA_L2 %>% rstatix::group_by(PTA) %>%
  rstatix::wilcox_test(data = ., 
                       dBHL ~ Group,detailed = TRUE) %>% 
  # adjust_pvalue(method = "bonferroni") %>% 
  as.data.frame() %>% 
  mutate(CI = sprintf("%s - %s",
                      round(conf.low,2),
                      round(conf.high,2))) %>%
  mutate_if(is.numeric, round, 2) %>%
  # select(estimate, statistic, p.adj, CI) %>%
  select(estimate, statistic, p, CI) %>%
  dplyr::rename("p-value"=p, "Estimate"=estimate)

EHFPTA_Wilcox$`p-value` <- ifelse(EHFPTA_Wilcox$`p-value` <.001,"\\textbf{< 0.001}", 
                                  ifelse(EHFPTA_Wilcox$`p-value` <.05, sprintf("\\textbf{%.02f}",EHFPTA_Wilcox$`p-value`),
                                         EHFPTA_Wilcox$`p-value`))

colnames(EHFPTA_Wilcox)[4] = sprintf("95\\%%-CI")

EHFPTA_d <- d_HF_PTA_L2 %>% rstatix::group_by(PTA) %>%
  rstatix::cohens_d(dBHL ~ Group, data=., ci = TRUE) %>% as.data.frame() %>%
  mutate_if(is.numeric, round, 2) %>%
  select(effsize,magnitude) %>%
  dplyr::rename("d"=effsize)

EHFPTA <- cbind(PTA=c("PTA$_{Left}$","PTA$_{Right}$"),EHFPTA_Wilcox,EHFPTA_d)

colnames(EHFPTA)[1] = ""

# remove attributes as it causes issues with kbl:
EHFPTA$Estimate <- labelled::remove_attributes(EHFPTA$Estimate, "difference in location")
EHFPTA$statistic <- labelled::remove_attributes(EHFPTA$statistic, "W")
EHFPTA$d <- labelled::remove_attributes(EHFPTA$d, "Cohen's d")
```