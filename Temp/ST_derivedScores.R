```{r,label='ST-DerivedScoreZ',echo=FALSE, message=FALSE, warning=FALSE, results='hide'}
# Get derived measures ----------------------------------------------------------------------

###### ASLN ######
# change data from Long2Wide
ASLN_w <- ASLN %>%
  pivot_wider(
    id_cols = c("listener","Group","Age"),
    names_from = "CondCode",
    values_from = c("uRevs","z_trim")) %>%
  ungroup()
colnames(ASLN_w)[1:length(ASLN_w)] <- gsub("-", "_", colnames(ASLN_w[,c(1:length(ASLN_w))]))

# -- get derived scores --------------------------------------
# uRevs
# ASLN_w$ASLN_QAlt_vs_QNoAlt   <-  ASLN_w$uRevs_Q_ASLN_Alt - ASLN_w$uRevs_Q_ASLN_NoAlt
# ASLN_w$ASLN_AMSSN_vs_QNoAlt  <-  ASLN_w$uRevs_AMSSN_ASLN_Alt - ASLN_w$uRevs_Q_ASLN_NoAlt
# ASLN_w$ASLN_MDR_vs_QNoAlt    <-  ASLN_w$uRevs_MDR_F_ASLN_Alt - ASLN_w$uRevs_Q_ASLN_NoAlt
# ASLN_w$ASLN_ENG_vs_QNoAlt    <-  ASLN_w$uRevs_ENG_F_ASLN_Alt - ASLN_w$uRevs_Q_ASLN_NoAlt

# z-scores
ASLN_w$ASLN_QAlt_vs_QNoAlt_z   <-  ASLN_w$z_trim_Q_ASLN_Alt - ASLN_w$z_trim_Q_ASLN_NoAlt
ASLN_w$ASLN_AMSSN_vs_QNoAlt_z  <-  ASLN_w$z_trim_AMSSN_ASLN_Alt - ASLN_w$z_trim_Q_ASLN_NoAlt
ASLN_w$ASLN_MDR_vs_QNoAlt_z    <-  ASLN_w$z_trim_MDR_F_ASLN_Alt - ASLN_w$z_trim_Q_ASLN_NoAlt
ASLN_w$ASLN_ENG_vs_QNoAlt_z    <-  ASLN_w$z_trim_ENG_F_ASLN_Alt - ASLN_w$z_trim_Q_ASLN_NoAlt

# -- Change data layout from Wide2Long -----------------------
# ASLN_L_uRevs <- ASLN_w %>% 
#   select(listener,Age, Group, ASLN_QAlt_vs_QNoAlt, ASLN_AMSSN_vs_QNoAlt,
#          ASLN_MDR_vs_QNoAlt, ASLN_ENG_vs_QNoAlt) %>%
#   pivot_longer(
#     cols = c("ASLN_QAlt_vs_QNoAlt","ASLN_AMSSN_vs_QNoAlt",
#              "ASLN_MDR_vs_QNoAlt","ASLN_ENG_vs_QNoAlt"), 
#     names_to = "CondCode", 
#     names_ptypes = list(CondCode = factor()),
#     values_to = "uRevs") %>%
#   ungroup()

ASLN_L_z <- ASLN_w %>% 
  dplyr::select(listener, Age, Group, ASLN_QAlt_vs_QNoAlt_z, ASLN_AMSSN_vs_QNoAlt_z,
                ASLN_MDR_vs_QNoAlt_z, ASLN_ENG_vs_QNoAlt_z) %>%
  pivot_longer(
    cols = c("ASLN_QAlt_vs_QNoAlt_z","ASLN_AMSSN_vs_QNoAlt_z",
             "ASLN_MDR_vs_QNoAlt_z","ASLN_ENG_vs_QNoAlt_z"), 
    names_to = "CondCode", 
    names_ptypes = list(CondCode = factor()),
    values_to = "zScore") %>% 
  ungroup()

###### CCRM ######
# change data from Long2Wide
CCRM_w <- CCRM %>%
  pivot_wider(
    id_cols = c("listener","Group","Age"),
    names_from = "CondCode",
    values_from = c("uRevs","z_trim")) %>%
  ungroup()
colnames(CCRM_w)[1:length(CCRM_w)] <- gsub("-", "_", colnames(CCRM_w[,c(1:length(CCRM_w))]))

# -- get derived scores --------------------------------------
# uRevs
# CCRM_w$CCRM_QAlt_vs_QNoAlt   <-  CCRM_w$uRevs_Q_CCRM_Alt - CCRM_w$uRevs_Q_CCRM_NoAlt
# CCRM_w$CCRM_AMSSN_vs_QNoAlt  <-  CCRM_w$uRevs_AMSSN_CCRM_Alt - CCRM_w$uRevs_Q_CCRM_NoAlt
# CCRM_w$CCRM_MDR_vs_QNoAlt    <-  CCRM_w$uRevs_MDR_F_CCRM_Alt - CCRM_w$uRevs_Q_CCRM_NoAlt
# CCRM_w$CCRM_ENG_vs_QNoAlt    <-  CCRM_w$uRevs_ENG_F_CCRM_Alt - CCRM_w$uRevs_Q_CCRM_NoAlt
# CCRM_w$CCRM_CCRM_vs_QNoAlt   <-  CCRM_w$uRevs_CCRM_F_CCRM_Alt - CCRM_w$uRevs_Q_CCRM_NoAlt

# z-scores
CCRM_w$CCRM_QAlt_vs_QNoAlt_z   <-  CCRM_w$z_trim_Q_CCRM_Alt - CCRM_w$z_trim_Q_CCRM_NoAlt
CCRM_w$CCRM_AMSSN_vs_QNoAlt_z  <-  CCRM_w$z_trim_AMSSN_CCRM_Alt - CCRM_w$z_trim_Q_CCRM_NoAlt
CCRM_w$CCRM_MDR_vs_QNoAlt_z    <-  CCRM_w$z_trim_MDR_F_CCRM_Alt - CCRM_w$z_trim_Q_CCRM_NoAlt
CCRM_w$CCRM_ENG_vs_QNoAlt_z    <-  CCRM_w$z_trim_ENG_F_CCRM_Alt - CCRM_w$z_trim_Q_CCRM_NoAlt
CCRM_w$CCRM_CCRM_vs_QNoAlt_z   <-  CCRM_w$z_trim_CCRM_F_CCRM_Alt - CCRM_w$z_trim_Q_CCRM_NoAlt

# -- Change data layout from Wide2Long -----------------------
# CCRM_L_uRevs <- CCRM_w %>% 
#   select(listener, Age, Group, CCRM_QAlt_vs_QNoAlt, CCRM_AMSSN_vs_QNoAlt,
#          CCRM_MDR_vs_QNoAlt, CCRM_ENG_vs_QNoAlt,CCRM_CCRM_vs_QNoAlt) %>%
#   pivot_longer(
#     cols = c("CCRM_QAlt_vs_QNoAlt","CCRM_AMSSN_vs_QNoAlt",
#              "CCRM_MDR_vs_QNoAlt","CCRM_ENG_vs_QNoAlt","CCRM_CCRM_vs_QNoAlt"), 
#     names_to = "CondCode", 
#     names_ptypes = list(CondCode = factor()),
#     values_to = "uRevs") %>%
#   ungroup()

CCRM_L_z <- CCRM_w %>% 
  dplyr::select(listener, Age, Group, CCRM_QAlt_vs_QNoAlt_z, CCRM_AMSSN_vs_QNoAlt_z,
                CCRM_MDR_vs_QNoAlt_z, CCRM_ENG_vs_QNoAlt_z,CCRM_CCRM_vs_QNoAlt_z) %>%
  pivot_longer(
    cols = c("CCRM_QAlt_vs_QNoAlt_z","CCRM_AMSSN_vs_QNoAlt_z",
             "CCRM_MDR_vs_QNoAlt_z","CCRM_ENG_vs_QNoAlt_z","CCRM_CCRM_vs_QNoAlt_z"), 
    names_to = "CondCode", 
    names_ptypes = list(CondCode = factor()),
    values_to = "zScore") %>% 
  ungroup()
```



```{r, label='ST-DerivedPlotsASL', fig.cap="Add caption here.", fig.align='center', fig.width=8, fig.asp=.8, out.width='100%',echo=FALSE, message=FALSE, warning=FALSE, results='hide',eval=FALSE, show=FALSE}

############ ASL ############
ASLN_w$Group <- factor(ASLN_w$Group,levels=c("APD","TD"))

# -------------------------------------------------------
require(GGally)

ggpairs(ASLN_w[c(4:17,2)],
        lower = list(continuous = "smooth"),
        mapping = aes(color = ASLN_w$Group),title="ST-ASL: Condition - Q-NoAlt (z)")

# -------------------------------------------------------

ggplot(ASLN_L_z, aes(x=CondCode,y=zScore,fill=Group))+ 
  geom_boxplot(position=position_dodge(width=0.8),outlier.shape=NA)+ 
  geom_quasirandom(dodge.width=0.9,shape=1,colour="blue")+
  # geom_hline(yintercept=0, linetype="dashed",color = "black", size=0.5) +
  labs(y = latex2exp::TeX("\\textbf{\\overset{standardised residual (z-score)}{better performance $\\rightarrow$}}"),x = NULL)+
  # annotate("text", label = latex2exp::TeX("$\\leftarrow$ better performance"),x=0.48, y=1, angle=90, size=5)+
  annotate(geom ="text",  x=1, y = 7.5, label = "ST-ASL", size=9, face="bold",fontface=2) +
  # scale_fill_discrete(name = "Group", labels = c("APD", "TD",bquote(bold(TD[sib]))))+
  # geom_text(aes(label = ifelse(ASLN$z_trim>CutOff,ASLN$listener,"")),
  # position = position_dodge2(width = .8,padding = 0.1))+
  # scale_y_continuous(limits = c(-5,8),breaks=seq(-5,8,2))+
  # scale_x_discrete(labels=c("Q-ASLN-NoAlt" ="Quiet-NoAlt","Q-ASLN-Alt"="Quiet-Alt",
  #                           "AMSSN-ASLN-Alt"="AMSSN-Alt","MDR_F-ASLN-Alt"="MDR_F-Alt","ENG_F-ASLN-Alt"="ENG_F-Alt"))+
  theme_bw()+
  theme(axis.text = element_text(size = 11, face="bold",colour = "black"),
        axis.title.x = element_text(size=11, face="bold"),
        axis.title.y = element_text(size=11, face="bold"),
        legend.title = element_text(size=11, face="bold"),
        legend.text  = element_text(size=11, face="bold"))

# -------------------------------------------------------
```