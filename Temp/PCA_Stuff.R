```{r, label='PCA-ST', include=FALSE}
library(magrittr)

# combine wide format ST data: #############################################################
ASL_w <- ASLN %>%
  pivot_wider(
    id_cols = c("listener","Group"),
    names_from = "CondCode",
    values_from = c("z_trim")) %>%
  ungroup()

CCRM_w<- CCRM %>%
  pivot_wider(
    id_cols = c("listener","Group"),
    names_from = "CondCode",
    values_from = c("z_trim")) %>%
  ungroup()

ST_w <- merge(ASL_w,CCRM_w,by=c("listener","Group"))
row.names(ST_w) <- levels(ST_w$listener)
ST_w <- data.frame(ST_w)

# Compute PCA: ############################################################################
library(FactoMineR) # to get PCA

# to get PCA results
# install.packages("Factoshiny")
# install.packages("FactoInvestigate")
# install.packages("factoextra")
library(Factoshiny)
library(FactoInvestigate)
library(factoextra)

#--------------------------------------------
## Check assumptions first:

# 1. psych::KMO() computes Kaiser-Meyer-Olkin Measure of Sampling Adequacy (see Field book p904). 
# The test examine the adequacy of the sample-size in order to confidently perform a PCA.
# Overall KMO value categories (Hutcheson & Sofroniou, 1999):
# * Acceptable limit: >= 0.5 (Field, 2009)
# * 0.5 - 0.7: mediocre
# * 0.7 - 0.8: good
# * 0.8 -0.9: great
# * >0.9: superb 

psych::KMO(ST_w[,3:13])
# Overall KMO = 0.76 ==> Good

# 2. Bartlett’s test
psych::cortest.bartlett(ST_w[,3:13])

# \chi^2(55)=190.139, p < 0.001 ==> sig. Thus, factor analysis is appropriate.
#--------------------------------------------
# compute PCA

PCA.ST <- PCA(ST_w[,3:13], ncp=3, graph = TRUE,scale.unit = TRUE) # with scaling

#--------------------------------------------
# Look at rotation of the solution
prc <- prcomp(ST_w[,3:13], center=T, scale=T, retx=T)
wts <- as.data.frame(prc$rotation[,1:3])


# rotation
# v2 <- varimax(prc$rotation[,1:3])
# wts <- as.data.frame(v2$loadings[,1:3])

# pca3 <- principal(ST_w[,3:13], nfactors = 3, rotate = "varimax")
# print.psych(pca3, cut = 0.2, sort = TRUE)
# 
# pca3 <- principal(ST_w[,3:13], nfactors = 3, rotate = "oblimin")
# print.psych(pca3, cut = 0.3, sort = TRUE)

PCA.ST.rot <- as.data.frame(wts)
PCA.ST.rot <- cbind("Item" = rownames(PCA.ST.rot),PCA.ST.rot)


PCA.ST.eig <- t(get_eigenvalue(PCA.ST)[1:3,1:3])
colnames(PCA.ST.eig) <- c("PC1","PC2","PC3")
PCA.ST.eig <- cbind("Item" = rownames(PCA.ST.eig),PCA.ST.eig)

PCA_tab <- rbind(PCA.ST.rot,PCA.ST.eig)
rownames(PCA_tab) <- NULL
PCA_tab$Item <- as.factor(c("Q-ASLN-NoAlt","Q-ASLN-Alt","AMSSN-ASLN.Alt","MDR\\_F-ASLN-Alt",
                            "ENG\\_F-ASLN-Alt", "Q-CCRM-NoAlt", "Q-CCRM-Alt","AMSSN-CCRM-Alt",
                            "MDR\\_F-CCRM-Alt", "ENG\\_F-CCRM-Alt", "CCRM\\_F-CCRM-Alt", "eigenvalue",
                            "variance (\\%)","cumulative variance (\\%)"))
PCA_tab[,c(2:4)] = apply(PCA_tab[,c(2:4)], 2, function(x) as.numeric(as.character(x)))

# Calculate composite measures: ##############################################################

ST_w$ST <- (ST_w$Q.ASLN.NoAlt + ST_w$Q.ASLN.Alt + ST_w$AMSSN.ASLN.Alt + ST_w$MDR_F.ASLN.Alt +
              ST_w$ENG_F.ASLN.Alt +
              ST_w$Q.CCRM.NoAlt + ST_w$Q.CCRM.Alt + ST_w$AMSSN.CCRM.Alt +
              ST_w$MDR_F.CCRM.Alt + ST_w$ENG_F.CCRM.Alt+ ST_w$CCRM_F.CCRM.Alt) / 11

ST_w$ASL <- (ST_w$Q.ASLN.NoAlt + ST_w$Q.ASLN.Alt + ST_w$AMSSN.ASLN.Alt + ST_w$MDR_F.ASLN.Alt +
               ST_w$ENG_F.ASLN.Alt) / 5
ST_w$CCRM <- (ST_w$Q.CCRM.NoAlt + ST_w$Q.CCRM.Alt + ST_w$AMSSN.CCRM.Alt +
                ST_w$MDR_F.CCRM.Alt + ST_w$ENG_F.CCRM.Alt+ ST_w$CCRM_F.CCRM.Alt) / 6

ST_w$Spch <- (ST_w$MDR_F.ASLN.Alt + ST_w$ENG_F.ASLN.Alt +
                ST_w$MDR_F.CCRM.Alt + ST_w$ENG_F.CCRM.Alt + ST_w$CCRM_F.CCRM.Alt) / 5

ST_w$NoSpch <- (ST_w$Q.ASLN.NoAlt + ST_w$Q.ASLN.Alt + ST_w$AMSSN.ASLN.Alt +
                  ST_w$Q.CCRM.NoAlt + ST_w$Q.CCRM.Alt + ST_w$AMSSN.CCRM.Alt) / 6

ST_L <- ST_w %>%
  pivot_longer(
    cols = c("ST","ASL","CCRM","NoSpch","Spch"), 
    names_to = "MeasureST", 
    names_ptypes = list(MeasureST = factor()),
    values_to = "SRdT") %>% 
  ungroup() %>% select(.,listener, Group, MeasureST, SRdT)

# t1 <- ggplot(ST_L, aes(x=MeasureST,y=SRdT,fill=Group))+ 
#   geom_rect(aes(xmin=-Inf, xmax=Inf, ymin=-CutOff, ymax=CutOff),fill="lightgray", alpha=0.05)+
#   geom_boxplot(position=position_dodge(width=0.8),outlier.shape=NA)+ 
#   geom_quasirandom(dodge.width=0.9,shape=1,colour="blue")+
#   # geom_hline(yintercept=0, linetype="dashed",color = "black", size=0.5) +
#   annotate("text", label = latex2exp::TeX("$\\leftarrow$ better performance"),x=0.48, y=1, angle=90, size=5)+
#   labs(y = "PCA based combined measures (z-score)" ,x = NULL)+ 
#   theme_bw()+
#   theme(axis.text = element_text(size = 11, face="bold",colour = "black"),
#         axis.title.x = element_text(size=11, face="bold"),
#         axis.title.y = element_text(size=11, face="bold"),
#         legend.title = element_text(size=11, face="bold"),
#         legend.text  = element_text(size=11, face="bold"))
# t1

```

```{r, label='PCA-Lang', include=FALSE, eval=FALSE}
library(magrittr)

# combine wide format Language data: #############################################################

# ECLIPS, CELF-RS, CCC2 (GCC,DLD, PLI)
Lang_RS <-  d_RS %>% select(.,listener, Group, "RS"=ScaledScore)

Lang_ECLIPS <-  ECLiPS_Total %>%  select(.,listener, Group, "ECLiPS"=ScaledScore)


Lang_CCC2 <-  d_CCC2 %>% mutate("GCC" = d_CCC2$GCC,
                                "SIDC" = d_CCC2$SPC,
                                "DLD" = ifelse(d_CCC2$GCC<55 & d_CCC2$SPC>0, 1, 0),
                                "PLI" = ifelse(d_CCC2$GCC<55 & d_CCC2$SPC<0, 1, 0)) %>%
  dplyr::select(.,listener,Group,GCC,SIDC,DLD,PLI)


df_list <- list(Lang_RS,Lang_ECLIPS,Lang_CCC2)
LangAll  <- df_list %>% reduce(full_join, by = c("listener","Group")) 

LangAll <- LangAll[LangAll$listener!=c("APD14","TD13"), ] %>% droplevels()
row.names(LangAll) <- levels(LangAll)

# Compute PCA: ############################################################################
library(FactoMineR) # to get PCA

# to get PCA results
# install.packages("Factoshiny")
# install.packages("FactoInvestigate")
# install.packages("factoextra")
library(Factoshiny)
library(FactoInvestigate)
library(factoextra)

#--------------------------------------------
## Check assumptions first:

# 1. psych::KMO() computes Kaiser-Meyer-Olkin Measure of Sampling Adequacy (see Field book p904). 
# The test examine the adequacy of the sample-size in order to confidently perform a PCA.
# Overall KMO value categories (Hutcheson & Sofroniou, 1999):
# * Acceptable limit: >= 0.5 (Field, 2009)
# * 0.5 - 0.7: mediocre
# * 0.7 - 0.8: good
# * 0.8 -0.9: great
# * >0.9: superb 

psych::KMO(LangAll[,3:6])
# Overall KMO = 0.76 ==> Good

# 2. Bartlett’s test
psych::cortest.bartlett(LangAll[,3:6])

# \chi^2(55)=190.139, p < 0.001 ==> sig. Thus, factor analysis is appropriate.
#--------------------------------------------
# compute PCA

PCA.ST <- PCA(LangAll[,3:6], ncp=2, graph = TRUE,scale.unit = TRUE) # with scaling

#--------------------------------------------
# Look at rotation of the solution
prc <- prcomp(na.omit(LangAll[,3:6]), center=T, scale=T, retx=T)
wts <- as.data.frame(prc$rotation[,1:3])


# rotation
# v2 <- varimax(prc$rotation[,1:3])
# wts <- as.data.frame(v2$loadings[,1:3])

# pca3 <- principal(ST_w[,3:13], nfactors = 3, rotate = "varimax")
# print.psych(pca3, cut = 0.2, sort = TRUE)
# 
# pca3 <- principal(ST_w[,3:13], nfactors = 3, rotate = "oblimin")
# print.psych(pca3, cut = 0.3, sort = TRUE)

PCA.ST.rot <- as.data.frame(wts)
PCA.ST.rot <- cbind("Item" = rownames(PCA.ST.rot),PCA.ST.rot)


PCA.ST.eig <- t(get_eigenvalue(PCA.ST)[1:3,1:3])
colnames(PCA.ST.eig) <- c("PC1","PC2","PC3")
PCA.ST.eig <- cbind("Item" = rownames(PCA.ST.eig),PCA.ST.eig)

PCA_tab <- rbind(PCA.ST.rot,PCA.ST.eig)
rownames(PCA_tab) <- NULL
PCA_tab$Item <- as.factor(c("CELF-RS","ECLiPS","GCC","SIDC","eigenvalue",
                            "variance (\\%)","cumulative variance (\\%)"))
PCA_tab[,c(2:4)] = apply(PCA_tab[,c(2:4)], 2, function(x) as.numeric(as.character(x)))

# Calculate composite measures: ##############################################################

ST_w$ST <- (ST_w$Q.ASLN.NoAlt + ST_w$Q.ASLN.Alt + ST_w$AMSSN.ASLN.Alt + ST_w$MDR_F.ASLN.Alt +
              ST_w$ENG_F.ASLN.Alt +
              ST_w$Q.CCRM.NoAlt + ST_w$Q.CCRM.Alt + ST_w$AMSSN.CCRM.Alt +
              ST_w$MDR_F.CCRM.Alt + ST_w$ENG_F.CCRM.Alt+ ST_w$CCRM_F.CCRM.Alt) / 11

ST_w$ASL <- (ST_w$Q.ASLN.NoAlt + ST_w$Q.ASLN.Alt + ST_w$AMSSN.ASLN.Alt + ST_w$MDR_F.ASLN.Alt +
               ST_w$ENG_F.ASLN.Alt) / 5
ST_w$CCRM <- (ST_w$Q.CCRM.NoAlt + ST_w$Q.CCRM.Alt + ST_w$AMSSN.CCRM.Alt +
                ST_w$MDR_F.CCRM.Alt + ST_w$ENG_F.CCRM.Alt+ ST_w$CCRM_F.CCRM.Alt) / 6

ST_w$Spch <- (ST_w$MDR_F.ASLN.Alt + ST_w$ENG_F.ASLN.Alt +
                ST_w$MDR_F.CCRM.Alt + ST_w$ENG_F.CCRM.Alt + ST_w$CCRM_F.CCRM.Alt) / 5

ST_w$NoSpch <- (ST_w$Q.ASLN.NoAlt + ST_w$Q.ASLN.Alt + ST_w$AMSSN.ASLN.Alt +
                  ST_w$Q.CCRM.NoAlt + ST_w$Q.CCRM.Alt + ST_w$AMSSN.CCRM.Alt) / 6

ST_L <- ST_w %>%
  pivot_longer(
    cols = c("ST","ASL","CCRM","NoSpch","Spch"), 
    names_to = "MeasureST", 
    names_ptypes = list(MeasureST = factor()),
    values_to = "SRdT") %>% 
  ungroup() %>% select(.,listener, Group, MeasureST, SRdT)

# t1 <- ggplot(ST_L, aes(x=MeasureST,y=SRdT,fill=Group))+ 
#   geom_rect(aes(xmin=-Inf, xmax=Inf, ymin=-CutOff, ymax=CutOff),fill="lightgray", alpha=0.05)+
#   geom_boxplot(position=position_dodge(width=0.8),outlier.shape=NA)+ 
#   geom_quasirandom(dodge.width=0.9,shape=1,colour="blue")+
#   # geom_hline(yintercept=0, linetype="dashed",color = "black", size=0.5) +
#   annotate("text", label = latex2exp::TeX("$\\leftarrow$ better performance"),x=0.48, y=1, angle=90, size=5)+
#   labs(y = "PCA based combined measures (z-score)" ,x = NULL)+ 
#   theme_bw()+
#   theme(axis.text = element_text(size = 11, face="bold",colour = "black"),
#         axis.title.x = element_text(size=11, face="bold"),
#         axis.title.y = element_text(size=11, face="bold"),
#         legend.title = element_text(size=11, face="bold"),
#         legend.text  = element_text(size=11, face="bold"))
# t1

```


```{r, label='PCA-Tab',echo=FALSE,warning=FALSE,message=FALSE}
# prepare table using kbl()
# for more options see: https://cran.r-project.org/web/packages/kableExtra/vignettes/awesome_table_in_pdf.pdf
kbl(PCA_tab, booktabs = T,escape = F, linesep = "", caption = "Add caption here.",
    align = c("lccc"),format = "latex",digits = 2) %>%
  row_spec(11, hline_after =T) %>%
  add_footnote(c("Add note here"), notation = "none")
```


```{r, label='getData-Corr', include=FALSE}
# merge all data together (wide format) ################################################
library(dplyr)
# 1. RS
Cmpr_RS <- d_RS %>% mutate("RS" = d_RS$ScaledScore) %>% select(.,listener, Group, RS)
# 2. CCC2 (GCC & SIDC)
Cmpr_CCC2 <-  d_CCC2 %>% mutate("GCC" = d_CCC2$GCC,"SIDC" = d_CCC2$SPC) %>%
  dplyr::select(.,listener,Group,GCC,SIDC)
# 3. ECLiPS (Total score)
Cmpr_ECLIPS <-  ECLiPS_Total %>% mutate("ECLiPS" = ECLiPS_Total$ScaledScore) %>% 
  select(.,listener, Group,ECLiPS)
# 4. Standard PTA [(500 + 1k + 2k + 4k)/4]
Cmpr_RegPTA <-  d_PTA_L %>% filter(PTA=="PTA_RL") %>% 
  dplyr::select(.,listener, Group,"PTA"=dBHL) %>% droplevels()
Cmpr_RegPTA <- data.frame(Cmpr_RegPTA)
# 5. Standard PTA [(11k + 16k) / 2]
Cmpr_EHFPTA <-  d_HF_PTA_L %>% filter(PTA=="PTA_RL") %>% 
  dplyr::select(.,listener, Group,"PTA_EHF"=dBHL) %>% droplevels()
# 6. ENVASA (Total_s)
Cmpr_ENVASA <-  ENVASA_Total %>% filter(CondCode=="Total_s") %>% 
  dplyr::select(.,listener, Group,"ENVASA"=z_trim) %>% droplevels()
# 8. ST combined measures (ASL, CCRM, NoSpch, Spch)
Cmpr_ST <- ST_w %>% select(., listener, Group, ST,ASL, CCRM, NoSpch, Spch)
# 9. LiSNS (SSN, S0N0, S0N90, SRM)
Cmpr_LiSNS <- LiSNS_w %>% select(., listener, Group, "SSN"=z_trim_SSN, "S0N0"=z_trim_S0N0,
                                 "S0N90"=z_trim_S0N90, "SRM"=z_trim_SRM)
Cmpr_LiSNS$Group <- as.factor(Cmpr_LiSNS$Group)  
Cmpr_LiSNS <- data.frame(Cmpr_LiSNS) 

# merge all columns together:
df_list <- list(Cmpr_RS,Cmpr_CCC2,Cmpr_ECLIPS,Cmpr_RegPTA,Cmpr_EHFPTA,Cmpr_ENVASA,Cmpr_ST,Cmpr_LiSNS)
CmprAll_w  <- df_list %>% purrr::reduce(full_join, by = c("listener","Group")) 

# Remove APD14
if (RmvSubj==1){CmprAll_w <- CmprAll_w[ ! CmprAll_w$listener %in% Subj2Remove, ] %>% droplevels()}

# write.csv(CmprAll_w,"corrData.csv")

# get correlation table: ####################################################################
library(xtable)

# x is a matrix containing the data
# method : correlation method. "pearson"" or "spearman"" is supported
# removeTriangle : remove upper or lower triangle
# results :  if "html" or "latex"
# the results will be displayed in html or latex format
corstars <-function(x, method=c("pearson", "spearman"), removeTriangle=c("upper", "lower"),
                    result=c("none", "html", "latex")){
  #Compute correlation matrix
  require(Hmisc)
  x <- as.matrix(x)
  correlation_matrix<-rcorr(x, type=method[1])
  R <- correlation_matrix$r # Matrix of correlation coefficients
  p <- correlation_matrix$P # Matrix of p-value 
  
  ## Define notions for significance levels; spacing is important.
  mystars <- ifelse(p < .0001, "****", ifelse(p < .001, "*** ", ifelse(p < .01, "**  ", ifelse(p < .05, "*   ", "    "))))
  # mystars <- ifelse(p < .0001, "\\text{*}\\text{*}\\text{*}\\text{*}", ifelse(p < .001, "\\text{*}\\text{*}\\text{*}", ifelse(p < .01, "\\text{*}\\text{*}", ifelse(p < .05, "\\text{*}", " "))))
  
  ## trunctuate the correlation matrix to two decimal
  R <- format(round(cbind(rep(-1.11, ncol(x)), R), 2))[,-1]
  
  ## build a new matrix that includes the correlations with their appropriate stars
  Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x))
  diag(Rnew) <- paste(diag(R), " ", sep="")
  rownames(Rnew) <- colnames(x)
  colnames(Rnew) <- paste(colnames(x), "", sep="")
  
  ## remove upper triangle of correlation matrix
  if(removeTriangle[1]=="upper"){
    Rnew <- as.matrix(Rnew)
    Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
    Rnew <- as.data.frame(Rnew)
  }
  
  ## remove lower triangle of correlation matrix
  else if(removeTriangle[1]=="lower"){
    Rnew <- as.matrix(Rnew)
    Rnew[lower.tri(Rnew, diag = TRUE)] <- ""
    Rnew <- as.data.frame(Rnew)
  }
  
  ## remove last column and return the correlation matrix
  Rnew <- cbind(Rnew[1:length(Rnew)-1])
  if (result[1]=="none") return(Rnew)
  else{
    if(result[1]=="html") print(xtable(Rnew), type="html")
    else print(xtable(Rnew), type="latex") 
  }
} 

################# Correlation matrix for all subjects ################# 

CorrAll <- Hmisc::rcorr(as.matrix(CmprAll_w)[,3:17],type="spearman")

CorrAll <- corstars(CmprAll_w[,3:17], result="none", method="spearman")

# nCols = c(3,4,6,7,10:17)
# CorrAll <- Hmisc::rcorr(as.matrix(CmprAll_w)[,nCols],type="spearman")
# CorrAll <- corstars(CmprAll_w[,nCols], result="none", method="spearman")

################# Correlation matrix by group ################# 

# CorrByGroups <- lapply(split(CmprAll_w, CmprAll_w$Group), function(x) {
#     rcorr(as.matrix(x[,3:17]), type="spearman")})

CorrByGroups <- lapply(split(CmprAll_w, CmprAll_w$Group), function(x) {
  corstars(x[,3:17], result="none", method="pearson")})

CorrByGroups_TD <- data.frame(CorrByGroups["TD"]$TD)
colnames(CorrByGroups_TD)[6] <- "PTA\\_EHF"
rownames(CorrByGroups_TD)[6] <- "PTA\\_EHF"
CorrByGroups_APD <- data.frame(CorrByGroups["APD"]$APD)
colnames(CorrByGroups_APD)[6] <- "PTA\\_EHF"
rownames(CorrByGroups_APD)[6] <- "PTA\\_EHF"
```

```{r, label='CorrMatrix-Tab', echo=FALSE,warning=FALSE,message=FALSE}
# prepare table using kbl()
# for more options see: https://cran.r-project.org/web/packages/kableExtra/vignettes/awesome_table_in_pdf.pdf

kbl(CorrByGroups_TD, booktabs = T, escape = F, linesep = "", caption = "TD group: add caption here.",
    align = c("lccccccccccccc"),format = "latex",digits = 3) %>%
  kable_styling(latex_options = c("scale_down")) %>%
  add_footnote(c("significant p-values: **** p < .0001, *** p < .001, ** p < .01, * p < .05"), notation = "none")

kbl(CorrByGroups_APD, booktabs = T, escape = F, linesep = "", caption = "APD group: add caption here.",
    align = c("lccccccccccccc"),format = "latex",digits = 3) %>%
  kable_styling(latex_options = c("scale_down")) %>%
  add_footnote(c("significant p-values: **** p < .0001, *** p < .001, ** p < .01, * p < .05"), notation = "none")
```

```{r, label='ggpairs-plot', eval=FALSE, include=FALSE}
# ggpairs plots: ---------------------------------------------------------
require(ggpubr)
require(GGally)
#
my_fn <- function(data, mapping, ...){
  ggplot(data = data, mapping = mapping, ...) + 
    geom_point(size=0.5,na.rm=TRUE) + 
    geom_smooth(method=lm, fill="#69b3a2", se=FALSE, na.rm=TRUE) +
    stat_cor(method = "spearman", na.rm = TRUE, size=3) +
    theme(legend.position = "bottom",
          legend.direction = "horizontal")
}

graph_corr <- ggpairs(CmprAll_w[c("Group","RS","GCC","ECLiPS","PTA","ST","ASL","CCRM",
                                  "NoSpch","Spch","SSN","S0N0","S0N90","SRM")],
                      upper = list(continuous= my_fn),
                      lower = list(continuous= my_fn),
                      aes(color = CmprAll_w$Group),legend=1) +
  theme(legend.position = "bottom",
        legend.direction = "horizontal")
#aes(color = ST_w$Group, alpha=.9),legend=1) 

graph_corr

ggsave(paste0('ggpairs_Corr_', date, '.png'), width = 25, height = 15, units = c("in"), dpi = 300)
```

