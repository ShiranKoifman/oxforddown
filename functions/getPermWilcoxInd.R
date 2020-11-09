## Wilcox rank sum test
# SK 29/10/2020

getPermWilcoxInd <- function(df,dv,btw,wtn){
 # ---------------------- #
  # Inputs: 
  # df       : Data frame, e.g., ASLN
  # dv       : dependent variable, e.g.,"z_trim"
  # btw   : within/between btw, e.g., Group
  # wtn : e.g, Condition / group
  # Type     : Type of rank test: paired (="TRUE") / unpaired (="FALSE")
  #
  # Outputs:
  # out_list : data for all the tests
  # Out_tab  : table with p's
 # ---------------------- # 

  library(coin)
  out_list <- vector(mode = "list", length = 5) 
  p <- numeric(length(levels(df[,wtn]))) 
  r <- numeric(length(levels(df[,wtn]))) 
  CI <- numeric(length(levels(df[,wtn]))) 
  magnitude <- numeric(length(levels(df[,wtn]))) 
  
  for (i in 1:length(levels(df[,wtn]))) {
    # get all subjects' scores in a single condition 
    ndf <- df[which(df[,wtn] == levels(df[,wtn])[i]),]
    
    fun <- as.formula(paste(dv, btw, sep = "~"))
    x <- coin::wilcox_test(fun, data = ndf, detailed=TRUE,
                           conf.int = TRUE,
                           na.rm=TRUE, paired = FALSE,
                           distribution=approximate(nresample=999999)) 
    # Get effect size (r) and magnitude:
    Z = as.numeric(statistic(x))
    N = length(ndf[,dv])
    r[i] = round(abs(Z)/sqrt(N),2)
    magnitude[i] = ifelse(r[i]<0.3,"small",
                              ifelse(r[i]>=0.3 &
                                       r[i]<=0.5,"moderate",
                                     "large"))
    # get p's:
    p[i] = round(pvalue(x)[1],2)
    # get confidence interval
    CI_temp = confint(x)
    CI[i] <- sprintf("%s - %s",round(CI_temp$conf.int[1],2),
                         round(CI_temp$conf.int[2],2))
    
    # save test results by names
    out_list[[i]] <- x 
    names(out_list)[i] <- paste(levels(df[,wtn])[i], i, sep = "_") 
  }
  
  Out_tab <- cbind(levels(df[,wtn]),CI,
                  p,ifelse(p < .05, "sig.","n.s"),
                  r,magnitude)
  
  # --------------------------------------------------------------------- #
  # gather the data frames into a list for the function output
  MyList <- list(out_list,Out_tab)
  return(MyList)
}

