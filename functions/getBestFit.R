getBestFit <- function(df,CondCodeName,CutOff,slope1,int1,brk,slope2){
  # --------------------------------------------
  # Input: 
  # df           : the complete tests data frame
  # CondCodeName : test condition of interest
  # GroupName    : group of interest
  #
  # Optional inputs for broken lines fit. 
  # nls() enables to manually feed some parameters and it gradually reduce them. 
  # If the start parameters are too far from the actual value the model won't converge.
  # see discussion here: https://stats.stackexchange.com/questions/183653/getting-the-right-starting-values-for-an-nls-model-in-r
  # slope1 : slope
  # int1   : intercept
  # brk    : brake point
  # --------------------------------------------  
  # Output:  
  # Output[[1]]  : df_All
  # Output[[2]]  : m1.linear
  # Output[[3]]  : m1.TwoLines
  # Output[[4]]  : ComprMdls1
  # Output[[5]]  : preFinalModel   
  # Output[[6]]  : m2.linear
  # Output[[7]]  : m2.TwoLines
  # Output[[8]]  : postFinalModel
  # Output[[9]]  : ComprMdls2
  # Output[[10]] : p

  # --------------------------------------------
  
  # uncomment for debug mode
  # browser()
  
  # check if explicit input parameters were given
  if(missing(slope1)){slope1 = 1}
  if(missing(slope2)){slope2 = 0}
  if(missing(int1)){int1 = 2}
  if(missing(brk)){brk = 9.5}

  
  ##########################################################################
  ## Declare some functions ------------------------------------------------
  ##########################################################################
  
  # broken stick regression
  TwoLinesFCT <- function(x, slope1, slope2, int1, brk) {
    (x<=brk)*(slope1 * x + int1) + (x>brk)*(slope2 * x + brk*(slope1-slope2)+int1) 
  }
  
  # Linear regression:
  linearFCT <- function(x, slope1, int1) {
    int1 + slope1 * x
  }
  # --------------------------------------------------------------------- #
  
  # get data
  # Both groups
  df_All <- df %>% filter(CondCode==CondCodeName) %>% droplevels()
  
  # TD only:
  # If specified, remove observations listed in 'TempRmv'
  if(("TempRmv" %in% colnames(df))==TRUE){
    df_TD <- df %>% filter(CondCode==CondCodeName & Group=="TD" & TempRmv<1) %>% droplevels() 
  } else {
    # otherwise, keep all TD obs.
    df_TD <- df %>% filter(CondCode==CondCodeName & Group=="TD") %>% droplevels()
  }

  
  ##########################################################################
  ## 1. Fit broken lines and single linear models --------------------------
  ##########################################################################
  
  # broken stick regression: ---------------------------------------------
  # Sinle slope (slope2=0)
  if (slope2==0){
  m1.TwoLines <- nls(uRevs ~ TwoLinesFCT(Age, slope1, 0, int1, brk),
                    start=list(slope1=slope1, int1=int1, brk=brk),
                    data=df_TD,
                    upper = max(df_TD$Age),
                    trace = FALSE,
                    algorithm = "port")
  # add a second slope if needed:
  } else {
  m1.TwoLines <- nls(uRevs ~ TwoLinesFCT(Age, slope1, slope2, int1, brk),
                    start=list(slope1=slope1,slope2=slope2, int1=int1, brk=brk),
                    data=df_TD,
                    upper = max(df_TD$Age),
                    trace = FALSE,
                    algorithm = "port")
  }
  
  # Linear regression: ----------------------------------------------------
  m1.linear <- nls(uRevs ~ linearFCT(Age, slope1, int1),
                  start=list(slope1=1, int1 = 2),
                  data=df_TD,
                  trace = FALSE)
  
  ##########################################################################
  ## 2. Compare models with and without a breakpoint -----------------------
  ##########################################################################
  
  ComprMdls1 <- anova(m1.linear,m1.TwoLines)
  
  ##########################################################################
  ## 3. Find best model (segmented/linear) ---------------------------------
  ##########################################################################

  if(ComprMdls1$`Pr(>F)`[2]<.05){
  # use broken lines regression
    preFinalModel <- 2 # 2= segmented line; 1=single line
  # Calculate z-scores (TDs only)
    res <- resid(m1.TwoLines)
    # now calculate z scores for TD only
    # sigma(r) function in r equals: sqrt(sum((r$residuals^2)/(N_TD-2)))
    # sigma_res <- sqrt(sum(res^2)/(length(res)-2))
    
    # ?????$$$$$$$$$$ WHICH ONE TO USE?
    df_TD$z <- res/sigma(m1.TwoLines)  
    # df_TD$z <- res/sigma_res
    # plot(df_TD$z)
    
  } else {
    # use single linear regression line
    preFinalModel <- 1
    # Calculate z-scores (TDs only)
    res <- resid(m1.linear)
    
    # now calculate z scores for TD only
    # sigma(r) function in r equals: sqrt(sum((r$residuals^2)/(N_TD-2)))
    # sigma_res <- sqrt(sum(res^2)/(length(res)-2))
    
    # ?????$$$$$$$$$$ WHICH ONE TO USE?
    df_TD$z <- res/sigma(m1.linear)  
    # df_TD$z <- res/sigma_res
    # plot(df_TD$z)
  }
  ##########################################################################
  ## 4. Remove outliers from the selected model ----------------------------
  ##########################################################################

  nRows2Rmv <- which(abs(df_TD$z) >CutOff)
  # run if nRows2Rmv is NOT empty, otherwise don't trim
  if (identical(nRows2Rmv, integer(0))==FALSE){
    df_TD_trimmed <- df_TD[- which(abs(df_TD$z) >CutOff),]
    RmvdSubj <- df_TD$listener[which(abs(df_TD$z) >CutOff)] %>% droplevels()
  } else {
    df_TD_trimmed <- df_TD
    RmvdSubj <- "none"
    }
  
  ##########################################################################
  ## 5. Re-fit models and choose the best one   ----------------------------
  ##########################################################################
  
  # broken stick regression: ----------------------------------------------------
  # Single slope (slope2=0)
  if (slope2==0){
    m2.TwoLines <- tryCatch({
      nls(uRevs ~ TwoLinesFCT(Age, slope1, 0, int1, brk),
                      start=list(slope1=slope1, int1=int1, brk=brk),
                      data=df_TD_trimmed,
                      upper = max(df_TD_trimmed$Age),
                      trace = FALSE,
                      algorithm = "port")}, 
             error=function(e){
               cat("ERROR : m2.TwoLines: ",conditionMessage(e), "\n")
               })
    # add a second slope if needed:
  } else {
    m2.TwoLines <- tryCatch({
    m2.TwoLines <- nls(uRevs ~ TwoLinesFCT(Age, slope1, slope2, int1, brk),
                      start=list(slope1=slope1,slope2=slope2, int1=int1, brk=brk),
                      data=df_TD_trimmed,
                      upper = max(df_TD_trimmed$Age),
                      trace = FALSE,
                      algorithm = "port")}, 
    error=function(e){
      cat("ERROR : m2.TwoLines: ",conditionMessage(e), "\n")
    })
  }
  
  # Linear regression: ----------------------------------------------------
  m2.linear <- nls(uRevs ~ linearFCT(Age, slope1, int1),
                  start=list(slope1=1, int1 = 2),
                  data=df_TD_trimmed,
                  trace = FALSE)
  
  ##########################################################################
  ## 6. Compare models with and without a breakpoint -----------------------
  ##########################################################################
  ComprMdls2 <- c()
  if(is.null(m2.TwoLines)==FALSE){
  ComprMdls2 <- anova(m2.linear,m2.TwoLines)
  } else {
    ComprMdls2$`Pr(>F)` <- c(1,1)
  }
  
  ##########################################################################
  ## 7. Get z-scores for subj. from both groups (APD/TD) -------------------
  ##########################################################################

    if(ComprMdls2$`Pr(>F)`[2]<.05){
    # use broken lines regression
    postFinalModel <- 2 # 2= segmented line; 1=single line
    m.Final <- m2.TwoLines
    
    # Calculate z-scores (ALL subjects)
    df_All$predicted <- predict(m.Final, df_All)
    # res <- resid(m.Final)
    # sqrt(sum((res^2)/(length(unique(df_TD_trimmed$listener))-2)))
    df_All$z_trim <- (df_All$uRevs-df_All$predicted)/sigma(m.Final)
    # plot(df_All$z_trim)
  } else {
    postFinalModel <- 1 # 2= segmented line; 1=single line
    m.Final <- m2.linear
    
    # Calculate z-scores (ALL subjects)
    df_All$predicted <- predict(m.Final, df_All)
    # res <- resid(m.Final)
    # sqrt(sum((res^2)/(length(unique(df_TD_trimmed$listener))-2)))
    df_All$z_trim <- (df_All$uRevs-df_All$predicted)/sigma(m.Final)
    # plot(df_All$z_trim)
  }
  
  ##########################################################################
  ## 8. Get plots ----------------------------------------------------------
  ##########################################################################
  yMin <- min(df_All$uRevs)
  yMax <- max(df_All$uRevs)
  
  # -------------------------------------------------------------0-------- #
  ## Pre trimming (TD only)
  
  # a prediction function for the plots (linear)
  LinearLine <- function(age) {
    linearFCT(age,
              as.numeric(coefficients(m1.linear)[1]),
              as.numeric(coefficients(m1.linear)[2]))
  }
  
  # a prediction function for the plots (broken lines)
  if (slope2==0){
    TwoLines <- function(age) {
      TwoLinesFCT(age,
                  as.numeric(coefficients(m1.TwoLines)[1]), 
                  0, 
                  as.numeric(coefficients(m1.TwoLines)[2]), 
                  as.numeric(coefficients(m1.TwoLines)[3]))
    }
  } else {
    TwoLines <- function(age) {
      TwoLinesFCT(age,
                  as.numeric(coefficients(m1.TwoLines)[1]), 
                  as.numeric(coefficients(m1.TwoLines)[2]), 
                  as.numeric(coefficients(m1.TwoLines)[3]), 
                  as.numeric(coefficients(m1.TwoLines)[4]))
    }
  }
  
  # plot
  p1 <- ggplot(df_TD, aes(Age, uRevs)) + geom_point() + ylim(yMin, yMax)
  p1 <- p1 +
    stat_function(fun = TwoLines, color="red") +
    stat_function(fun = LinearLine, color="blue") +
    geom_text(label=df_TD$listener, size=3, colour="blue") +
    labs(title = paste0(CondCodeName," - non-trimmed TD group"))

  # --------------------------------------------------------------------- #
  ## Post trimming (TD only)
  
  # a prediction function for the plots (linear)
  LinearLine <- function(age) {
    linearFCT(age,
              as.numeric(coefficients(m2.linear)[1]),
              as.numeric(coefficients(m2.linear)[2]))
  }
  
  # a prediction function for the plots (broken lines)
  if(is.null(m2.TwoLines)==FALSE){
  if (slope2==0){
    TwoLines <- function(age) {
      TwoLinesFCT(age,
                  as.numeric(coefficients(m2.TwoLines)[1]), 
                  0, 
                  as.numeric(coefficients(m2.TwoLines)[2]), 
                  as.numeric(coefficients(m2.TwoLines)[3]))
    }
  } else {
    TwoLines <- function(age) {
      TwoLinesFCT(age,
                  as.numeric(coefficients(m2.TwoLines)[1]), 
                  as.numeric(coefficients(m2.TwoLines)[2]), 
                  as.numeric(coefficients(m2.TwoLines)[3]), 
                  as.numeric(coefficients(m2.TwoLines)[4]))
    }
  }
  
  # plot
  p2 <- ggplot(df_TD_trimmed, aes(Age, uRevs)) + geom_point() + ylim(yMin, yMax)
  p2 <- p2 +
    stat_function(fun = TwoLines, color="red") +
    stat_function(fun = LinearLine, color="blue") +
    labs(title = paste0(CondCodeName," - trimmed TD group"),
         subtitle = paste0("trimmed TD's: ",toString(RmvdSubj)))
  } else {
    # plot
    p2 <- ggplot(df_TD_trimmed, aes(Age, uRevs)) + geom_point() + ylim(yMin, yMax)
    p2 <- p2 + stat_function(fun = LinearLine, color="blue") +
      labs(title = paste0(CondCodeName," - trimmed TD group"),
           subtitle = paste0("trimmed TD's: ",toString(RmvdSubj))) +
      annotate("text",x=9, y=mean(df_TD_trimmed$uRevs),label = "m2.TwoLines did not converged!!",color="red")
    
  }
  # --------------------------------------------------------------------- #
  ## Final model (TD + APD)
  
  # If 'postFinalModel'=1, use single line for the plot, 
  # Otherwise, use segmented line:
  if (postFinalModel==1){
  # a prediction function for the plots (linear)
  LinearLine <- function(age) {
    linearFCT(age,
              as.numeric(coefficients(m.Final)[1]),
              as.numeric(coefficients(m.Final)[2]))
  }
  
  PlotLine <- stat_function(fun = LinearLine, color="blue")
  } else {
  # a prediction function for the plots (broken lines)
  if (slope2==0){
    TwoLines <- function(age) {
      TwoLinesFCT(age,
                  as.numeric(coefficients(m.Final)[1]), 
                  0, 
                  as.numeric(coefficients(m.Final)[2]), 
                  as.numeric(coefficients(m.Final)[3]))
    }
    
  } else {
    TwoLines <- function(age) {
      TwoLinesFCT(age,
                  as.numeric(coefficients(m.Final)[1]), 
                  as.numeric(coefficients(m.Final)[2]), 
                  as.numeric(coefficients(m.Final)[3]), 
                  as.numeric(coefficients(m.Final)[4]))
    }
  }
    PlotLine <- stat_function(fun = TwoLines, color="red")
  }
  
  # plot
  p3 <- ggplot(df_All, aes(Age, uRevs, color=Group)) + geom_point() + ylim(yMin, yMax)
  p3 <- p3 + PlotLine + labs(title = paste0(CondCodeName, " - all subjects")) + 
    theme(legend.position = "bottom")
  
  # get all plots together:
  library(cowplot)
  p <- plot_grid(p1,p2,p3, ncol=1, nrow = 3) 
  # --------------------------------------------------------------------- #
  # gather the data frames into a list for the function output
  MyList <- list(df_All, m1.linear, m1.TwoLines, ComprMdls1, preFinalModel,
                 m2.linear, m2.TwoLines, postFinalModel, ComprMdls2,p,df_TD_trimmed)
  return(MyList)
}
