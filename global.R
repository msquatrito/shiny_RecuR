library(shiny)
library(weights)
library(gridExtra)
library(survival)
library(tidyr)
# library(DT)
# library(Cairo)
# library(reshape2)
# library(RColorBrewer)
# library(scales)
library(shinythemes)
library(tidyverse)

options(shiny.usecairo=TRUE)
`%then%` <- shiny:::`%OR%`


#######################################
############## Datasets  ##############
#######################################
datasets <- readRDS("data/datasets.Rds")

#######################################
########## other variables  ###########
#######################################
gene_names <- readRDS("data/gene_names.Rds")

################################################
##############  Remove NA column  ##############
################################################
rmNA <- function (df) {
  df <- df[,colSums(is.na(df)) < nrow(df)]
}

###################
## Survival plot ##
###################
get_cutoff <- function(mRNA, cutoff, numeric) {
  mRNA.q <- round(quantile(mRNA, probs=c(0.25, 0.5, 0.75), na.rm = TRUE),2)
  if (cutoff == "quartiles"){
    strat <- cut(mRNA, quantile(mRNA,na.rm = T), include.lowest = TRUE)
  }
  if (cutoff != "quartiles") {
    if (cutoff == "high vs low") {
      strat <- ifelse(mRNA >= mRNA.q [3], "high", ifelse(mRNA <= mRNA.q [1], "low",NA))
    } else {
      cut <- switch(cutoff, 
                    "median" = mRNA.q[2],
                    "lower quartile" = mRNA.q [1],
                    "upper quartile" = mRNA.q [3],
                    "Use a specific mRNA value" = numeric)
      f <- function(x) ifelse(x >= cut, c("high"),c("low"))
      strat <- f(mRNA)
    }
  }
  strat
}

survivalPlot <- function (df, surv_type, gene, group, subtype, cutoff, numeric, cex = 1.2) {
  # Select a specific Histology
  if (group != "All") {
    df <- filter(df, Pathology == group)
  }
  # Select a specific subtype
  if (subtype != "All") {
    df <- filter(df, Subtype == subtype)
  } 
  if(cutoff == "Use a specific mRNA value") {
    main <- paste0("Histology: ", group, 
                   "; Subtype: ", subtype,
                   "; Cutoff: ", round(numeric, 2)) 
  } else {
    main <- paste0("Histology: ", group, 
                   "; Subtype: ", subtype,
                   "; Cutoff: ", cutoff)
  }
  
  mRNA <- df[ ,"mRNA"]
  surv.status <- df[ ,"CensorN"]
  if(surv_type == "Overall"){
    surv.time <- df[ ,"OS"]
  } else if (surv_type == "Progression free"){
    surv.time <- df[ ,"PFS"]
  }
  surv.time <- round(0.0328767*surv.time,2)
  my.Surv <- Surv(time = surv.time, event = surv.status== 1)
  smax <- max(surv.time, na.rm = TRUE)
  tmax <- smax-(25*smax)/100
  xmax <- (95*tmax)/100
  strat <- get_cutoff(mRNA, cutoff, numeric)
  expr.surv <- survfit(my.Surv ~ strat, conf.type = "none")
  log.rank <- survdiff(my.Surv ~ strat, rho = 0)
  mantle.cox <- survdiff(my.Surv~ strat, rho = 1)
  surv <- data.frame(summary(expr.surv)$table)
  model <- summary(coxph(my.Surv ~ strat))
  HR <- round(model$conf.int[1],2)
  HR.lower <- round(model$conf.int[3],2)
  HR.upper <- round(model$conf.int[4],2)
  log.rank.p <- round(1 - pchisq(log.rank$chi, df = 1), 4)
  mantle.cox.p <- round(1 - pchisq(mantle.cox$chi, df = 1), 4)
  star.log <- starmaker(log.rank.p)
  star.mcox <- starmaker(mantle.cox.p)
  plot(expr.surv, xlab = "Survival time (months)", ylab = "Survival Fraction", yscale = 1, xlim = c(0,smax),
       main = main, col = c("red", "blue"), mark.time = FALSE, cex.axis = cex, cex.lab = cex)
  legend("topright", legend = c(sprintf("%s High, (n=%s, events=%s, median=%s)", gene, surv$records[1], surv$events[1], surv$median[1]), 
                                sprintf("%s Low, (n=%s, events=%s, median=%s)", gene, surv$records[2], surv$events[2], surv$median[2])),
         col= c("red", "blue"), lty = 1, cex = cex)
  graphics::text(xmax, 0.725, sprintf("HR = %s, (%s - %s)",HR, HR.lower, HR.upper), cex = cex)
  graphics::text(xmax, 0.65, sprintf("%s Log-rank p value= %s", star.log, log.rank.p), cex = cex)
  graphics::text(xmax, 0.575, sprintf("%s Wilcoxon p value= %s",star.mcox, mantle.cox.p), cex = cex)
  
  if (cutoff == "quartiles"){
    expr.surv <- survfit(my.Surv ~ strata(strat), conf.type="none")
    z <- data.frame(summary(expr.surv)$table) 
    plot(expr.surv, xlab="Survival time (months)", ylab="Survival Fraction", yscale = 1, xlim = c(0,smax), 
         main = main, col= c(1:4), mark.time=FALSE, cex.axis = cex, cex.lab = cex)
    legend("topright", title = "Quantile", 
           legend = c(sprintf("1st (n=%s, median=%s)", z$records[1], z$median[1]),
                      sprintf("2nd (n=%s, median=%s)", z$records[2], z$median[2]),
                      sprintf("3rd (n=%s, median=%s)", z$records[3], z$median[3]),
                      sprintf("4th (n=%s, median=%s)", z$records[4], z$median[4])),
           col= c(1:4), lty=1, cex = cex)
  } 
}
