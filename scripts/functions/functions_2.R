# R script with a set of functions for analysis RQ3 and RQ4

####################
# Create databases #
####################

#Arguments: original ddbb, ddbb with QT (LF), number of repetitions, size of sample
create.ddbb <- function(case_ddbb, ddbb_QT, n_repetitions, n_sample){
  
  #Dummy variable (value = 1 for QT tweets)
  ddbb_QT$QT <- rep(1, dim(ddbb_QT)[1])
  
  # Fixing the seed for duplication
  set.seed(1111)
  
  #Creating samples of QT according the number of repetitions
  ddbb_QT <- replicate(n_repetitions, ddbb_QT[sample(nrow(ddbb_QT), size = n_sample), ], simplify = FALSE)
  
  #Database with retweets
  case_ddbb_RT <- case_ddbb[which(case_ddbb$referenced_tweets_type == 'retweeted'),]
  
  
  #Duplicating necessary columns
  case_ddbb_RT$qt_id <- case_ddbb_RT$id
  case_ddbb_RT$qt_text <- case_ddbb_RT$text
  case_ddbb_RT$sent_score_qt <- case_ddbb_RT$sent_score
  case_ddbb_RT$sent_label_qt <- case_ddbb_RT$sent_label
  case_ddbb_RT$sent_language_qt <- case_ddbb_RT$lang
  case_ddbb_RT$sadness_qt <- case_ddbb_RT$sadness
  case_ddbb_RT$joy_qt <- case_ddbb_RT$joy
  case_ddbb_RT$fear_qt <- case_ddbb_RT$fear
  case_ddbb_RT$disgust_qt <- case_ddbb_RT$disgust
  case_ddbb_RT$anger_qt <- case_ddbb_RT$anger
  case_ddbb_RT$QT <- rep(0, dim(case_ddbb_RT)[1]) #Dummy variable (value = 0 for RT tweets)
  
  #Creating samples of RT according the number of repetitions
  ddbb_RT <- replicate(repetitions, case_ddbb_RT[sample(nrow(case_ddbb_RT), size = n_sample), ], simplify = FALSE)
  
  
  #Creation of databases with pre and post emotions for the analysis
  ddbb.result <- list()
  
  for (i in 1:repetitions){
    
    ddbb.result[[i]] <- rbind(ddbb_QT[[i]], ddbb_RT[[i]])
    
  }
  
  return(ddbb.result)
  # returns the data base for analysis with LB and LF
}

###############################
# Plot for pre/post LF and LB #
###############################

#Arguments: ddbb from create.ddbb, number of repetitions, plot title
create.QT_RT.plot <- function(db_QTRT, n_repetitions, title = NULL){
  
  db_All_QT <- list()
  db_All_RT <- list()
  
  pre_post_qt <- data.frame(matrix(ncol = 10, nrow = n_repetitions))
  pre_post_rt <- data.frame(matrix(ncol = 10, nrow = n_repetitions))
  
  for (i in 1:n_repetitions){
    
    db_All_QT[[i]] <- db_QTRT[[i]][db_QTRT[[i]]$QT==1, ]
    db_All_RT[[i]] <- db_QTRT[[i]][db_QTRT[[i]]$QT==0, ]
    
    sum_row <- colMeans(db_All_QT[[i]][,c(20:24,10:14)]) #in dataframe quoted text come in later rows
    pre_post_qt[i,] <- sum_row
    
    sum_row1 <- colMeans(db_All_RT[[i]][,c(20:24,10:14)]) #in dataframe quoted text come in later rows
    pre_post_rt[i,] <- sum_row1

  }
  
  pre_post_QT <- data.frame(rbind(colMeans(pre_post_qt[,1:5]),
                                  colMeans(pre_post_qt[,6:10]),
                                  colMeans(pre_post_rt[,1:5]))) #attention to the order
  
  colnames(pre_post_QT) <- colnames(db_QTRT[[1]][10:14])
  
  pre_post_QT$time <- c("pre LF","post LF", "LB") #attention to the order
  
  #libraries to graph the distribution of p-values and standard estimates
  library(reshape2)
  library(ggplot2)
  
  # melt the data frame for plotting
  pre_post_QT.m <- melt(pre_post_QT, id.vars='time')
  
  #Definition of the order to plot stages and emotions
  pre_post_QT.m$variable <- factor(pre_post_QT.m$variable, 
                                   levels=c('fear', 'anger', 'disgust', 'sadness', 'joy'))
  
  pre_post_QT.m$time <- factor(pre_post_QT.m$time, 
                               levels=c("pre LF","post LF", "LB")) #attention to the order
  
  QT_RT.plot <- ggplot(pre_post_QT.m, aes(variable, value)) +
    geom_bar(aes(fill = time), position = "dodge", stat="identity") +
    scale_fill_grey(start = .25, end = .5) +
    guides(fill = guide_legend(title = "Tweets")) +
    xlab("") + 
    ggtitle(title) +
    theme_bw()
  
  #Dataframe with comparisons pre QT-RT and post QT-RT 
  dif_QT_RT <- data.frame(t(pre_post_QT[ ,1:5]))
  colnames(dif_QT_RT) <- pre_post_QT[,6]
  dif_QT_RT[,'|pre LF - LB|'] <- abs(dif_QT_RT[,"pre LF"] - dif_QT_RT[,"LB"])
  dif_QT_RT[,'|post LF - LB|'] <- abs(dif_QT_RT[,"post LF"] - dif_QT_RT[,"LB"])
  dif_QT_RT[,'smaller'] <- dif_QT_RT[,'|post LF - LB|'] < dif_QT_RT[,'|pre LF - LB|']
  
  list.result <- list("QT_RT.plot" = QT_RT.plot,
                      "dif_pre_post_Table" = dif_QT_RT)
  
  return(list.result)
  #returns: pre/post LF and LB plot, table with the differences pre/post LF and LB
  
}

##############
# Model fit #
#############

#Arguments: ddbb from create.ddbb, specified model, number of repetitions
modelfit.list <- function(ddbb_case, model, n_repetitions){
  
  #List to populate with the model fitted for each repetition
  model_fit.result <- list()
  
  #Model fit for each repetition
  for (i in 1:n_repetitions){
    
    model_fit.result[[i]] <- sem(model, data = ddbb_case[[i]])

  }
  
  return(model_fit.result)
  #returns fitted model for all the repetitions
}


################################################
# p-values and standard estimates distribution #
################################################

#Arguments: list of fitted models from modelfit.list, number of repetitions
distribution_p_est.std <- function (model_fit_list, n_repetitions){
  
  #Obtaining the distribution of p-values and standard estimates
  pvalue_dist <- data.frame(matrix(ncol = 36, nrow = n_repetitions))
  est.std_dist <- data.frame(matrix(ncol = 36, nrow = n_repetitions))
  
  for (i in 1:n_repetitions){
    
    pvalue_row <- standardizedSolution(model_fit_list[[i]])$pvalue[c(1:10,42:66,11)]
    pvalue_dist[i,] <- pvalue_row
    
    est.std_row <- standardizedSolution(model_fit_list[[i]])$est.std[c(1:10,42:66,11)]
    est.std_dist[i,] <- est.std_row
  }
  
  #Names of the effects of each estimate for the distribution of p-values and standard estimates
  cause <- c('fear -> M', 'ang -> M', 'dis -> M', 'sad -> M', 'joy -> M', 
             'M -> fear', 'M -> ang', 'M -> dis', 'M -> sad', 'M -> joy',
             'fear -> fear', 'fear -> ang', 'fear -> dis', 'fear -> sad', 'fear -> joy',
             'ang -> fear', 'ang -> ang', 'ang -> dis', 'ang -> sad', 'ang -> joy',
             'dis -> fear', 'dis -> ang', 'dis -> dis', 'dis -> sad', 'dis -> joy',
             'sad -> fear', 'sad -> ang', 'sad -> dis', 'sad -> sad', 'sad -> joy',
             'joy -> fear', 'joy -> ang', 'joy -> dis', 'joy -> sad', 'joy -> joy',
             'M~~M')
  
  #Assigning names
  names(pvalue_dist) <- cause
  names(est.std_dist) <- cause
  
  list.result <- list("dist_pvalue" = pvalue_dist,
                      "dist_est.std" = est.std_dist)
  
  return(list.result)
  #returns: list of p-values for each cause-effect estimation,
  #list of standard estimates for each cause-effect estimation.
}


#Arguments: list of standard estimates from list.result, list of p-values from list.result
distribution_p_est.std.plot <- function (est.std_dist, pvalue_dist){
  
  #libraries to graph the distribution of p-values and standard estimates
  library(reshape2)
  library(ggplot2)
  
  melt_pvalue <- melt(pvalue_dist[,1:35])
  melt_est.std <- melt(est.std_dist[,1:35])
  
  #Graph of the distribution of p-values for each effect
  pvalue.plot <- ggplot(melt_pvalue,aes(x = value)) +
    facet_wrap(~variable, ncol = 5) + 
    geom_histogram()
  
  #Graph of the distribution of standard estimates for each effect
  est.std.plot <- ggplot(melt_est.std,aes(x = value)) +
    facet_wrap(~variable, ncol = 5) + 
    geom_histogram()
  
  list.result <- list("dist_pvalue.plot" = pvalue.plot, 
                      "dist_est.std.plot" = est.std.plot)
  
  return(list.result)
  #returns plot of p-value disttribution, plot of standard estimates distribution
}


########################
# Model estimates plot #
########################

#Arguments: standard estimates distribution, p-values distributions, use sign level TREU/FALSE,
#level of signicance, plot title
create.model_estimates.plot <- function (est.std_dist, pvalue_dist, significant = FALSE, 
                                         sig_level=.05, title = NULL){

  #Median of each standard estimates and TRUE/FALSE values if mean(p-values) <= sig_level
  est.std_All <- data.frame(cbind(colMeans(est.std_dist),
                                  colMeans(pvalue_dist) <= sig_level))
  
  #Assigning the name to the previous dataframe
  colnames(est.std_All) <- c('est_std', 'p_value')
  
  #List of estimates with cause-effect based on front-door criterion
  std_aux <- est.std_All[11:35, ]
  
  #Replace the value of estimates with 0 if p-values are non significant
  if (significant == TRUE){
    
    std_aux$est_std[std_aux$p_value == 0] <- 0  #Replace with 0 non-significant values
    
  }
  
  #Separate the effects of each pre-LF emotion on the five post-LF emotions
  model_std <- data.frame(rbind(std_aux$est_std[1:5], std_aux$est_std[6:10],
                                std_aux$est_std[11:15], std_aux$est_std[16:20],
                                std_aux$est_std[21:25]))
  
  colnames(model_std) <- c("fear","anger","disgust","sadness", "joy")
  
  model_std$Emotions <- c("fear","anger","disgust","sadness", "joy")
  
  # melt the data frame for plotting
  library(reshape2)
  model_std.m <- melt(model_std, id.vars='Emotions')
  
  #Definition of the order to plot stages and emotions
  model_std.m$Emotions <- factor(model_std.m$Emotions, levels=c('fear', 'anger', 'disgust', 'sadness', 'joy'))
  
  model_estimates.plot <- ggplot(model_std.m, aes(Emotions, value)) +   
    geom_bar(aes(fill = variable), position = "dodge", stat="identity") +
    scale_fill_grey(start = .4, end = .8) +
    ylim(-0.12, 0.16) +
    guides(fill = guide_legend(title = "Emotional response")) +
    xlab("") +
    ylab("Estimate") +
    ggtitle(title) +
    theme_bw()

  return(model_estimates.plot)
  #returns plot with model significant model estimates base on signicance level
}
