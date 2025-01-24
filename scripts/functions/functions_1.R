# R script with a set of functions for analysis RQ1 and RQ2

####################################
# Plot for distribution of valence #
####################################

#Arguments: original ddbb, plot title
create.valence.plot <- function(case_ddbb, plot_title=NULL){
  
  #libraries to graph valence plot
  library(ggplot2)
  library(dplyr)
  
  valence.plot <- ggplot() +
    geom_histogram(data = case_ddbb %>% filter(referenced_tweets_type == "quoted"),
                   aes(x = sent_score, y = ..density.. , fill = referenced_tweets_type),
                   color = "black", binwidth = .1) +
    geom_histogram(data = case_ddbb %>% filter(referenced_tweets_type == "retweeted"),
                   aes(x = sent_score, y = -..density.. , fill = referenced_tweets_type),
                   color = "black", binwidth = .1) +
    coord_cartesian(xlim=c(-1.0,1.0), ylim=c(-5.0,5.0)) +
    guides(fill=guide_legend(title="Lean type")) +
    scale_fill_grey(start = .5, end = .8, breaks=c('quoted', 'retweeted'), 
                    labels=c('LF (quote)', 'LB (retweet)')) +
    theme_bw() +
    labs(x = "valence") +
    ggtitle(plot_title)
  
  return(valence.plot)
  
}

#####################################
# Plot for distribution of emotions #
#####################################

#Arguments: original ddbb, type of emotion, plot title
create.emotion.plot <- function(case_ddbb, emotion="emotion", plot_title=NULL){
  
  #libraries to graph emotions plot
  library(ggplot2)
  library(dplyr)
  
  emotion.plot <- ggplot() +
    geom_histogram(data = case_ddbb %>% filter(referenced_tweets_type == "quoted"),
                   aes_string(x = emotion, y = "..density.." , fill = "referenced_tweets_type"),
                   color = "black", binwidth = .1) +
    geom_histogram(data = case_ddbb %>% filter(referenced_tweets_type == "retweeted"),
                   aes_string(x = emotion, y = "-..density.." , fill = "referenced_tweets_type"),
                   color = "black", binwidth = .1) +
    coord_cartesian(xlim=c(0,1.0), ylim=c(-7.0,7.0)) +
    guides(fill=guide_legend(title="Lean type")) +
    scale_fill_grey(start = .5, end = .8, breaks=c('quoted', 'retweeted'), 
                    labels=c('LF (quote)', 'LB (retweet)')) +
    theme_bw() +
    labs(x = "emotion level") +
    ggtitle(plot_title)
  
  return(emotion.plot)
  
}

###################################
# Table and plot for effect sizes #
###################################

#Arguments: original ddbb, plot title
create.effectsize.tableplot <- function(case_ddbb, plot_title = NULL){
  
  #Calculate the effect size Cohen's d for emotions using "effectsize" package
  cn_fe <- effectsize::cohens_d(case_ddbb[which(case_ddbb$referenced_tweets_type != 'retweeted'), 'fear'],
                                case_ddbb[which(case_ddbb$referenced_tweets_type == 'retweeted'), 'fear'])
  cn_an <- effectsize::cohens_d(case_ddbb[which(case_ddbb$referenced_tweets_type != 'retweeted'), 'anger'],
                                case_ddbb[which(case_ddbb$referenced_tweets_type == 'retweeted'), 'anger'])
  cn_di <- effectsize::cohens_d(case_ddbb[which(case_ddbb$referenced_tweets_type != 'retweeted'), 'disgust'],
                                case_ddbb[which(case_ddbb$referenced_tweets_type == 'retweeted'), 'disgust'])
  cn_sa <- effectsize::cohens_d(case_ddbb[which(case_ddbb$referenced_tweets_type != 'retweeted'), 'sadness'],
                                case_ddbb[which(case_ddbb$referenced_tweets_type == 'retweeted'), 'sadness'])
  cn_jo <- effectsize::cohens_d(case_ddbb[which(case_ddbb$referenced_tweets_type != 'retweeted'), 'joy'],
                                case_ddbb[which(case_ddbb$referenced_tweets_type == 'retweeted'), 'joy'])
  
  
  #Summary of effect sizes and confidence intervals
  cn <- c(cn_fe$Cohens_d, cn_an$Cohens_d, cn_di$Cohens_d, #order of emotions
          cn_sa$Cohens_d, cn_jo$Cohens_d)
  
  cn_low <- c(cn_fe$CI_low, cn_an$CI_low, cn_di$CI_low, #order of emotions
              cn_sa$CI_low, cn_jo$CI_low)
  
  cn_high <- c(cn_fe$CI_high, cn_an$CI_high, cn_di$CI_high, #order of emotions
               cn_sa$CI_high, cn_jo$CI_high)
  
  emotions <- c('fear', 'anger', 'disgust', 'sadness', 'joy')
  
  #Dataframe with the summary of emotions
  cohens.result <- data.frame(cbind(emotions,cn, cn_low, cn_high))
  
  names(cohens.result) <- c("Emotions", "Cohen's d", "CI low", "CI high")
  
  #Variable to change plot color if positive or negative effect size
  pos_neg <- cn > 0
  
  #Auxiliar dataframe to create plot
  em_cn <- data.frame(emotions, cn, pos_neg, cn_low, cn_high)
  em_cn$cn <- round(em_cn$cn, digits = 2) #rounding values for plotting
  em_cn$cn_low <- round(em_cn$cn_low, digits = 2)
  em_cn$cn_high <- round(em_cn$cn_high, digits = 2)
  
  #Definition of the order to plot emotions
  em_cn$emotions <- factor(em_cn$emotions, levels=c('fear', 'anger', 'disgust', 'sadness', 'joy'))
  
  efsz <- ggplot(em_cn, aes(x = cn, y = emotions, color = pos_neg, label = cn)) +
    #geom_segment(aes(x = 0, xend = cn, yend = emotions), lty=2) +
    geom_point(shape=18, size=2) +
    geom_errorbar(aes(xmin=cn_low, xmax=cn_high), width=.2, position=position_dodge(0.05)) +
    geom_vline(xintercept = 0) +
    geom_text(nudge_y = 0.2, size = 3) +
    guides(color='none') +
    scale_colour_manual(values = c('gray60', 'black')) +
    xlim(-0.8, 0.8) +
    theme_bw() + 
    labs(x = expression(paste("Cohen's ", italic("d ")))) +
    ggtitle(plot_title)
  #ggtitle(expression(paste("Cohen's ", italic("d "), "emotions JB")))
  
  list.result <- list("effectsize.table" = cohens.result,
                      "effectsize.plot" = efsz)
  
  return(list.result)
}