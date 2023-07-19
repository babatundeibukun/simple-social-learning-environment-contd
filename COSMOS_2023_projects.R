########################################################################################################
#INSTALLING NECCESSARY PACKAGES 
#######################################################################################################
packages <- c('tidyverse', "vembedr", 'formatR')
#invisible(lapply(packages, install.packages, character.only = TRUE)) #Install packages if any are missing
invisible(lapply(packages, require, character.only = TRUE)) #Load all packages
knitr::opts_chunk$set(warning = FALSE, message = FALSE) 


#install.packages("ggplot2")  # Install the ggplot2 package (only need to run this once)
library(ggplot2)             # Load the ggplot2 package
library(dplyr)
######################################################################################
#THIS IS FOR THE GEOM_FLAT_VIOLIN , it helps generate a normal distribution curve
#beside the normal distribution
##################################################################################

"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}

geom_flat_violin <- function(mapping = NULL, data = NULL, stat = "ydensity",
                             position = "dodge", trim = TRUE, scale = "area",
                             show.legend = NA, inherit.aes = TRUE, ...) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomFlatViolin,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      trim = trim,
      scale = scale,
      ...
    )
  )
}


GeomFlatViolin <-
  ggproto(
    "GeomFlatViolin",
    Geom,
    setup_data = function(data, params) {
      data$width <- data$width %||%
        params$width %||% (resolution(data$x, FALSE) * 0.9)
      
      # ymin, ymax, xmin, and xmax define the bounding rectangle for each group
      data %>%
        dplyr::group_by(.data = ., group) %>%
        dplyr::mutate(
          .data = .,
          ymin = min(y),
          ymax = max(y),
          xmin = x,
          xmax = x + width / 2
        )
    },
    
    draw_group = function(data, panel_scales, coord)
    {
      # Find the points for the line to go all the way around
      data <- base::transform(data,
                              xminv = x,
                              xmaxv = x + violinwidth * (xmax - x))
      
      # Make sure it's sorted properly to draw the outline
      newdata <-
        base::rbind(
          dplyr::arrange(.data = base::transform(data, x = xminv), y),
          dplyr::arrange(.data = base::transform(data, x = xmaxv), -y)
        )
      
      # Close the polygon: set first and last point the same
      # Needed for coord_polar and such
      newdata <- rbind(newdata, newdata[1,])
      
      ggplot2:::ggname("geom_flat_violin",
                       GeomPolygon$draw_panel(newdata, panel_scales, coord))
    },
    
    draw_key = draw_key_polygon,
    
    default_aes = ggplot2::aes(
      weight = 1,
      colour = "grey20",
      fill = "white",
      size = 0.5,
      alpha = NA,
      linetype = "solid"
    ),
    
    required_aes = c("x", "y")
  )

###############################################################################################
#the bandit generator for generating payoffs
###############################################################################################


#generative model of the bandit task 
set.seed(123) #set seed for reproducibility
K = 2 #number of options
meanVec <- runif(n=K, min = -10, max = 10) #Payoff means, which we sample from uniform distribution between -10 to 10
sigmaVec <- rep(1, K) #Variability of payoffs (as a standard deviation), which we assume is the same for all options. 

banditGenerator <- function(k) {#k is an integer or k vector of integers, selecting one of the 1:K arms of the bandit
  payoff <- rnorm(n = length(k), mean = meanVec[k], sd = sigmaVec[k])
  return (payoff)
}

actionSequence <- sample(1:K, size = 25, replace = TRUE)# select 25 random actions
actionSequence
payoffs <- banditGenerator(actionSequence)

df <- data.frame(action = actionSequence, payoff = payoffs)
knitr::kable(df)


############################################################################################
#Let’s now visualize the environment, where we use a large number of samples (n=10000)
#to paint an accurate picture of the true generative reward structure (vertical density plot),
#and plot that against a small number (n=25) of sampled payoffs (dots):
#######################################################################################




sampleDF <- data.frame()
nsamples = 10000 #Samples used to approximate the normal distribution
plotsamples = 25 #samples to plot as dots

rewardSamples <- c(sapply(1:K, FUN=function(k) banditGenerator(rep(k, nsamples))))#use an apply function to simulate multiple samples of each arm
sampleDF <- data.frame(option = rep(1:K, each = nsamples), payoff=rewardSamples, nsample = rep(1:nsamples, K)) #create a dataframe for plotting

ggplot(sampleDF, aes(x = factor(option), y = payoff, fill = factor(option)))+
  geom_flat_violin(position = position_nudge(x = 0.3, y = 0), alpha = .5) +
  geom_jitter(data = subset(sampleDF, nsample<=plotsamples), aes(y = payoff, color = factor(option)),width = 0.2 )+  #Plot only a subset of points show how well a limited number of samples approximates the true distribution
  ylab('Payoff')+
  xlab('Option')+
  theme_classic() +
  scale_fill_viridis_d()+
  scale_color_viridis_d()+
  ggtitle('Payoff conditions')+
  theme(legend.position ='none')





#To you modify the payoff generating function to describe other tasks? 
#Sometimes rewards are binary, such as with success or failure outcomes
#when publishing a paper or hunting for food. With binary reward structures,
#you can use Bernoulli distribution in place of a Gaussian distribution. 
#The code snippet below contains a brief example.

K <- 10
successVec = runif(K) #different success probabilities for each option sampled from a uniform distribution between 0 and 1
binaryGenerator <- function(k) {#k is an integer or k vector of integers, selecting one of the 1:K arms of the bandit
  payoff <- rbernoulli(n = length(k), p=successVec[k]) #a Bernoulli distribution can be used to describe a binary reward structure
  return (payoff)
}
binaryGenerator(sample(1:10, 25, replace = TRUE)) 