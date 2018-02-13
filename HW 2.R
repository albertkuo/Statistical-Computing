Factorial <- function(n) {
  if(!is.numeric(n)){
    stop("Not an integer")
  } else if (length(n)>1){
    stop("Not an integer")
  } else if (n%%1!=0){
    stop("Not an integer")
  } else if(n<0){
    return(NaN)
  } else if(n==0){
    return(1)
  } else{
    x = seq(1, n)
    return(prod(x))
  }
}

# Part 2
library(readr)
library(dplyr)
library(ggplot2)
mie <- read_csv("~/Documents/Grad School/Computing/MIE.zip", col_types = "cicdi")
summary(mie)

mie %>% group_by(id, room) %>%
  filter(row_number()==1) %>%
  group_by(id) %>%
  mutate(count = n()) %>%
  arrange(-count)

plot_id = function(id_char){
  if(!id_char %in% mie$id){
    print("ID not valid")
    stop()
  }
  else {
    mie.sub = filter(mie, id==id_char)
    ggplot(mie.sub, aes(x=timepoint, y=value)) + 
      geom_point(alpha=0.4, color="black", fill="white", 
                 shape=21, size=0.3) + 
      facet_grid(room~visit, labeller=label_both) + 
      theme_bw()
  }
}

# Part 3
source("~/Documents/Grad School/Computing/median_testdata.R")
bootstrap_median_CI = function(x){
  # Check input
  if(!is.vector(x)){
    stop("x is not a vector")
  } else if(!is.numeric(x)){
    stop("x not numeric")
  } else if(any(is.infinite(x)) | anyNA(x)){
    stop("x not a valid numeric input")
  }

  # Bootstrap median
  n = 1000
  trials = 1:n
  medians = sapply(trials, function(i){return(median(sample(x, replace=T)))})
  lower = quantile(medians, 0.025)
  upper = quantile(medians, 0.975)
  return(c(lower, upper))
}

bootstrap_median_CI(x1)
bootstrap_median_CI(x2)
bootstrap_median_CI(x3)
bootstrap_median_CI(x4)
bootstrap_median_CI(x5)
