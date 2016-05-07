
sim_one_person <- function(prob, a=10)
{  
  if (a > sum(prob > 0))
    stop("the number of attributes 'a' must not exceed the number of categories, i.e. length of 'prob'")
  d.all <- NULL
  for (i in 1:a){
    prob <- prob / sum(prob)
    d <- rmultinom(1, size = 1, prob=prob) 
    d.index <- which(d == 1) 
    prob[d.index] <- 0  
    d.all <- cbind(d.all, d) 
  }
  apply(d.all, 1, sum)    
} 
# sim_one_person(dexp(1:30, rate=.05), a=10)

# a can be a number of a function (if wrapped in quote) 
# returning a number
sim_n_persons <- function(prob, n, a=10, ap=rep(1/length(a), length(a)))
{ 
  if (length(a) != length(ap))
    stop("the number of attributes must match the attribute probabilites")
  ap <- ap/sum(ap)  # make sure sum of ap equals one (probability)   
  sim <- replicate(n, sim_one_person(prob, a=sample_new(a, 1, prob=ap)) )
  apply(sim, 1, sum) 
}
# sim_n_persons(dexp(1:30, .05), n=2, a=10)
# sim_n_persons(dexp(1:30, .05), n=2, a=c(1, 30))
# sim_n_persons(dexp(1:30, .05), n=2, a=c(1, 30), ap=c(1,4))
# sim_n_persons(dexp(1:30, .05), n=2, a=1:5, ap=c(1,1,2,2,3))
#   


draw_n_person_sample <- function(prob, n, a=10, ap=rep(1/length(a), length(a))){
  res <- sim_n_persons(prob, n, a, ap)
  df <- data.frame(no=seq_along(res), cat=res)
  g <- ggplot(df, aes(x=no, y=cat)) + geom_line() + geom_point() +
              scale_y_continuous(limits = c(0, max(res)))  
  print(g)
}
#draw_n_person_sample(dexp(1:30, rate=.05), 100, a=10)
#draw_n_person_sample(dexp(1:30, rate=.05), 100, a=1:5, ap=5:1) 
#    

sim_n_persons_x_times <- function(prob, n, a, ap=rep(1/length(a), 
                                  length(a)), times=100, progress="text"){   
  ldply(1L:times, function(x, prob, n, a, ap){
          sim_n_persons(prob, n, a, ap)
        }, prob=prob, n=n, a=a, ap=ap, .progress=progress)
}   

# sim_n_persons_x_times(dexp(1:30, .05), n=2, a=c(1,30), ap=1:2, times=100)  
# sim_n_persons_x_times(dexp(1:30, .05), n=2, a=c(1,30), ti=1000, progress="tk" ) 
#   

expected_frequencies <- function(r){  
  co <- t(apply(r, 2, quantile, probs=c(.05, .25, .5, .75, .95)))
  df <- cbind(cat=1L:nrow(co), as.data.frame(co)) 
  df.melted <- melt(df, id.vars="cat")
  df.melted$variable <- as.factor(df.melted$variable) 
  mval <- max(df.melted$value)   
  g <- ggplot(subset(df.melted, variable != "50%"), 
              aes(x=cat, y=value, group=variable, shape=variable)) +  
              geom_line() + geom_point() +
              geom_line(data=subset(df.melted, variable=="50%"), col="blue") +
              geom_point(data=subset(df.melted, variable=="50%"), col="blue") +
              scale_y_continuous("Probability", lim=c(0, mval))  
  g
}                     
# r <- sim_n_persons_x_times(dexp(1:30, rate=.05), n=50, a=5:7, ap=1:3, 100)  
# expected_frequencies(r)      



# Wahrscheinlichkeit, dass mindestens prop Prozent der Kategorien mindestens  
# m mal gennant wurden. 
prob_categories <- function(r, m, min.prop=1){
  s <- apply(r, 1, function(x, min.prop){    # does the sample render more than 
    (sum(x >= m) / length(x)) >= min.prop    # min.prop categories with >=  m attributes                                                                                     
  }, min.prop=min.prop)  
  sum(s) / nrow(r)
} 

# r <- sim_n_persons_x_times(dexp(1:30, rate=.05), n=50, a=5:7, times=100)
# prob_categories(r, 4, min.prop=.9)   


# Wie ist die Wkt bei der gegebenen Verteilung für verschiedene m und verschiedene
# min.prop
sim_n_persons_x_times_many_n <- function(prob, n=seq(10, 80, by=10), a=7, 
                               ap=rep(1/length(a), length(a)), times=100,
                               progress="text"){
  r <- list()
  for (i in seq_along(n))
    r[[i]] <- sim_n_persons_x_times(prob, n=n[i], a=a, ap=ap, times=times, 
                                    progress=progress)
  r
}

# r <- sim_n_persons_x_times_many_n(dexp(1:30, .05), a=7, times=100)
# r <- sim_n_persons_x_times_many_n(dexp(1:30, .05), a=5:7, ap=1:3, times=100, prog="tk")
#  

calc_probabilities <- function(r, n, ms, min.props=c(.9, .95, .99)){  
  res <- NULL
  for (m in ms){
    for (min.prop in min.props){  
      probs <- sapply(r, prob_categories, m=m, min=min.prop)
      res.df <- cbind(n=n, m=m, min.prop=min.prop, prob=probs)
      res <- append(res, list(res.df))    
    }
  }
  dd <- as.data.frame(do.call(rbind, res))  
  dd
}

# prob <-  dexp(1:30, .05) 
# n <- seq(10, 80, by=20)  
# r <- sim_n_persons_x_times_many_n(prob, n, a=7, times=100)
# dd <- calc_probabilities(r, n, ms=1:5, min.props=c(0.9, .95, 1))
# 
# dd$m <- as.factor(dd$m) 
# g <- ggplot(dd, aes(x=n, y=prob, group=m, shape=m)) +  geom_line() + geom_point() +
#        scale_y_continuous("Probability", lim=c(0,1)) + 
#        scale_x_continuous("Sample size N") + facet_grid(. ~ min.prop)
# print(g)  



draw_multiple_n_persons_x_times <- function(prob, n=seq(10, 80, by=10), a=7, 
                               ap=rep(1/length(a), length(a)), times=100,
                               progress="text", ms, min.props=c(.9, .95, .99))
{
 res <- sim_n_persons_x_times_many_n(prob=prob, n=n, a=a, ap=ap, 
                                     times=times, progress=progress)  
 dd <- calc_probabilities(r=res, n=n, ms=ms, min.props=min.props)
 dd$m <- as.factor(dd$m) 
  g <- ggplot(dd, aes(x=n, y=prob, group=m, shape=m)) +  geom_line() + 
         geom_point() +
         scale_y_continuous("Probability", lim=c(0,1)) + 
         scale_x_continuous("Sample size N") + facet_grid(. ~ min.prop)
  g  
}

# g <- draw_multiple_n_persons_x_times(dexp(1:30, .05), n=seq(10, 80, by=10),
#                                       a=7, ms=1:5, min.prop=.95) 
# g  

   



