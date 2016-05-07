
gset <- RGtk2::gObjectSetData
gget <- RGtk2::gObjectGetData

# sapply_pb <- function(X, FUN, ..., progress=TRUE)
# {
#   if (progress){
#     env <- environment()                                      # this environment
#     pb_Total <- length(X)                                     # get max value for progress bar
#     counter <- 0                                              # make counter variable
#     pb <- txtProgressBar(min = 0, max = pb_Total, style = 3)  # make progress bar
#     
#     # wrapper around FUN
#     wrapper <- function(...){
#       curVal <- get("counter", envir = env)                   # get counter value 
#       assign("counter", curVal +1 ,envir=env)                 # and increment it by one
#       setTxtProgressBar(get("pb", envir=env), curVal +1)      # update progress bar
#       FUN(...)
#     }
#     res <- sapply(X, wrapper, ...)    # use wrapper with sapply
#     close(pb)                         # close progress bar 
#     return(res)
#   } else {
#     sapply(X, FUN, ...)
#   }
# }
# 
# 
# replicate_pb <- function (n, expr, simplify = "array", progress=TRUE){
#   if (progress)
#    fun <- sapply_pb
#   else  
#    fun <- sapply
#   fun(integer(n), eval.parent(substitute(function(...) expr)), 
#       simplify = simplify) 
# }    

sample_new <- function(x, size, replace = FALSE, prob = NULL){
  if (length(x) == 1)
    rep(x, size)
  else
    sample(x=x, size=size, replace = replace, prob = prob) 
}


# learing fuunctions

# foo <- function(x){
#   replicate(10, x)
# }
# foo(sample(1:5, 1))
# 
#   
# foo <- function(x){
#   x <- substitute(x)
#   replicate(10, eval(x))
# }
# foo(sample(1:5, 1))
# 
# nums <- 1:5
# foo(sample(nums, 1))
# 
# foo <- function(x){
#   replicate(10, eval(x))
# }
# foo(quote(sample(1:5, 1)))
# 
# foo <- function(x){
#   replicate(10, eval(x))
# }
# foo(enquote(sample(1:5, 1)))   



