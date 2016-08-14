
#gset <- RGtk2::gObjectSetData
#gget <- RGtk2::gObjectGetData


sample_new <- function(x, size, replace = FALSE, prob = NULL)
{
  if (length(x) == 1)
    rep(x, size)
  else
    sample(x=x, size=size, replace = replace, prob = prob)
}




