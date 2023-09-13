# utility functions
## Check labels
.check_desc <- function(x){
  out <- is.list(x) &&
    !is.null(names(x)) &&
    all(names(x) %in% c("en","nl")) &&
    length(x) <= 2 &&
    all(sapply(x,is.character))
  if(!out)
    stop("Description names look off.")
}
