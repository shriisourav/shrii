#' Square Function - for square a number
#' @param  value
#' @return value
#' @export


####SQUARE###################
shriisqr <- function(x) {
  return(x * x)
}


#####BOXPLOT MANIPULATION######
cc <- c("data.frame", "matrix", "data.table")
shriibox <- function(x){
  if (class(x) %in% cc) {
    sx <- data.frame(Colum = colnames(x), Count=length(x), Percent=length(x))
    for (i in 1:length(x)) {
      n <- boxplot(x[,i], plot = FALSE)
      cnt <- length(n$out)
      per <- cnt/length(x[,i])*100
      sx[i,2:3] <- c(cnt,per)
    }
    boxplot(x, col = "bisque")
    return(sx)
  } else {
    n <- boxplot(x, col = "bisque")
    cnt <- length(n$out)
    per <- (cnt/length(x))*100
    vec <- c("Count:", cnt, "Percent:" ,per)
    return(noquote(vec))
  }
}


###################Replace NA#####################
shriina <- function(x,y){ifelse(is.na(x),y(na.omit(x)),x)}

#########ReadXML#####










