
# utilities.R
# custom functions to make R friendlier

# forked and adapted from M.Bishop, https://github.com/MichaelMBishop/util

# HOW TO USE?
# source("/path/to/util.R")
# functions are good citizens in that they don't pollute the global namespace.
# they are wrapped in "util", which you can attach to at the start of your source file, like so:
# > attach(util)


util = new.env()

library(stats)

util$as.c <- as.character

util$as.n <- as.numeric

util$as.nc <- function(to_be_numeric) {
  as.numeric(as.character(to_be_numeric))
}

util$unwhich <- function(indices, len=length(indices)) {
  # reverse of which(): from indices to boolean mask.
  ret = rep(F,len)
  ret[indices] = T
  ret
}

util$nna <- function(...) !is.na(...)

util$kna <- function(x) x[nna(x)]

util$present_levels <- function(x) intersect(levels(x), x)

util$trim_levels <- function(...) UseMethod("trim_levels")

util$trim_levels.factor <- function(x)  factor(x, levels=present_levels(x))

util$trim_levels.data.frame <- function(x) {
  for (n in names(x))
    if (is.factor(x[,n]))
      x[,n] = trim_levels(x[,n])
  x
}


# grep() returns indices of matches.  Variants:

util$bgrep <- function(pat,x, ...) {
  # "boolean" grep: return a logical vector ready for vector ops
  # like & |  and others
  unwhich(grep(pat,x,...), length(x))
}

util$ngrep <- function(pat,x, ...)
  # "normal" grep: return values, not indices
  x[grep(pat,x,...)]


util$tapply3 <- function(x, ...) {
  # like tapply but preserves factors
  if (is.factor(x)) {
    r = factor(tapply(as.character(x), ...), levels=levels(x))
  } else {
    r = tapply(x, ...)
  }
  r
}


util$printf <- function(...) cat(sprintf(...))

util$msg <- function(...)  cat(..., "\n", file=stderr())

util$ppy <- function(x, column.major=FALSE, ...) {
  # pretty-print as yaml.  intended for rows with big textual cells.
  # a la mysql's \G operator
  library(yaml)
  cat(as.yaml(x, column.major=column.major), ...)
  cat("\n", ...)
}


util$timeit <- function(expr, name=NULL) {
  # print how long the expression takes, and return its value too.
  # So you can interpose   timeit({ blabla })   around any chunk of code "blabla".
  start = Sys.time()
  ret = eval(expr)
  finish = Sys.time()
  if (!is.null(name)) cat(name,": ")
  print(finish-start)
  invisible(ret)
}


util$vim <- function(...) {
  system(paste("vim",...))
}

########################################
## Graphics output wrappers
## For easy one-liners, like:
## dopdf("tmp.pdf",width=5,height=5,cmd=plot(x,y))

util$dopdf <- function(filename,..., cmd) {
  pdf(filename, ...)
  eval(cmd)
  dev.off()
  if (exists('OPEN') && OPEN)
    system(sprintf("open %s", filename))
}

util$dopng <- function(filename,..., cmd) {
  png(filename, ...)
  eval(cmd)
  dev.off()
  if ((exists('OPEN') && OPEN))
    system(sprintf("open %s", filename))
}


util$dosink <- function(filename,cmd, open=NULL) {
  # like capture.output() but follows open/OPEN conventions here
  sink(filename)
  eval(cmd)
  sink(NULL)
  if (prio_check(open, exists('OPEN') && OPEN))
    system(sprintf("open %s", filename))
}

util$excel <- function(d, row.names=FALSE, file.name="") {
  if(file.name!='')
  {
     f <- p("/tmp/", paste(file.name, "csv", sep='.'))
  }
  else
  {
     f <- paste("/tmp/tmp", round(runif(1)*1000),"csv",  sep='.')
  }

  con = file(f, "w")
  write.csv(d, con, row.names)
  close(con)
  system(paste("open -a '/Applications/Excel.app' ",f, sep=''))
}

util$keep.columns <-  function(data, columns) data[, names(data) %in% columns]
util$drop.columns <-  function(data, columns) data[, !names(data) %in% columns]

util$match.columns <- function(dataFrame, pattern, includeFirstColumn=FALSE) {
  # returns columns of dataset that match a regular expression.
  # this function is purely for syntactic sugar
  # includeFirstColumn allows to include the index

  matchPattern <- grep(pattern, names(dataFrame))

  if (includeFirstColumn) {
    columns <- c(1, matchPattern)
  } else {
    columns <- matchPattern
  }
  return( dataFrame[, columns])
}

util$match.column.names <- function(data, exp, ...) {
  return( names(data)[grep(exp, names(data), ...)] )
}

util$match.column.index <- function(data, exp, ...) {
  return( grep(exp, names(data), ...) )
}

util$logit <- function(p) { log(p/(1-p)) }
util$antilogit <- function(eta) {exp(eta)/(1 + exp(eta)) }

util$lu <- function(x) length(unique(x))

util$p <- function(...) {
  return(paste(..., sep=""))
}

util$apply.function <- function(pointer, f, env = .GlobalEnv)
{
  # apply a function to a variable (but variable is specified as string)
  assign(pointer, f(get(pointer)), envir = env)
}

util$make.factor <- function(x) 
{ 
  assign(x, factor(get(x)), envir = .GlobalEnv)
}

# make code more readable
util$first <- function(x) x[1]
util$last <- function(x) x[length(x)]

util$r <- function(x) round(x, 2)

util$random <- function(x, num=1) round(runif(num)*x, 0)

util$browse <- function(d) { tmp.12389 <- fix(d); tmp.12389 <- NULL }

util$keep.pos <- function(x) ifelse(x>0, x, 0)
util$keep.real <- function(x) ifelse(!(is.infinite(x) | is.nan(x)), x, 0)

util$normalize <- function(x) (x-mean(x))/sd(x)

util$reorder.factor <- function(x, ord) factor(x, levels=levels(x)[ord])

# for latex export
util$nb <- function(x) formatC(x, big.mark=",", format="d")
# display percentage in standard format
util$nbp <- function(x) paste(round(x*100),"%",sep="")
# same for inline references (using Sexpr)
util$in_nbp <- function(x) paste(round(x*100),"\\%",sep="")

util$mean.na <- function(x) mean(x, na.rm=T)
util$sum.na <- function(x) sum(x, na.rm=T)
util$na_to_0 <- function(x) ifelse(is.na(x), 0, x)

util$between <- function(x, a, b) { (x>=a) & (x<=b) }	# returns boolean value

util$se <- function(x) sqrt(var(x)/length(x))

util$ci95<-function(x) { 
	t.value<- qt(0.975,length(x)-1) 
	standard.error<-se(x) 

	t.value*standard.error 
}


util$num <- round(runif(10)*100)
util$flt <- runif(10)*100
util$dup <- rep(round(runif(5)*100), 2)
util$str <- c("one","two","three","four","five","six","seven","eight","nine","ten")

util$sample.data <- data.frame(num, flt, dup, str)

util$num <- NULL
util$flt <- NULL
util$str <- NULL
util$dup <- NULL


util$circle.cor = function(cor, data, axes = FALSE, xlab = "",
     ylab = "", asp = 1, title = paste("Correlation Matrix. DV:",colnames(data)[n] ,sep=""),
     ...) 
  {
# adapted from http://weitaiyun.blogspot.com/2009/03/visulization-of-correlation-matrix.html
#################################################################################
## an example 
#	data(mtcars) 
#	fit = lm(mpg ~ ., mtcars) 
#	cor = summary(fit, correlation = TRUE)$correlation 
#	circle.cor(cor)
#################################################################################
   n = nrow(cor)
   par(mar = c(0, 5, 2, 0), bg = "white", xpd=T)
   plot(c(0, n + 0.8), c(0, n + 0.8), axes = axes, xlab = "",
           ylab = "", asp = 1, type = "n")
   ##add grid
   segments(rep(0.5, n + 1), 0.5 + 0:n, rep(n + 0.5, n + 1),
           0.5 + 0:n, col = "gray")
   segments(0.5 + 0:n, rep(0.5, n + 1), 0.5 + 0:n, rep(n + 0.5,
                   n), col = "gray")
   ##define circles' background color.
   ##black for positive correlation coefficient and white for negative
   bg = cor
   bg[cor > 0] = "black"
   bg[cor <= 0] = "white"   ##plot n*n circles using vector language

   symbols(
    rep(1:n, each = n), 
    rep(n:1, n), 
    add = TRUE, inches = F, 
    circles = as.vector(sqrt(abs(cor))/2), 
    bg = as.vector(bg))
# text(rep(0, n), 1:n, n:1, col = "red")  
# text(1:n, rep(n + 1), 1:n, col = "red")  
   text(-1, seq(1:n), c(colnames(data)[(n-1):1],"intcp."), cex=0.6, col = "red")
   text(seq(1:n), n+1.2, c("intcp.",colnames(data)[1:(n-1)]), cex=0.6, srt=45, col = "red")
   title(title) 
} 

while("util" %in% search())
  detach("util")
attach(util)
