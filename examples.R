## ------------------------------------------------------------------------
library(inline)
library(data.table)
library(dplyr, warn.conflicts=FALSE)
library(Hmisc, warn.conflicts=FALSE)

## ------------------------------------------------------------------------
set.seed(90)
n <- 2000000
x <- data.frame(id=sample(10000, n, replace=TRUE),
  dateoffset=round(runif(n, -1000, 1000)),
  daysupply=sample(c(NA, 30, 90, 180, 360, 720), n, 
                   prob=c(0.01, 0.05, 0.75, 0.15, 0.03, 0.01), replace=TRUE)
)
x[,'date'] <- as.Date(x[,'dateoffset'], origin='2000-01-01')
y <- data.table(x)
head(x)

## ------------------------------------------------------------------------
system.time({
  a1 <- sapply(with(x, split(dateoffset, id)), min)
  x[,'base1'] <- with(x, unsplit(a1, id))
})

## ------------------------------------------------------------------------
system.time({
  a2 <- with(x, tapply(dateoffset, id, min))
  x[,'base2'] <- with(x, unsplit(a2, id))
})

## ------------------------------------------------------------------------
system.time({
  a3 <- aggregate(dateoffset ~ id, data=x, min)
  x[,'base3'] <- a3[match(x$id, a3$id),'dateoffset']
})

## ------------------------------------------------------------------------
system.time({
  a4 <- with(x, Hmisc::summarize(dateoffset, id, min))
  x[,'base4'] <- a4[match(x$id, a4$id),'dateoffset']
})

## ------------------------------------------------------------------------
system.time({
  y1 <- copy(y)
  setkey(y1, id, dateoffset)
  y1[,first := min(dateoffset), by=id]
})

## ------------------------------------------------------------------------
  system.time({
  y2 <- mutate(group_by(y, id), first=min(dateoffset))
})

## ------------------------------------------------------------------------
system.time({
  x1 <- x[with(x, order(id, dateoffset)),]
})

system.time({
  y2 <- arrange(y2, id, dateoffset)
})

## ------------------------------------------------------------------------
head(x1)
head(y1)
head(y2)

## ------------------------------------------------------------------------
system.time({
  y3 <- y %>% 
    group_by(id) %>% 
      mutate(first=min(dateoffset)) %>% 
        arrange(id, dateoffset)
})

## ------------------------------------------------------------------------
set.seed(90)
n <- 20000
x <- data.frame(id=sample(5000, n, replace=TRUE),
  dateoffset=round(runif(n, -1000, 1000)),
  daysupply=sample(c(NA, 30, 90, 180, 360, 720), n, 
                   prob=c(0.01, 0.05, 0.75, 0.15, 0.03, 0.01), replace=TRUE)
)
x[,'date'] <- as.Date(x[,'dateoffset'], origin='2000-01-01')
x1 <- x[with(x, order(id, dateoffset)),]
y1 <- data.table(x1)

## ------------------------------------------------------------------------
inbetween <- function(x, left, right) {
  x >= left & x <= right
}

daysOn <- function(dateoffset, daysupply) {
  i1a <- dateoffset
  i1b <- dateoffset-359
  i2a <- dateoffset-360
  i2b <- dateoffset-719
  daysupply[is.na(daysupply)] <- 0
  dates <- vector('list', length(dateoffset))
  for(i in seq_along(dates)) {
    dates[[i]] <- seq(dateoffset[i]+1, length.out=daysupply[i])
  }
  dates <- unlist(dates)
  i1 <- i2 <- integer(length(dateoffset))
  if(length(dates) > 0) {
    for(i in seq_along(i1)) {
      i1[i] <- as.integer(any(inbetween(dates, i1b[i], i1a[i])))
      i2[i] <- as.integer(any(inbetween(dates, i2b[i], i2a[i])))
    }
  }
  data.frame(in1=i1, in2=i2)
}

daysOn(c(0, 100, 350, 500), c(90, 180, 90, 90))

## ------------------------------------------------------------------------
inbetweenRcpp <- cxxfunction(
  signature(x="numeric", left="numeric", right="numeric"), plugin="Rcpp", 
  body='
  Rcpp::NumericVector X(x);
  double l = Rcpp::as<double>(left);
  double r = Rcpp::as<double>(right);
  int n = X.size();
  Rcpp::LogicalVector V(n);
  for (int i=0; i<n; i++) {
    V[i] = X[i] >= l && X[i] <= r;
  }
  return(V);
')

daysOn <- function(dateoffset, daysupply) {
  i1a <- dateoffset
  i1b <- dateoffset-359
  i2a <- dateoffset-360
  i2b <- dateoffset-719
  daysupply[is.na(daysupply)] <- 0
  # shorter to write, but saves no time
  dates <- unlist(mapply(seq, dateoffset+1, length.out=daysupply, SIMPLIFY=FALSE))
  i1 <- i2 <- numeric(length(dateoffset))
  if(length(dates) > 0) {
    for(i in seq_along(i1)) {
      i1[i] <- max(inbetweenRcpp(dates, i1b[i], i1a[i]))
      i2[i] <- max(inbetweenRcpp(dates, i2b[i], i2a[i]))
    }
  }
  data.frame(in1=i1, in2=i2)
}

## ------------------------------------------------------------------------
system.time({
  uids <- unique(x1$id)
  res <- vector('list', length(uids))
  for(i in seq_along(res)) {
    s <- x1[x1$id==uids[i],]
    res[[i]] <- daysOn(s$dateoffset, s$daysupply)
  }
  z1 <- do.call(rbind, res)
})

system.time(z2 <- do.call(rbind, lapply(split(x1, x1$id), function(i) 
  daysOn(i$dateoffset, i$daysupply)
)))

## ------------------------------------------------------------------------
system.time({
  uids <- unique(y1$id)
  res <- vector('list', length(uids))
  for(i in seq_along(res)) {
    s <- y1[id==uids[i]]
    res[[i]] <- daysOn(s$dateoffset, s$daysupply)
  }
  z3 <- do.call(rbind, res)
})

system.time(z4 <- do.call(rbind, 
  y1[,list(item=list(daysOn(dateoffset, daysupply))), by=id]$item
))

## ------------------------------------------------------------------------
system.time(z5 <- do(group_by(y1, id), daysOn(dateoffset, daysupply)))

## ------------------------------------------------------------------------
x1[,c('in1','in2')] <- z5[,list(in1,in2)]
head(x1)

## ------------------------------------------------------------------------
size <- 50000
x <- data.frame(pid=sample(15000, size, replace=TRUE), x=rnorm(size))
head(x)

## ------------------------------------------------------------------------
system.time({
  y <- split(x, x$pid)
  set.seed(100)
  ids <- sort(unique(x$pid))
  ids.boot <- sample(ids, replace=TRUE)
  x.boot <- lapply(seq_along(ids.boot), function(i) {
    cnt <- sum(ids.boot[seq(i)] == ids.boot[i])
    cbind(pid=sprintf("%s-%s", ids.boot[i], cnt), 
          y[[as.character(ids.boot[i])]][,-1,drop=FALSE], 
          stringsAsFactors=FALSE
    )
  })
  x.boot <- do.call(rbind, x.boot)
  # I like to reset rownames
  row.names(x.boot) <- NULL
})
head(x.boot)

