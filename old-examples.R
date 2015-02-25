library(Hmisc)
library(data.table)
library(dplyr)
library(inline)

set.seed(90)
n <- 2000000
x <- data.frame(id=sample(10000, n, replace=TRUE),
  dateoffset=round(runif(n, -1000, 1000)),
  daysupply=sample(c(NA, 30, 90, 180, 360, 720), n, prob=c(0.01, 0.05, 0.75, 0.15, 0.03, 0.01), replace=TRUE)
)
x[,'date'] <- as.Date(x[,'dateoffset'], origin='2000-01-01')
y <- data.table(x)

# first date

system.time({
a1 <- sapply(with(x, split(dateoffset, id)), min)
x[,'base1'] <- with(x, unsplit(a1, id))
})

system.time({
a2 <- with(x, tapply(dateoffset, id, min))
x[,'base2'] <- with(x, unsplit(a2, id))
})

system.time({
a3 <- aggregate(dateoffset ~ id, data=x, min)
x[,'base3'] <- a3[match(x$id, a3$id),'dateoffset']
})

system.time({
a4 <- with(x, Hmisc::summarize(dateoffset, id, min))
x[,'base4'] <- a4[match(x$id, a4$id),'dateoffset']
})

system.time({
y1 <- copy(y)
setkey(y1, id, dateoffset)
y1[,first := min(dateoffset), by=id]
})

system.time({
y2 <- mutate(group_by(y, id), first=min(dateoffset))
})

system.time({
x1 <- x[with(x, order(id, dateoffset)),]
})

system.time({
y2 <- arrange(y2, id, dateoffset)
})

system.time({
y3 <- y %>% group_by(id) %>% mutate(first=min(dateoffset)) %>% arrange(id, dateoffset)
})

colMaxRcpp <- cxxfunction(signature(X_="numeric"), plugin="Rcpp", body='
  Rcpp::NumericMatrix X(X_);
  int n = X.ncol();
  Rcpp::NumericVector V(n);
  for (int i=0; i<n; i++) {
     Rcpp::NumericVector W = X.column(i);
     V[i] = *std::max_element(W.begin(), W.end());  // from the STL
  }
  return(V);
')

inbetween <- function(x, left, right) {
  x >= left & x <= right
}

inbetweenRcpp <- cxxfunction(signature(x="numeric", left="numeric", right="numeric"), plugin="Rcpp", body='
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

daysOn <- function(dateoffset, daysupply, method=1) {
  i1a <- dateoffset
  i1b <- dateoffset-359
  i2a <- dateoffset-360
  i2b <- dateoffset-719
  daysupply[is.na(daysupply)] <- 0
  daysOn <- unlist(mapply(seq, dateoffset+1, length.out=daysupply, SIMPLIFY=FALSE))
  if(length(daysOn) == 0) {
    if(method == 4) {
      i1 <- i2 <- numeric(length(dateoffset))
    } else {
      i1 <- i2 <- integer(length(dateoffset))
    }
  } else {
    in1 <- mapply(inbetweenRcpp, left=i1b, right=i1a, MoreArgs = list(x=daysOn))
    in2 <- mapply(inbetweenRcpp, left=i2b, right=i2a, MoreArgs = list(x=daysOn))
    if(method == 1) {
      i1 <- +apply(in1, 2, any)
      i2 <- +apply(in2, 2, any)
    } else if(method == 2) {
      i1 <- apply(in1, 2, max)
      i2 <- apply(in2, 2, max)
    } else if(method == 3) {
      i1 <- do.call(pmax, data.frame(t(in1)))
      i2 <- do.call(pmax, data.frame(t(in2)))
    } else if(method == 4) {
      i1 <- colMaxRcpp(in1)
      i2 <- colMaxRcpp(in2)
    }
  }
  data.frame(in1=i1, in2=i2)
}

set.seed(90)
n <- 20000
x <- data.frame(id=sample(5000, n, replace=TRUE),
  dateoffset=round(runif(n, -1000, 1000)),
  daysupply=sample(c(NA, 30, 90, 180, 360, 720), n, prob=c(0.01, 0.05, 0.75, 0.15, 0.03, 0.01), replace=TRUE)
)
x[,'date'] <- as.Date(x[,'dateoffset'], origin='2000-01-01')
x1 <- x[with(x, order(id, dateoffset)),]
y1 <- data.table(x1)

system.time({
  uids <- unique(x1$id)
  res <- vector('list', length(uids))
  for(i in seq_along(res)) {
    s <- x1[x1$id==uids[i],]
    res[[i]] <- daysOn(s$dateoffset, s$daysupply, 4)
  }
  z <- do.call(rbind, res)
})

system.time(z0 <- do.call(rbind, lapply(split(x1, x1$id), function(i) daysOn(i$dateoffset, i$daysupply, 4))))

system.time({
  uids <- unique(y1$id)
  res <- vector('list', length(uids))
  for(i in seq_along(res)) {
    s <- y1[id==uids[i]]
    res[[i]] <- daysOn(s$dateoffset, s$daysupply, 4)
  }
  z <- do.call(rbind, res)
})

system.time(zzz <- do.call(rbind, y1[,list(bah=list(daysOn(dateoffset, daysupply))), by=id]$bah))

system.time(z1 <- do(group_by(y1, id), daysOn(dateoffset, daysupply, 1)))
system.time(z2 <- do(group_by(y1, id), daysOn(dateoffset, daysupply, 2)))
system.time(z3 <- do(group_by(y1, id), daysOn(dateoffset, daysupply, 3)))
system.time(z4 <- do(group_by(y1, id), daysOn(dateoffset, daysupply, 4)))
x1[,c('in1','in2')] <- z4[,list(in1,in2)]


# bootstrap by id

system.time({
  size <- 50000
  x <- data.frame(pid=sample(15000, size, replace=TRUE), x=rnorm(size))
  y <- split(x, x$pid)
  set.seed(100)
  ids <- sort(unique(x$pid))
  ids.boot <- sample(ids, replace=TRUE)
  x.boot <- lapply(seq_along(ids.boot), function(i) {
    cnt <- sum(ids.boot[seq(i)] == ids.boot[i])
    cbind(pid=sprintf("%s-%s", ids.boot[i], cnt), y[[as.character(ids.boot[i])]][,-1,drop=FALSE], stringsAsFactors=FALSE)
  })
  x.boot <- do.call(rbind, x.boot)
  # I like to reset rownames
  row.names(x.boot) <- NULL
})

# countRcpp <- cxxfunction(signature(needle="numeric", haystack="numeric", length="integer"), plugin="Rcpp", body='
#   Rcpp::NumericVector X(haystack);
#   double need = Rcpp::as<double>(needle);
#   int n = Rcpp::as<int>(length);
#   int cnt = 0;
#   for (int i=0; i<n; i++) {
#     if(X[i] == need) cnt++;
#   }
#   return Rcpp::wrap(cnt);
# ')
# 
# system.time({
#   size <- 50000
#   x <- data.frame(pid=sample(15000, size, replace=TRUE), x=rnorm(size))
#   y <- split(x, x$pid)
#   set.seed(100)
#   ids <- sort(unique(x$pid))
#   ids.boot <- sample(ids, replace=TRUE)
#   x.boot <- lapply(seq_along(ids.boot), function(i) {
#     cnt <- countRcpp(ids.boot[i], ids.boot, i)
#     cbind(pid=sprintf("%s-%s", ids.boot[i], cnt), y[[as.character(ids.boot[i])]][,-1,drop=FALSE], stringsAsFactors=FALSE)
#   })
#   x.boot <- do.call(rbind, x.boot)
#   # I like to reset rownames
#   row.names(x.boot) <- NULL
# })
