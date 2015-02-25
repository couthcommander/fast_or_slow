# When Fast is Slow and Slow is Fast

An experienced R user will be familiar with the `tapply` and
`aggregate` functions, as well as iterating over the output of `split`.
These methods may be difficult to learn, and worse, suffer from slow
performance.  It may be worth investing the time into some R packages.
This discussion will give an overview of `data.table`, `dplyr`, `inline` and `Rcpp`.

Or so I thought.

If I've learned one thing in the last 48 hours, then I've learned two.

1. Just because you've used a package in the last week doesn't mean you can
put together a seminar in only two hours.

1. Nothing is ever as fast or slow as you think it is.

---

Load several R packages


```r
library(inline)
library(data.table)
library(dplyr, warn.conflicts=FALSE)
library(Hmisc, warn.conflicts=FALSE)
```

---

# Task 1

What is the best way to find the first date for each patient?


```r
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
```

```
##     id dateoffset daysupply       date
## 1 5308        184        90 2000-07-03
## 2 8851       -864        90 1997-08-20
## 3 4400       -412        90 1998-11-15
## 4 9423       -609        90 1998-05-02
## 5 1884        838       360 2002-04-18
## 6 6556       -697       180 1998-02-03
```

---

## split/unsplit


```r
system.time({
  a1 <- sapply(with(x, split(dateoffset, id)), min)
  x[,'base1'] <- with(x, unsplit(a1, id))
})
```

```
##    user  system elapsed 
##   0.451   0.024   0.474
```

---

## tapply/unsplit


```r
system.time({
  a2 <- with(x, tapply(dateoffset, id, min))
  x[,'base2'] <- with(x, unsplit(a2, id))
})
```

```
##    user  system elapsed 
##   0.670   0.012   0.681
```

---

## aggregate/match


```r
system.time({
  a3 <- aggregate(dateoffset ~ id, data=x, min)
  x[,'base3'] <- a3[match(x$id, a3$id),'dateoffset']
})
```

```
##    user  system elapsed 
##  29.736   0.120  29.829
```

---

## summarize/match


```r
system.time({
  a4 <- with(x, Hmisc::summarize(dateoffset, id, min))
  x[,'base4'] <- a4[match(x$id, a4$id),'dateoffset']
})
```

```
##    user  system elapsed 
##   1.265   0.036   1.299
```

---

## data.table


```r
system.time({
  y1 <- copy(y)
  setkey(y1, id, dateoffset)
  y1[,first := min(dateoffset), by=id]
})
```

```
##    user  system elapsed 
##   0.291   0.024   0.315
```

---

## dplyr


```r
  system.time({
  y2 <- mutate(group_by(y, id), first=min(dateoffset))
})
```

```
##    user  system elapsed 
##   0.328   0.112   0.440
```

---

## Reorder data set by id


```r
system.time({
  x1 <- x[with(x, order(id, dateoffset)),]
})
```

```
##    user  system elapsed 
##   3.142   0.068   3.209
```

```r
system.time({
  y2 <- arrange(y2, id, dateoffset)
})
```

```
##    user  system elapsed 
##   0.315   0.016   0.331
```

---

### Output


```r
head(x1)
```

```
##         id dateoffset daysupply       date base1 base2 base3 base4
## 87788    1       -998        90 1997-04-08  -998  -998  -998  -998
## 786145   1       -996        90 1997-04-10  -998  -998  -998  -998
## 1076317  1       -969        90 1997-05-07  -998  -998  -998  -998
## 1156307  1       -955       720 1997-05-21  -998  -998  -998  -998
## 1091331  1       -939       180 1997-06-06  -998  -998  -998  -998
## 1913376  1       -938        90 1997-06-07  -998  -998  -998  -998
```

```r
head(y1)
```

```
##    id dateoffset daysupply       date first
## 1:  1       -998        90 1997-04-08  -998
## 2:  1       -996        90 1997-04-10  -998
## 3:  1       -969        90 1997-05-07  -998
## 4:  1       -955       720 1997-05-21  -998
## 5:  1       -939       180 1997-06-06  -998
## 6:  1       -938        90 1997-06-07  -998
```

```r
head(y2)
```

```
##   id dateoffset daysupply       date first
## 1  1       -998        90 1997-04-08  -998
## 2  1       -996        90 1997-04-10  -998
## 3  1       -969        90 1997-05-07  -998
## 4  1       -955       720 1997-05-21  -998
## 5  1       -939       180 1997-06-06  -998
## 6  1       -938        90 1997-06-07  -998
```

---
 
## dplyr alternative


```r
system.time({
  y3 <- y %>% 
    group_by(id) %>% 
      mutate(first=min(dateoffset)) %>% 
        arrange(id, dateoffset)
})
```

```
##    user  system elapsed 
##   0.558   0.076   0.635
```

---

# Task 2

For each record, determine if patient was on a prescription given in the last 360 days.  Also calculate for between 360 and 720 days.


```r
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
```

---

## daysOn function

The first step is writing a function that will calculate indicators.  We'll want to optimize this as much as possible.


```r
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
```

```
##   in1 in2
## 1   0   0
## 2   1   0
## 3   1   0
## 4   1   1
```

---

## Inline C++ Code


```r
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
```

```
## 
## Attaching package: 'Rcpp'
## 
## The following object is masked from 'package:inline':
## 
##     registerPlugin
```

```r
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
```

---

## Base Functions


```r
system.time({
  uids <- unique(x1$id)
  res <- vector('list', length(uids))
  for(i in seq_along(res)) {
    s <- x1[x1$id==uids[i],]
    res[[i]] <- daysOn(s$dateoffset, s$daysupply)
  }
  z1 <- do.call(rbind, res)
})
```

```
##    user  system elapsed 
##  12.660   0.052  12.700
```

```r
system.time(z2 <- do.call(rbind, lapply(split(x1, x1$id), function(i) 
  daysOn(i$dateoffset, i$daysupply)
)))
```

```
##    user  system elapsed 
##   2.850   0.012   2.859
```

---

## data.table


```r
system.time({
  uids <- unique(y1$id)
  res <- vector('list', length(uids))
  for(i in seq_along(res)) {
    s <- y1[id==uids[i]]
    res[[i]] <- daysOn(s$dateoffset, s$daysupply)
  }
  z3 <- do.call(rbind, res)
})
```

```
##    user  system elapsed 
##   6.956   0.000   6.949
```

```r
system.time(z4 <- do.call(rbind, 
  y1[,list(item=list(daysOn(dateoffset, daysupply))), by=id]$item
))
```

```
##    user  system elapsed 
##   2.155   0.000   2.153
```

---

## dplyr


```r
system.time(z5 <- do(group_by(y1, id), daysOn(dateoffset, daysupply)))
```

```
## Warning in `[.data.table`(`_dt`, , daysOn(dateoffset, daysupply), by =
## `_vars`, : This j doesn't use .SD but .SDcols has been supplied. Ignoring
## .SDcols. See ?data.table.
```

```
##    user  system elapsed 
##   1.679   0.000   1.678
```

Add indicators to original data.frame


```r
x1[,c('in1','in2')] <- z5[,list(in1,in2)]
head(x1)
```

```
##       id dateoffset daysupply       date in1 in2
## 2543   1       -960        90 1997-05-16   0   0
## 15742  1       -733       180 1997-12-29   1   0
## 2427   1       -511        90 1998-08-08   1   1
## 3748   1       -421       720 1998-11-06   1   1
## 362    1         93        30 2000-04-03   1   1
## 5925   1        335       180 2000-12-01   1   1
```

---

# Task 3

What's the best way to sample by id with replacement?


```r
size <- 50000
x <- data.frame(pid=sample(15000, size, replace=TRUE), x=rnorm(size))
head(x)
```

```
##     pid           x
## 1 10379  2.34370451
## 2 13478  0.49335897
## 3  6373 -0.10659186
## 4 10976  0.54657588
## 5 10124 -0.09025427
## 6 10808  1.03918057
```
---

## Solution with base functions


```r
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
```

```
##    user  system elapsed 
##  10.889   0.004  10.885
```

```r
head(x.boot)
```

```
##      pid           x
## 1 4602-1  0.04101226
## 2 4602-1 -0.57563023
## 3 4602-1  0.42206017
## 4 4602-1 -1.53307351
## 5 3849-1 -0.53401561
## 6 3849-1 -1.92786810
```
