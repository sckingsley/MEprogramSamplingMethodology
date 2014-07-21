library(gtools)
estimate = function (a, d) { -log(d/2) / (2*a^2) }
sig = function (a, n) { pbinom(floor(n*0.5)-floor(n*a), size=floor(n), prob=0.5)}
binomsize = function (a,d) {
  r=c(1,2*estimate(a,d))
  v=binsearch(function(n) {sig(a,n)-d}, range=r, lower=min(r), upper=max(r))
  v$where[[length(v$where)]]}

help(gtools)
