data{
  int N;
  int y[N];
  real alpha;
  real beta;
}
parameters{
  real<lower=0,upper=1> p;
}
model{
  target+=bernoulli_lpmf( y | p );
  target+=beta_lpdf( p | alpha, beta );
}

