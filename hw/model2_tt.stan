
data {
  int<lower=1> N;
  int<lower=0> y[N];
  int<lower=0> n[N];
}
parameters {
  real<lower=0> a;
  real<lower=0> b;
  vector<lower=0,upper=1>[N] theta;
}
model {
  a ~ exponential(1);
  b ~ exponential(1);
  theta ~ beta(a,b);
  y ~ binomial(n, theta);
}
generated quantities {
  real diff_contrast = theta[N] - theta[1];
  real group_mode = (a - 1) / (a + b - 2);
}


