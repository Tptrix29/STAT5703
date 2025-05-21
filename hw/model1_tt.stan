
data {
  int<lower=1> N;
  int<lower=0> y[N];
  int<lower=0> n[N];
}
parameters {
  vector<lower=0,upper=1>[N] theta;
}
model {
  theta ~ beta(9,2);
  y ~ binomial(n, theta);
}
generated quantities {
  real diff_contrast = max(theta) - min(theta);
}


