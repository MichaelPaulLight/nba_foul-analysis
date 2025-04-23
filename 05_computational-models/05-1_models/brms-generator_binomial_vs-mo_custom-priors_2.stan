// generated with brms 2.22.0
functions {
  /* compute monotonic effects
   * Args:
   *   scale: a simplex parameter
   *   i: index to sum over the simplex
   * Returns:
   *   a scalar between 0 and rows(scale)
   */
  real mo(vector scale, int i) {
    if (i == 0) {
      return 0;
    } else {
      return rows(scale) * sum(scale[1:i]);
    }
  }
 /* compute correlated group-level effects
  * Args:
  *   z: matrix of unscaled group-level effects
  *   SD: vector of standard deviation parameters
  *   L: cholesky factor correlation matrix
  * Returns:
  *   matrix of scaled group-level effects
  */
  matrix scale_r_cor(matrix z, vector SD, matrix L) {
    // r is stored in another dimension order than z
    return transpose(diag_pre_multiply(SD, L) * z);
  }




}
data {
  int<lower=1> N;  // total number of observations
  array[N] int Y;  // response variable
  array[N] int trials;  // number of trials
  int<lower=1> Ksp;  // number of special effects terms
  int<lower=1> Imo;  // number of monotonic variables
  array[Imo] int<lower=1> Jmo;  // length of simplexes
  array[N] int Xmo_1;  // monotonic variable
  vector[Jmo[1]] con_simo_1;  // prior concentration of monotonic simplex
  // data for group-level effects of ID 1
  int<lower=1> N_1;  // number of grouping levels
  int<lower=1> M_1;  // number of coefficients per level
  array[N] int<lower=1> J_1;  // grouping indicator per observation
  // group-level predictor values
  vector[N] Z_1_1;
  int<lower=1> NC_1;  // number of group-level correlations
  // data for group-level effects of ID 2
  int<lower=1> N_2;  // number of grouping levels
  int<lower=1> M_2;  // number of coefficients per level
  array[N] int<lower=1> J_2;  // grouping indicator per observation
  // group-level predictor values
  vector[N] Z_2_1;
  int<lower=1> NC_2;  // number of group-level correlations
  // data for group-level effects of ID 3
  int<lower=1> N_3;  // number of grouping levels
  int<lower=1> M_3;  // number of coefficients per level
  array[N] int<lower=1> J_3;  // grouping indicator per observation
  // group-level predictor values
  vector[N] Z_3_1;
  // data for group-level effects of ID 4
  int<lower=1> N_4;  // number of grouping levels
  int<lower=1> M_4;  // number of coefficients per level
  array[N] int<lower=1> J_4;  // grouping indicator per observation
  // group-level predictor values
  vector[N] Z_4_1;
  int<lower=1> NC_4;  // number of group-level correlations
  // data for group-level effects of ID 5
  int<lower=1> N_5;  // number of grouping levels
  int<lower=1> M_5;  // number of coefficients per level
  array[N] int<lower=1> J_5;  // grouping indicator per observation
  // group-level predictor values
  vector[N] Z_5_1;
  int<lower=1> NC_5;  // number of group-level correlations
  // data for group-level effects of ID 6
  int<lower=1> N_6;  // number of grouping levels
  int<lower=1> M_6;  // number of coefficients per level
  array[N] int<lower=1> J_6;  // grouping indicator per observation
  // group-level predictor values
  vector[N] Z_6_1;
  int<lower=1> NC_6;  // number of group-level correlations
  int prior_only;  // should the likelihood be ignored?
}
transformed data {
}
parameters {
  real Intercept;  // temporary intercept for centered predictors
  simplex[Jmo[1]] simo_1;  // monotonic simplex
  vector[Ksp] bsp;  // special effects coefficients
  vector<lower=0>[M_1] sd_1;  // group-level standard deviations
  matrix[M_1, N_1] z_1;  // standardized group-level effects
  cholesky_factor_corr[M_1] L_1;  // cholesky factor of correlation matrix
  vector<lower=0>[M_2] sd_2;  // group-level standard deviations
  matrix[M_2, N_2] z_2;  // standardized group-level effects
  cholesky_factor_corr[M_2] L_2;  // cholesky factor of correlation matrix
  vector<lower=0>[M_3] sd_3;  // group-level standard deviations
  array[M_3] vector[N_3] z_3;  // standardized group-level effects
  vector<lower=0>[M_4] sd_4;  // group-level standard deviations
  matrix[M_4, N_4] z_4;  // standardized group-level effects
  cholesky_factor_corr[M_4] L_4;  // cholesky factor of correlation matrix
  vector<lower=0>[M_5] sd_5;  // group-level standard deviations
  matrix[M_5, N_5] z_5;  // standardized group-level effects
  cholesky_factor_corr[M_5] L_5;  // cholesky factor of correlation matrix
  vector<lower=0>[M_6] sd_6;  // group-level standard deviations
  matrix[M_6, N_6] z_6;  // standardized group-level effects
  cholesky_factor_corr[M_6] L_6;  // cholesky factor of correlation matrix
}
transformed parameters {
  matrix[N_1, M_1] r_1;  // actual group-level effects
  // using vectors speeds up indexing in loops
  vector[N_1] r_1_1;
  vector[N_1] r_1_2;
  matrix[N_2, M_2] r_2;  // actual group-level effects
  // using vectors speeds up indexing in loops
  vector[N_2] r_2_1;
  vector[N_2] r_2_2;
  vector[N_3] r_3_1;  // actual group-level effects
  matrix[N_4, M_4] r_4;  // actual group-level effects
  // using vectors speeds up indexing in loops
  vector[N_4] r_4_1;
  vector[N_4] r_4_2;
  matrix[N_5, M_5] r_5;  // actual group-level effects
  // using vectors speeds up indexing in loops
  vector[N_5] r_5_1;
  vector[N_5] r_5_2;
  matrix[N_6, M_6] r_6;  // actual group-level effects
  // using vectors speeds up indexing in loops
  vector[N_6] r_6_1;
  vector[N_6] r_6_2;
  real lprior = 0;  // prior contributions to the log posterior
  // compute actual group-level effects
  r_1 = scale_r_cor(z_1, sd_1, L_1);
  r_1_1 = r_1[, 1];
  r_1_2 = r_1[, 2];
  // compute actual group-level effects
  r_2 = scale_r_cor(z_2, sd_2, L_2);
  r_2_1 = r_2[, 1];
  r_2_2 = r_2[, 2];
  r_3_1 = (sd_3[1] * (z_3[1]));
  // compute actual group-level effects
  r_4 = scale_r_cor(z_4, sd_4, L_4);
  r_4_1 = r_4[, 1];
  r_4_2 = r_4[, 2];
  // compute actual group-level effects
  r_5 = scale_r_cor(z_5, sd_5, L_5);
  r_5_1 = r_5[, 1];
  r_5_2 = r_5[, 2];
  // compute actual group-level effects
  r_6 = scale_r_cor(z_6, sd_6, L_6);
  r_6_1 = r_6[, 1];
  r_6_2 = r_6[, 2];
  lprior += normal_lpdf(Intercept | 0, 1);
  lprior += dirichlet_lpdf(simo_1 | con_simo_1);
  lprior += normal_lpdf(bsp[1] | 0.2, 0.5);
  lprior += exponential_lpdf(sd_1 | 1);
  lprior += lkj_corr_cholesky_lpdf(L_1 | 2);
  lprior += exponential_lpdf(sd_2 | 1);
  lprior += lkj_corr_cholesky_lpdf(L_2 | 2);
  lprior += exponential_lpdf(sd_3 | 1);
  lprior += exponential_lpdf(sd_4 | 1);
  lprior += lkj_corr_cholesky_lpdf(L_4 | 2);
  lprior += exponential_lpdf(sd_5 | 1);
  lprior += lkj_corr_cholesky_lpdf(L_5 | 2);
  lprior += exponential_lpdf(sd_6 | 1);
  lprior += lkj_corr_cholesky_lpdf(L_6 | 2);
}
model {
  // likelihood including constants
  if (!prior_only) {
    // initialize linear predictor term
    vector[N] mu = rep_vector(0.0, N);
    mu += Intercept;
    for (n in 1:N) {
      // add more terms to the linear predictor
      mu[n] += (bsp[1] + r_1_2[J_1[n]] + r_2_2[J_2[n]] + r_4_2[J_4[n]] + r_5_2[J_5[n]] + r_6_2[J_6[n]]) * mo(simo_1, Xmo_1[n]) + r_1_1[J_1[n]] * Z_1_1[n] + r_2_1[J_2[n]] * Z_2_1[n] + r_3_1[J_3[n]] * Z_3_1[n] + r_4_1[J_4[n]] * Z_4_1[n] + r_5_1[J_5[n]] * Z_5_1[n] + r_6_1[J_6[n]] * Z_6_1[n];
    }
    target += binomial_logit_lpmf(Y | trials, mu);
  }
  // priors including constants
  target += lprior;
  target += std_normal_lpdf(to_vector(z_1));
  target += std_normal_lpdf(to_vector(z_2));
  target += std_normal_lpdf(z_3[1]);
  target += std_normal_lpdf(to_vector(z_4));
  target += std_normal_lpdf(to_vector(z_5));
  target += std_normal_lpdf(to_vector(z_6));
}
generated quantities {
  // actual population-level intercept
  real b_Intercept = Intercept;
  // compute group-level correlations
  corr_matrix[M_1] Cor_1 = multiply_lower_tri_self_transpose(L_1);
  vector<lower=-1,upper=1>[NC_1] cor_1;
  // compute group-level correlations
  corr_matrix[M_2] Cor_2 = multiply_lower_tri_self_transpose(L_2);
  vector<lower=-1,upper=1>[NC_2] cor_2;
  // compute group-level correlations
  corr_matrix[M_4] Cor_4 = multiply_lower_tri_self_transpose(L_4);
  vector<lower=-1,upper=1>[NC_4] cor_4;
  // compute group-level correlations
  corr_matrix[M_5] Cor_5 = multiply_lower_tri_self_transpose(L_5);
  vector<lower=-1,upper=1>[NC_5] cor_5;
  // compute group-level correlations
  corr_matrix[M_6] Cor_6 = multiply_lower_tri_self_transpose(L_6);
  vector<lower=-1,upper=1>[NC_6] cor_6;
  // extract upper diagonal of correlation matrix
  for (k in 1:M_1) {
    for (j in 1:(k - 1)) {
      cor_1[choose(k - 1, 2) + j] = Cor_1[j, k];
    }
  }
  // extract upper diagonal of correlation matrix
  for (k in 1:M_2) {
    for (j in 1:(k - 1)) {
      cor_2[choose(k - 1, 2) + j] = Cor_2[j, k];
    }
  }
  // extract upper diagonal of correlation matrix
  for (k in 1:M_4) {
    for (j in 1:(k - 1)) {
      cor_4[choose(k - 1, 2) + j] = Cor_4[j, k];
    }
  }
  // extract upper diagonal of correlation matrix
  for (k in 1:M_5) {
    for (j in 1:(k - 1)) {
      cor_5[choose(k - 1, 2) + j] = Cor_5[j, k];
    }
  }
  // extract upper diagonal of correlation matrix
  for (k in 1:M_6) {
    for (j in 1:(k - 1)) {
      cor_6[choose(k - 1, 2) + j] = Cor_6[j, k];
    }
  }
}

