data{
    int<lower=1> N_reg;
    int<lower=1> N_stt;
    int<lower=1> N_eth;
    int<lower=1> N_inc;
    int<lower=1> N_age;
    int<lower=1> N_reg_X_eth;
    int<lower=1> N_reg_X_inc;
    int<lower=1> N_reg_X_age;
    int<lower=1> N_stt_X_eth;
    int<lower=1> N_stt_X_inc;
    int<lower=1> N_stt_X_age;
    int<lower=1> N_eth_X_inc;
    int<lower=1> N_eth_X_age;
    int<lower=1> N_inc_X_age;
    int<lower=1> N_stt_X_eth_X_inc;
    int<lower=1> N_stt_X_eth_X_age;
    int<lower=1> N_stt_X_inc_X_age;
    int<lower=1> N_eth_X_inc_X_age;
    int<lower=1> N;
    int yes[N];
    int yes_size[N];
    real z_inc[N];
    real z_incstt[N];
    real z_trnprv[N];
    real z_inc_X_z_incstt[N];
    real z_inc_X_z_trnprv[N];
    int reg[N];
    int stt[N];
    int eth[N];
    int inc[N];
    int age[N];
    int reg_X_eth[N];
    int reg_X_inc[N];
    int reg_X_age[N];
    int stt_X_eth[N];
    int stt_X_inc[N];
    int stt_X_age[N];
    int eth_X_inc[N];
    int eth_X_age[N];
    int inc_X_age[N];
    int stt_X_eth_X_inc[N];
    int stt_X_eth_X_age[N];
    int stt_X_inc_X_age[N];
    int eth_X_inc_X_age[N];
}
parameters{
    real Intercept;
    real b_z_inc;
    real b_z_incstt;
    real b_z_trnprv;
    real b_z_inc_X_z_incstt;
    real b_z_inc_X_z_trnprv;
    vector[N_inc] v_inc_Intercept_std;
    real<lower=0> sigma_v_inc_Intercept_inc;
    vector<lower=0>[2] sigma_reg;
    cholesky_factor_corr[2] L_Rho_reg;
    matrix[2, N_reg] m_z_reg_z_inc_std;
    vector<lower=0>[2] sigma_stt;
    cholesky_factor_corr[2] L_Rho_stt;
    matrix[2, N_stt] m_z_stt_z_inc_std;
    vector<lower=0>[2] sigma_eth;
    cholesky_factor_corr[2] L_Rho_eth;
    matrix[2, N_eth] m_z_eth_z_inc_std;
    vector<lower=0>[2] sigma_age;
    cholesky_factor_corr[2] L_Rho_age;
    matrix[2, N_age] m_z_age_z_inc_std;
    vector[N_reg_X_eth] v_reg_X_eth_Intercept_std;
    real<lower=0> sigma_v_reg_X_eth_Intercept_reg_X_eth;
    vector[N_reg_X_inc] v_reg_X_inc_Intercept_std;
    real<lower=0> sigma_v_reg_X_inc_Intercept_reg_X_inc;
    vector[N_reg_X_age] v_reg_X_age_Intercept_std;
    real<lower=0> sigma_v_reg_X_age_Intercept_reg_X_age;
    vector[N_stt_X_eth] v_stt_X_eth_Intercept_std;
    real<lower=0> sigma_v_stt_X_eth_Intercept_stt_X_eth;
    vector[N_stt_X_inc] v_stt_X_inc_Intercept_std;
    real<lower=0> sigma_v_stt_X_inc_Intercept_stt_X_inc;
    vector[N_stt_X_age] v_stt_X_age_Intercept_std;
    real<lower=0> sigma_v_stt_X_age_Intercept_stt_X_age;
    vector[N_eth_X_inc] v_eth_X_inc_Intercept_std;
    real<lower=0> sigma_v_eth_X_inc_Intercept_eth_X_inc;
    vector[N_eth_X_age] v_eth_X_age_Intercept_std;
    real<lower=0> sigma_v_eth_X_age_Intercept_eth_X_age;
    vector[N_inc_X_age] v_inc_X_age_Intercept_std;
    real<lower=0> sigma_v_inc_X_age_Intercept_inc_X_age;
    vector[N_stt_X_eth_X_inc] v_stt_X_eth_X_inc_Intercept_std;
    real<lower=0> sigma_v_stt_X_eth_X_inc_Intercept_stt_X_eth_X_inc;
    vector[N_stt_X_eth_X_age] v_stt_X_eth_X_age_Intercept_std;
    real<lower=0> sigma_v_stt_X_eth_X_age_Intercept_stt_X_eth_X_age;
    vector[N_stt_X_inc_X_age] v_stt_X_inc_X_age_Intercept_std;
    real<lower=0> sigma_v_stt_X_inc_X_age_Intercept_stt_X_inc_X_age;
    vector[N_eth_X_inc_X_age] v_eth_X_inc_X_age_Intercept_std;
    real<lower=0> sigma_v_eth_X_inc_X_age_Intercept_eth_X_inc_X_age;
}
transformed parameters{
    vector[N_eth_X_inc_X_age] v_eth_X_inc_X_age_Intercept;
    vector[N_stt_X_inc_X_age] v_stt_X_inc_X_age_Intercept;
    vector[N_stt_X_eth_X_age] v_stt_X_eth_X_age_Intercept;
    vector[N_stt_X_eth_X_inc] v_stt_X_eth_X_inc_Intercept;
    vector[N_inc_X_age] v_inc_X_age_Intercept;
    vector[N_eth_X_age] v_eth_X_age_Intercept;
    vector[N_eth_X_inc] v_eth_X_inc_Intercept;
    vector[N_stt_X_age] v_stt_X_age_Intercept;
    vector[N_stt_X_inc] v_stt_X_inc_Intercept;
    vector[N_stt_X_eth] v_stt_X_eth_Intercept;
    vector[N_reg_X_age] v_reg_X_age_Intercept;
    vector[N_reg_X_inc] v_reg_X_inc_Intercept;
    vector[N_reg_X_eth] v_reg_X_eth_Intercept;
    matrix[N_age,2] m_z_age_z_inc;    
    matrix[N_eth,2] m_z_eth_z_inc;    
    matrix[N_stt,2] m_z_stt_z_inc;    
    matrix[N_reg,2] m_z_reg_z_inc;    
    vector[N_inc] v_inc_Intercept;
    v_eth_X_inc_X_age_Intercept <- sigma_v_eth_X_inc_X_age_Intercept_eth_X_inc_X_age * v_eth_X_inc_X_age_Intercept_std; 
    v_stt_X_inc_X_age_Intercept <- sigma_v_stt_X_inc_X_age_Intercept_stt_X_inc_X_age * v_stt_X_inc_X_age_Intercept_std; 
    v_stt_X_eth_X_age_Intercept <- sigma_v_stt_X_eth_X_age_Intercept_stt_X_eth_X_age * v_stt_X_eth_X_age_Intercept_std; 
    v_stt_X_eth_X_inc_Intercept <- sigma_v_stt_X_eth_X_inc_Intercept_stt_X_eth_X_inc * v_stt_X_eth_X_inc_Intercept_std; 
    v_inc_X_age_Intercept <- sigma_v_inc_X_age_Intercept_inc_X_age * v_inc_X_age_Intercept_std; 
    v_eth_X_age_Intercept <- sigma_v_eth_X_age_Intercept_eth_X_age * v_eth_X_age_Intercept_std; 
    v_eth_X_inc_Intercept <- sigma_v_eth_X_inc_Intercept_eth_X_inc * v_eth_X_inc_Intercept_std; 
    v_stt_X_age_Intercept <- sigma_v_stt_X_age_Intercept_stt_X_age * v_stt_X_age_Intercept_std; 
    v_stt_X_inc_Intercept <- sigma_v_stt_X_inc_Intercept_stt_X_inc * v_stt_X_inc_Intercept_std; 
    v_stt_X_eth_Intercept <- sigma_v_stt_X_eth_Intercept_stt_X_eth * v_stt_X_eth_Intercept_std; 
    v_reg_X_age_Intercept <- sigma_v_reg_X_age_Intercept_reg_X_age * v_reg_X_age_Intercept_std; 
    v_reg_X_inc_Intercept <- sigma_v_reg_X_inc_Intercept_reg_X_inc * v_reg_X_inc_Intercept_std; 
    v_reg_X_eth_Intercept <- sigma_v_reg_X_eth_Intercept_reg_X_eth * v_reg_X_eth_Intercept_std; 
    m_z_age_z_inc <- (diag_pre_multiply(sigma_age, L_Rho_age) * m_z_age_z_inc_std)';
    m_z_eth_z_inc <- (diag_pre_multiply(sigma_eth, L_Rho_eth) * m_z_eth_z_inc_std)';
    m_z_stt_z_inc <- (diag_pre_multiply(sigma_stt, L_Rho_stt) * m_z_stt_z_inc_std)';
    m_z_reg_z_inc <- (diag_pre_multiply(sigma_reg, L_Rho_reg) * m_z_reg_z_inc_std)';
    v_inc_Intercept <- sigma_v_inc_Intercept_inc * v_inc_Intercept_std; 
}
model{
    vector[N] p;
    sigma_v_eth_X_inc_X_age_Intercept_eth_X_inc_X_age ~ normal(0, 1);
    v_eth_X_inc_X_age_Intercept_std ~ normal(0, 1);
    sigma_v_stt_X_inc_X_age_Intercept_stt_X_inc_X_age ~ normal(0, 1);
    v_stt_X_inc_X_age_Intercept_std ~ normal(0, 1);
    sigma_v_stt_X_eth_X_age_Intercept_stt_X_eth_X_age ~ normal(0, 1);
    v_stt_X_eth_X_age_Intercept_std ~ normal(0, 1);
    sigma_v_stt_X_eth_X_inc_Intercept_stt_X_eth_X_inc ~ normal(0, 1);
    v_stt_X_eth_X_inc_Intercept_std ~ normal(0, 1);
    sigma_v_inc_X_age_Intercept_inc_X_age ~ normal(0, 1);
    v_inc_X_age_Intercept_std ~ normal(0, 1);
    sigma_v_eth_X_age_Intercept_eth_X_age ~ normal(0, 1);
    v_eth_X_age_Intercept_std ~ normal(0, 1);
    sigma_v_eth_X_inc_Intercept_eth_X_inc ~ normal(0, 1);
    v_eth_X_inc_Intercept_std ~ normal(0, 1);
    sigma_v_stt_X_age_Intercept_stt_X_age ~ normal(0, 1);
    v_stt_X_age_Intercept_std ~ normal(0, 1);
    sigma_v_stt_X_inc_Intercept_stt_X_inc ~ normal(0, 1);
    v_stt_X_inc_Intercept_std ~ normal(0, 1);
    sigma_v_stt_X_eth_Intercept_stt_X_eth ~ normal(0, 1);
    v_stt_X_eth_Intercept_std ~ normal(0, 1);
    sigma_v_reg_X_age_Intercept_reg_X_age ~ normal(0, 1);
    v_reg_X_age_Intercept_std ~ normal(0, 1);
    sigma_v_reg_X_inc_Intercept_reg_X_inc ~ normal(0, 1);
    v_reg_X_inc_Intercept_std ~ normal(0, 1);
    sigma_v_reg_X_eth_Intercept_reg_X_eth ~ normal(0, 1);
    v_reg_X_eth_Intercept_std ~ normal(0, 1);
    L_Rho_age ~ lkj_corr_cholesky(2);
    sigma_age ~ normal(0, 1);
    to_vector(m_z_age_z_inc_std) ~ normal(0, 1);
    L_Rho_eth ~ lkj_corr_cholesky(2);
    sigma_eth ~ normal(0, 1);
    to_vector(m_z_eth_z_inc_std) ~ normal(0, 1);
    L_Rho_stt ~ lkj_corr_cholesky(2);
    sigma_stt ~ normal(0, 1);
    to_vector(m_z_stt_z_inc_std) ~ normal(0, 1);
    L_Rho_reg ~ lkj_corr_cholesky(2);
    sigma_reg ~ normal(0, 1);
    to_vector(m_z_reg_z_inc_std) ~ normal(0, 1);
    sigma_v_inc_Intercept_inc ~ normal(0, 1);
    v_inc_Intercept_std ~ normal(0, 1);
    b_z_inc_X_z_trnprv ~ normal(0, 10);
    b_z_inc_X_z_incstt ~ normal(0, 10);
    b_z_trnprv ~ normal(0, 10);
    b_z_incstt ~ normal(0, 10);
    b_z_inc ~ normal(0, 10);
    Intercept ~ normal(0, 10);
    for (i in 1:N) {
        p[i] <- Intercept + b_z_inc * z_inc[i] + b_z_incstt * z_incstt[i] + b_z_trnprv * 
            z_trnprv[i] + b_z_inc_X_z_incstt * z_inc_X_z_incstt[i] + b_z_inc_X_z_trnprv * 
            z_inc_X_z_trnprv[i] + m_z_age_z_inc[age[i],1] + m_z_age_z_inc[age[i],2] * z_inc[i] +
            m_z_eth_z_inc[eth[i],1] + m_z_eth_z_inc[eth[i],2] * z_inc[i] + 
            m_z_stt_z_inc[stt[i],1] + m_z_stt_z_inc[stt[i],2] * z_inc[i] + 
            m_z_reg_z_inc[reg[i],1] + m_z_reg_z_inc[reg[i],2] * z_inc[i] + 
            v_inc_Intercept[inc[i]] + v_reg_X_eth_Intercept[reg_X_eth[i]] + 
            v_reg_X_inc_Intercept[reg_X_inc[i]] + v_reg_X_age_Intercept[reg_X_age[i]] + 
            v_stt_X_eth_Intercept[stt_X_eth[i]] + v_stt_X_inc_Intercept[stt_X_inc[i]] + 
            v_stt_X_age_Intercept[stt_X_age[i]] + v_eth_X_inc_Intercept[eth_X_inc[i]] + 
            v_eth_X_age_Intercept[eth_X_age[i]] + v_inc_X_age_Intercept[inc_X_age[i]] + 
            v_stt_X_eth_X_inc_Intercept[stt_X_eth_X_inc[i]] + v_stt_X_eth_X_age_Intercept[stt_X_eth_X_age[i]] + 
            v_stt_X_inc_X_age_Intercept[stt_X_inc_X_age[i]] + v_eth_X_inc_X_age_Intercept[eth_X_inc_X_age[i]];
    }
    yes ~ binomial_logit(yes_size , p);
}
generated quantities{
    vector[N] p;
    real dev;
    dev <- 0;
    for (i in 1:N) {
        p[i] <- Intercept + b_z_inc * z_inc[i] + b_z_incstt * z_incstt[i] + b_z_trnprv * 
            z_trnprv[i] + b_z_inc_X_z_incstt * z_inc_X_z_incstt[i] + b_z_inc_X_z_trnprv * 
            z_inc_X_z_trnprv[i] + m_z_age_z_inc[age[i],1] + m_z_age_z_inc[age[i],2] * z_inc[i] +
            m_z_eth_z_inc[eth[i],1] + m_z_eth_z_inc[eth[i],2] * z_inc[i] + 
            m_z_stt_z_inc[stt[i],1] + m_z_stt_z_inc[stt[i],2] * z_inc[i] + 
            m_z_reg_z_inc[reg[i],1] + m_z_reg_z_inc[reg[i],2] * z_inc[i] + 
            v_inc_Intercept[inc[i]] + v_reg_X_eth_Intercept[reg_X_eth[i]] + 
            v_reg_X_inc_Intercept[reg_X_inc[i]] + v_reg_X_age_Intercept[reg_X_age[i]] + 
            v_stt_X_eth_Intercept[stt_X_eth[i]] + v_stt_X_inc_Intercept[stt_X_inc[i]] + 
            v_stt_X_age_Intercept[stt_X_age[i]] + v_eth_X_inc_Intercept[eth_X_inc[i]] + 
            v_eth_X_age_Intercept[eth_X_age[i]] + v_inc_X_age_Intercept[inc_X_age[i]] + 
            v_stt_X_eth_X_inc_Intercept[stt_X_eth_X_inc[i]] + v_stt_X_eth_X_age_Intercept[stt_X_eth_X_age[i]] + 
            v_stt_X_inc_X_age_Intercept[stt_X_inc_X_age[i]] + v_eth_X_inc_X_age_Intercept[eth_X_inc_X_age[i]];
    }
    dev <- dev + (-2)*binomial_logit_log( yes , yes_size , p);
}

