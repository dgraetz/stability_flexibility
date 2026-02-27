
library(brms)


bayes_formula <- bf(
  v ~ is_bivalent_contr * prev_bivalent_contr * switch_contr + 
    (is_bivalent_contr + prev_bivalent_contr + switch_contr | id)
)


my_priors <- c(
  prior(normal(0, 5), class = "b"),      # Priors for fixed effects
  prior(student_t(3, 0, 2.5), class = "sd"), # Priors for random effect SDs
  prior(lkj(2), class = "cor")           # Prior for the correlation matrix
)

# 3. Run the model
alt_cue_v_bayes <- brm(
  formula = bayes_formula,
  data = alt_cue_agg_ID_ddm_nocycle,
  prior = my_priors,
  family = gaussian(), # 'v' is usually continuous/normally distributed
  chains = 4,          # Number of Markov chains
  iter = 2000,         # Total iterations (including warmup)
  warmup = 1000,       # Burn-in period
  cores = 4,           # Run chains in parallel
  control = list(adapt_delta = 0.95), # Helps with divergent transitions
  file = "alt_cue_v_bayes_model"      # Saves the model to disk
)

# 4. Check results
summary(alt_cue_v_bayes)
plot(alt_cue_v_bayes) # Visual check for chain convergence
