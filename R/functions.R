sim_ve_imperfect_tests <- function(n = 1000,
                                   base_dis_prev = 0.05,
                                   true_OR = 0.1,
                                   covariates = F,
                                   Se1 = 1,
                                   Sp1 = 1,
                                   Se2 = 1,
                                   Sp2 = 1) {
  # generate 2 covariates
  x_cont <- rnorm(n, mean = 0, sd = 1)
  x_bin <- rbinom(n, size = 1, prob = 0.2)
  x <- cbind(x_cont, x_bin)
  x
  
  # Simulate propensity to be vaccinated
  if (covariates == T) {vax.beta <- 0.5 + 0.25 * x_cont  - 0.25 * x_bin}
  else if (covariates == F){vax.beta <- 0.5} 
  
  vax.prob <- exp(vax.beta) / (1 + exp(vax.beta))
  vax      <- rbinom(n, 1, prob = vax.prob)
  
  # Simulate propensity to infections (vax + covariates related to no protection)
  b0 = log(base_dis_prev/(1-base_dis_prev ))
  if (covariates == T){dis.beta <- b0 + log(true_OR) * vax + 0.25 * x_cont  - 0.25 * x_bin}
  else if (covariates == F) {dis.beta <- b0 + log(true_OR) * vax}
  
  dis.prob <- exp(dis.beta) / (1 + exp(dis.beta))
  truestatus <- rbinom(n, 1, dis.prob)
  # Disease diagnosed with imperfect tests
  dis1 <-
    rbinom(n, 1, prob = truestatus * Se1 + (1 - truestatus) * (1 - Sp1))
  dis2 <-
    rbinom(n, 1, prob = truestatus * Se2 + (1 - truestatus) * (1 - Sp2))
  
  df <- data.frame(truestatus, dis1 , dis2, vax, x)
  
  t1 <- as.numeric(table(df$vax, df$dis1))
  t2 <- as.numeric(table(df$vax, df$dis2))
  
  estimated_OR1 <- (t1[1] * t1[4]) / (t1[2] * t1[3])
  estimated_OR2 <- (t2[1] * t2[4]) / (t2[2] * t2[3])
  
  d <- list("estimated OR T1" = estimated_OR1,
            "estimated OR T2" = estimated_OR2,
            "true OR" = true_OR,
            "data" = df)
  
  return(d)
  
}

# tests
sim <- sim_ve_imperfect_tests(covariates = F, Sp2=0.75, n=10000, true_OR = 0.1)
sim$`estimated OR T1`
sim$`estimated OR T2`
sim$`true OR`
# sim_ve_imperfect_tests(covariates = T)
# sim_ve_imperfect_tests(covariates = F, n=10000000)
# sim_ve_imperfect_tests(covariates = F, Sp2=0.75, n=10000000)


