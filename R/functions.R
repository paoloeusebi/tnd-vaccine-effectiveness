sim_ve_2_imperfect_tests <- function(n = 1000,
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
sim <- sim_ve_2_imperfect_tests(covariates = F, Sp2=0.75, n=10000, true_OR = 0.1)
sim$`estimated OR T1`
sim$`estimated OR T2`
sim$`true OR`
# sim_ve_imperfect_tests(covariates = T)
# sim_ve_imperfect_tests(covariates = F, n=10000000)
# sim_ve_imperfect_tests(covariates = F, Sp2=0.75, n=10000000)



sim_ve_imperfect_tests_diff <- function(n = 1000,
                                   base_dis_prev = 0.05,
                                   true_OR = 0.1,
                                   covariates = F,
                                   Se1_V = 1,
                                   Sp1_V = 1,
                                   Se2_V = 1,
                                   Sp2_V = 1,
                                   Se1_nV = 1,
                                   Sp1_nV = 1,
                                   Se2_nV = 1,
                                   Sp2_nV = 1) {
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
    rbinom(n, 1, prob = truestatus * vax * Se1_V + (1 - truestatus) * vax * (1 - Sp1_V) + 
             truestatus * (1-vax) * Se1_nV + (1 - truestatus) * (1-vax) * (1 - Sp1_nV))
  dis2 <-
    rbinom(n, 1, prob = truestatus * vax * Se2_V + (1 - truestatus) * vax * (1 - Sp2_V) + 
             truestatus * (1-vax) * Se2_nV + (1 - truestatus) * (1-vax) * (1 - Sp2_nV))
  
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
sim <- sim_ve_imperfect_tests_diff(covariates = F, Sp2_nV=0.75, n=10000, true_OR = 0.1)
sim$`estimated OR T1`
sim$`estimated OR T2`
sim$`true OR`
# sim_ve_imperfect_tests(covariates = T)
# sim_ve_imperfect_tests(covariates = F, n=10000000)
# sim_ve_imperfect_tests(covariates = F, Sp2=0.75, n=10000000)

data_prep <- function(data){
  df <- d$data
  a <- as.numeric(table(df[df$vax==0,]$dis1, df[df$vax==0,]$dis2))
  b <- as.numeric(table(df[df$vax==1,]$dis1, df[df$vax==1,]$dis2))
  y <- rbind(a, b) %>% 
    as.data.frame %>%
    select(V4, V2, V3, V1) %>%
    as.matrix()
  colnames(y) <- c("T1+/T2+","T1+/T2-","T1-/T2+","T1-/T2-")
  rownames(y) <- c("V+","V-")
  return(y)
}

sim_ve_1_imperfect_test <- function(n = 1000,
                                     base_dis_prev = 0.05,
                                     true_OR = 0.1,
                                     covariates = F,
                                     Se = 1,
                                     Sp = 1) {
  
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
  # Disease diagnosed with imperfect test
  dis <-
    rbinom(n, 1, prob = truestatus * Se + (1 - truestatus) * (1 - Sp))
   
  df <- data.frame(truestatus, dis, vax, x)
  
  t <- as.numeric(table(df$vax, df$dis))
  
  estimated_OR <- (t[1] * t[4]) / (t[2] * t[3])
  
  d <- list("estimated OR" = estimated_OR,
            "true OR" = true_OR,
            "data" = df)
  
  return(d)
  
}

# tests
sim <- sim_ve_1_imperfect_test(covariates = F, Sp=0.75, n=10000, true_OR = 0.1)
sim$`estimated OR`
sim$`true OR`
# sim_ve_imperfect_tests(covariates = T)
# sim_ve_imperfect_tests(covariates = F, n=10000000)
# sim_ve_imperfect_tests(covariates = F, Sp2=0.75, n=10000000)