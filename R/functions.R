# A modified version of findbeta ----------------------------------------------
findbeta2 <- function (themean = NULL, themedian = NULL, themode = NULL, percentile = 0.95, 
          lower.v = F, percentile.value) 
{
  stopifnot((is.null(themean) + is.null(themedian) + is.null(themode)) == 
              2)
  if (is.null(themode) && is.null(themedian)) {
    stopifnot((lower.v == T && themean <= percentile.value) | 
                (lower.v == F && themean >= percentile.value))
  }
  else if (is.null(themean) && is.null(themode)) {
    stopifnot((lower.v == T && themedian <= percentile.value) | 
                (lower.v == F && themedian >= percentile.value))
  }
  else {
    stopifnot((lower.v == T && themode <= percentile.value) | 
                (lower.v == F && themode >= percentile.value))
  }
  a = runif(1, 1, 10)
  if (lower.v == T) {
    pr_n = percentile
  }
  else {
    pr_n = 1 - percentile
  }
  if (is.null(themode) && is.null(themedian)) {
    to.minimize <- function(a) {
      abs(qbeta(pr_n, shape1 = a, shape2 = a * (1 - themean)/themean) - 
            percentile.value)
    }
  }
  else if (is.null(themean) && is.null(themode)) {
    to.minimize <- function(a) {
      abs(qbeta(pr_n, shape1 = a, shape2 = (3 * a * (1 - 
                                                       themedian) + 2 * themedian - 1)/(3 * themedian)) - 
            percentile.value)
    }
  }
  else {
    to.minimize <- function(a) {
      abs(qbeta(pr_n, shape1 = a, shape2 = (a * (1 - themode) + 
                                              2 * themode - 1)/themode) - percentile.value)
    }
  }
  estimate <- optim(runif(1, 1, 10), to.minimize, lower = 0.1, 
                    upper = 10^4, method = "Brent")
  finalshape1 = estimate$par
  if (is.null(themode) && is.null(themedian)) {
    finalshape2 = finalshape1 * (1 - themean)/themean
  }
  else if (is.null(themean) && is.null(themode)) {
    finalshape2 = (3 * finalshape1 * (1 - themedian) + 2 * 
                     themedian - 1)/(3 * themedian)
  }
  else {
    finalshape2 = (finalshape1 * (1 - themode) + 2 * themode - 
                     1)/themode
  }
  shapes <- c(round(finalshape1, 2), round(finalshape2, 2))
  
  return(shapes)
}

# A modified version of findbetaqq --------------------------------------------

findbetaqq2 <- function (percentile.value1, percentile1, percentile.value2, percentile2) 
{
  findcentiles <- function(x) {
    c(F1 = qbeta(percentile1, x[1], x[2]) - percentile.value1, 
      F2 = qbeta(percentile2, x[1], x[2]) - percentile.value2)
  }
  
  ss <- multiroot(f = findcentiles, start = c(1, 1))
  finalshape1 = ss$root[1]
  finalshape2 = ss$root[2]
  sample_beta = rbeta(10000, finalshape1, finalshape2)
  shapes <- c(round(finalshape1, 2), round(finalshape2, 2))
  
  return(shapes)
}

# Test
findbetaqq2(percentile.value1=0.30, percentile1=0.20, percentile.value2=0.60, percentile2=0.90)

# logit / inv_logit-----

logit <- function(x) log(x/(1-x))
inv_logit <- function(x) exp(x)/(1+exp(x))

# normal prior on logit Se/Sp ----

normal_prior <- function(lo95 = NULL, hi95 = NULL) {
  a <-
    matrix(c(1, 1, -1.96, 1.96),
           nrow = 2)
  b <-
    matrix(logit(c(lo95, hi95)),
           nrow = 2)
  d <- as.vector(solve(a, b))
  names(d) <- c("logit_mu", "logit_se")
  
  return(d)
}

# Test
parms <- normal_prior(lo95=0.95, hi95=0.99)
parms

# Simulating 1 imperfect test with differential miss-classification ------

sim_ve_1t_diff <- function(n = 1000,
                           base_dis_prev = 0.05,
                           true_OR = 0.1,
                           covariates = F,
                           vax_prob = 0.75,
                           Se_V = 1,
                           Sp_V = 1,
                           Se_nV = 1,
                           Sp_nV = 1) {
  
  # generate 2 covariates
  x_cont <- rnorm(n, mean = 0, sd = 1)
  x_bin <- rbinom(n, size = 1, prob = 0.2)
  x <- cbind(x_cont, x_bin)
  x
  
  # Simulate propensity to be vaccinated
  a0 = log(vax_prob/(1-vax_prob))
  if (covariates == T) {vax.beta <- a0 + 0.25 * x_cont  - 0.25 * x_bin}
  else if (covariates == F){vax.beta <- a0} 
  
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
    rbinom(n, 1, prob = truestatus * vax * Se_V + (1 - truestatus) * vax * (1 - Sp_V) + 
             truestatus * (1-vax) * Se_nV + (1 - truestatus) * (1-vax) * (1 - Sp_nV))
  
  df <- data.frame(truestatus, dis , vax, x)
  
  t <- as.numeric(table(df$vax, df$dis))
  
  estimated_OR <- (t[1] * t[4]) / (t[2] * t[3])
  
  y <- rbind(as.numeric(table(df[df$vax==0,]$dis)), 
             as.numeric(table(df[df$vax==1,]$dis))) %>% 
    as.data.frame() %>%
    select(V2, V1) %>%
    as.matrix()
  colnames(y) <- c("T+","T-")
  rownames(y) <- c("V-","V+")
  
  t <- as.numeric(table(df$vax, df$dis))
  
  estimated_OR <- (t[1] * t[4]) / (t[2] * t[3])
  
  d <- list("estimated OR" = estimated_OR,
            "true OR" = true_OR,
            "ipd_data" = df,
            "data" = y)
  return(d)
  
}

# tests
sim <- sim_ve_1t_diff(covariates = F, Sp_nV=0.75, n=100000, true_OR = 0.1)
sim$`estimated OR`
sim$`estimated OR T2`
sim$`true OR`
sim$data

# Simulating 1 imperfect test with non-differential miss-classification -------

sim_ve_1t <- function(n = 1000,
                      base_dis_prev = 0.1,
                      true_OR = 0.1,
                      covariates = F,
                      vax_prob = 0.75,
                      Se = 1,
                      Sp = 1) {
  
  # generate 2 covariates
  x_cont <- rnorm(n, mean = 0, sd = 1)
  x_bin <- rbinom(n, size = 1, prob = 0.2)
  x <- cbind(x_cont, x_bin)
  x
  
  # Simulate propensity to be vaccinated
  a0 = log(vax_prob/(1-vax_prob))
  if (covariates == T) {vax.beta <- a0 + 0.25 * x_cont  - 0.25 * x_bin}
  else if (covariates == F) {vax.beta <- a0} 
  
  vax.prob <- exp(vax.beta) / (1 + exp(vax.beta))
  vax <- rbinom(n, 1, prob = vax.prob)
  
  # Simulate propensity to infections (vax + covariates related to no protection)
  b0 <- log(base_dis_prev/(1-base_dis_prev ))
  if (covariates == T){dis.beta <- b0 + log(true_OR) * vax + 0.25 * x_cont  - 0.25 * x_bin}
  else if (covariates == F) {dis.beta <- b0 + log(true_OR) * vax}
  
  dis.prob <- exp(dis.beta) / (1 + exp(dis.beta))
  truestatus <- rbinom(n, 1, dis.prob)
  # Disease diagnosed with imperfect test
  dis <- rbinom(n, 1, prob = truestatus * Se + (1 - truestatus) * (1 - Sp))
  
  df <- data.frame(truestatus, dis, vax, x)
  
  y <- rbind(as.numeric(table(df[df$vax==0,]$dis)), 
             as.numeric(table(df[df$vax==1,]$dis))) %>% 
    as.data.frame() %>%
    select(V2, V1) %>%
    as.matrix()
  colnames(y) <- c("T+","T-")
  rownames(y) <- c("V-","V+")
  
  t <- as.numeric(table(df$vax, df$dis))
  
  estimated_OR <- (t[1] * t[4]) / (t[2] * t[3])
  
  d <- list("estimated OR" = estimated_OR,
            "true OR" = true_OR,
            "ipd_data" = df,
            "data" = y)
  return(d)
}

# tests
sim <- sim_ve_1t(covariates = F, n=1000, true_OR = 0.1)
round(sim$`estimated OR`, 3)
sim$`true OR`
t <- table(sim$ipd_data$vax, sim$ipd_data$truestatus)
t; prop.table(t, 1); prop.table(t, 2)


# Simulating 2 imperfect tests with non-differential miss-classification ------

sim_ve_2t <- function(n = 1000,
                      base_dis_prev = 0.05,
                      true_OR = 0.1,
                      covariates = F,
                      vax_prob = 0.75,
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
  a0 = log(vax_prob/(1-vax_prob))
  if (covariates == T) {vax.beta <- a0 + 0.25 * x_cont  - 0.25 * x_bin}
  else if (covariates == F){vax.beta <- a0} 
  
  vax.prob <- exp(vax.beta) / (1 + exp(vax.beta))
  vax      <- rbinom(n, 1, prob = vax.prob)
  
  # Simulate propensity to infections (vax + covariates related to no protection)
  b0 = log(base_dis_prev/(1-base_dis_prev ))
  if (covariates == T){dis.beta <- b0 + log(true_OR) * vax + 0.25 * x_cont  - 0.25 * x_bin}
  else if (covariates == F) {dis.beta <- b0 + log(true_OR) * vax}
  
  dis.prob <- exp(dis.beta) / (1 + exp(dis.beta))
  truestatus <- rbinom(n, 1, dis.prob)
  # Disease diagnosed with imperfect tests
  dis1 <- rbinom(n, 1, prob = truestatus * Se1 + (1 - truestatus) * (1 - Sp1))
  dis2 <- rbinom(n, 1, prob = truestatus * Se2 + (1 - truestatus) * (1 - Sp2))
  
  df <- data.frame(truestatus, dis1 , dis2, vax, x)
  
  t1 <- as.numeric(table(df$vax, df$dis1))
  t2 <- as.numeric(table(df$vax, df$dis2))
  
  estimated_OR1 <- (t1[1] * t1[4]) / (t1[2] * t1[3])
  estimated_OR2 <- (t2[1] * t2[4]) / (t2[2] * t2[3])
  
  y <- rbind(as.numeric(table(df[df$vax==0,]$dis1, df[df$vax==0,]$dis2)), 
             as.numeric(table(df[df$vax==1,]$dis1, df[df$vax==1,]$dis2))) %>% 
    as.data.frame() %>%
    select(V4, V2, V3, V1) %>%
    as.matrix()
  colnames(y) <- c("T1+/T2+","T1+/T2-","T1-/T2+","T1-/T2-")
  rownames(y) <- c("V+","V-")
  
  d <- list("estimated OR T1" = estimated_OR1,
            "estimated OR T2" = estimated_OR2,
            "true OR" = true_OR,
            "ipd_data" = df,
            "data" = y)
  
  return(d)
  
}

# tests
sim <- sim_ve_2t(covariates = F, Se1=0.8, Sp1=0.95, Sp2=0.9, n=100000, true_OR = 0.2)
sim <- sim_ve_2t(covariates = F, n=100000, true_OR = 0.2)
round(sim$`estimated OR T1`,3)
round(sim$`estimated OR T2`,3)
sim$`true OR`
sim$data
sim$ipd_data


# Simulating 2 imperfect tests with differential miss-classification ------

sim_ve_2t_diff <- function(n = 1000,
                           base_dis_prev = 0.05,
                           true_OR = 0.1,
                           covariates = F,
                           vax_prob = 0.75,
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
  a0 = log(vax_prob/(1-vax_prob))
  if (covariates == T) {vax.beta <- a0 + 0.25 * x_cont  - 0.25 * x_bin}
  else if (covariates == F){vax.beta <- a0} 
  
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
  
  y <- rbind(as.numeric(table(df[df$vax==0,]$dis1, df[df$vax==0,]$dis2)), 
             as.numeric(table(df[df$vax==1,]$dis1, df[df$vax==1,]$dis2))) %>% 
    as.data.frame() %>%
    select(V4, V2, V3, V1) %>%
    as.matrix()
  colnames(y) <- c("T1+/T2+","T1+/T2-","T1-/T2+","T1-/T2-")
  rownames(y) <- c("V+","V-")
  
  d <- list("estimated OR T1" = estimated_OR1,
            "estimated OR T2" = estimated_OR2,
            "true OR" = true_OR,
            "ipd_data" = df,
            "data" = y)
  
  return(d)
  
}

# tests
sim <- sim_ve_2t_diff(covariates = F, Sp2_nV=0.75, n=10000, true_OR = 0.1)
sim$`estimated OR T1`
sim$`estimated OR T2`
sim$`true OR`
sim$data
sim$ipd_data
