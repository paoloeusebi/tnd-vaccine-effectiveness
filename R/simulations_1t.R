n_sim <- 1000
load.module("lecuyer")
inits1 <- list(.RNG.name = "lecuyer::RngStream", .RNG.seed = 2020)
inits2 <- list(.RNG.name = "lecuyer::RngStream", .RNG.seed = 2021)
inits3 <- list(.RNG.name = "lecuyer::RngStream", .RNG.seed = 2022)

initial_seed <- as.integer(as.Date("2022-01-01"))
seeds <- initial_seed + 1:n_sim

parms_ <- expand.grid(
  sim = 1:n_sim,
  n = c(10000),
  p1 = c(0.25),
  Sp_V = c(0.99, 0.99999), 
  Sp_nV = c(0.99, 0.99999),
  Se_V = c(0.925, 0.975),
  Se_nV = c(0.925, 0.975),
  logit_se_Sp = c(0.1, 0.00001),
  logit_se_Se = c(0.2),
  true_OR = c(0.1, 0.2)
)

parms <- parms_ %>%
  filter(
    !Sp_V!=Sp_nV,
    !(Sp_V==0.99 & logit_se_Sp==0.00001),
    !(Sp_V==0.99999 & logit_se_Sp==0.1)
    )

parms$seed <- initial_seed + parms$sim

f <- as.numeric(row.names(parms))

s <- split(parms, f)

d0 <- mclapply(split(parms, f), function(x) {
  set.seed(x$seed)
  d <- sim_ve_1t_diff(
    # simulation of data
    base_dis_prev = x$p1,
    true_OR = x$true_OR,
    Se_V = x$Se_V,
    Sp_V = x$Sp_V,
    Se_nV =  x$Se_nV,
    Sp_nV = x$Sp_nV,
    n = x$n
  )
  
  y <- d$data
  m = 2
  N = apply(y, 1, sum)
  
  HPSe_V <- c(x$Se_V, 1 / (x$logit_se_Se ^ 2))
  HPSp_V <- c(x$Sp_V, 1 / (x$logit_se_Sp ^ 2))
  HPSe_nV <- c(x$Se_nV, 1 / (x$logit_se_Se ^ 2))
  HPSp_nV <- c(x$Sp_nV, 1 / (x$logit_se_Sp ^ 2))
  
  results <- autorun.jags(
    bm_1t_dif,
    data = list(
      N = N,
      y = y,
      HPSe_V = HPSe_V,
      HPSp_V = HPSp_V,
      HPSe_nV = HPSe_nV,
      HPSp_nV = HPSp_nV
    ),
    inits = list(inits1, inits2, inits3),
    progress.bar = 'none',
    silent.jags = T
  )
  
  return(data.frame(cbind(
    parameter = row.names(results$summaries),
    round(results$summaries, 3), # results from JAGS
    raw_OR = d$`estimated OR`,   # raw estimate assuming perfect test
    x                           # parameters of simulation scenario
  )))
})

# All the simulations results in one file
d1 <- data.table::rbindlist(d0)
write.csv(d1, "tfls/simulations_1t.csv", row.names = F)
