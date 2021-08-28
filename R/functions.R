gen <- function(n=1000,
                true_OR=0.1,
                Se=0.95, 
                Sp=0.95){
  # generate 2 covariates
  x_cont <- rnorm(n, mean=0, sd = 1)
  x_bin <- rbinom(n, size = 1, prob = 0.2)
  x <- cbind(x_cont, x_bin)
  x
  
  # Simulate propensity to be vaccinated
  vax.beta <- 0.5 + 0.25 * x_cont  - 0.25 * x_bin
  vax.prob <- exp(vax.beta) / (1 + exp(vax.beta))
  vax      <- rbinom(n, 1, prob = vax.prob)
  
  # Simulate propensity to infections (vax + covariates related to no protection)
  dis.beta <- 0.5 + log(true_OR)*vax + 0.25 * x_cont  - 0.25 * x_bin
  dis.prob <- exp(dis.beta ) / (1 + exp(dis.beta ))
  
  # Disease diagnosed with imperfect reference
  dis <- rbinom(n, 1, prob = dis.prob*Se+(1-dis.prob)*(1-Sp))
  
  df <- data.frame(vax, dis ,x)
  
  t <- as.numeric(table(df$vax, df$dis))
  
  estimated_OR <- (t[1]*t[4])/(t[2]*t[3])
  
  df <- c("estimated OR"=estimated_OR,
                  "true OR"=true_OR)
  
  return(df)
  
}


