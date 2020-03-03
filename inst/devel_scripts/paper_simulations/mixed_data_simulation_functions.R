# Exact mixed data functions
dens_x_given_S_is_C_func <- function(x,C_lower,C_upper,xi,Omega,algorithm) {
  # Formula in equation (13) in this paper
  # https://www.jstor.org/stable/pdf/20445223.pdf?refreqid=excelsior%3A9fdbaaf0a8fe22e64418448ad4f8090b
  # letting V = x, and U correspond to the dimensions specified in C
  # C_lower is a vector of length dim with lower bounds for each dimension, C_upper similalry contains the upper bounds
  # Omega is the joint covariance matrix of x and the length of C_lower and C_upper (dim)
  # xi is the joint mean vector of x and the length of C_lower and C_upper (dim)
  # Note: x is always one dimensional

  these_U <- (1:length(C_lower))+1
  these_V <- 1

  Omega_U <- Omega[these_U,these_U,drop=F]
  Omega_V <- Omega[these_V,these_V,drop=F]
  Delta <- Omega[these_V,these_U,drop=F]

  xi_U <- xi[these_U]
  xi_V <- xi[these_V]

  C_lower <- unlist(C_lower)
  C_upper <- unlist(C_upper)

  mean_above <- as.vector(t(Delta)%*%solve(Omega_V)%*%(x-xi_V) + xi_U)
  sigma_above <- Omega_U - t(Delta)%*%solve(Omega_V)%*%Delta

  mean_below <- xi_U
  sigma_below <- Omega_U


  above <- mvtnorm::pmvnorm(lower = C_lower,upper = C_upper,
                            mean = mean_above,
                            sigma = sigma_above,
                            algorithm = algorithm)


  below <- mvtnorm::pmvnorm(lower = C_lower,upper = C_upper,
                            mean = mean_below,
                            sigma = sigma_below,
                            algorithm = algorithm)

  left <- dnorm(x,mean=xi_V,sd = sqrt(Omega_V))

  dens <- left*above/below

  return(dens)

}

# Splitting the main function in two to make it more efficient
# Here is the preparation function
prep_dens_x_given_S_is_C_func <- function(C_lower,C_upper,xi,Omega,algorithm) {

  these_U <- (1:length(C_lower))+1
  these_V <- 1

  Omega_U <- Omega[these_U,these_U,drop=F]
  Omega_V <- Omega[these_V,these_V,drop=F]
  Delta <- Omega[these_V,these_U,drop=F]

  xi_U <- xi[these_U]
  xi_V <- xi[these_V]

  C_lower <- unlist(C_lower)
  C_upper <- unlist(C_upper)

  mean_above_mult <- t(Delta)%*%solve(Omega_V)
  mean_above_add <- xi_U - t(Delta)%*%solve(Omega_V)%*%xi_V
  sigma_above <- Omega_U - t(Delta)%*%solve(Omega_V)%*%Delta

  mean_below <- xi_U
  sigma_below <- Omega_U

  below <- mvtnorm::pmvnorm(lower = C_lower,upper = C_upper,
                            mean = mean_below,
                            sigma = sigma_below,
                            algorithm = algorithm)


  left_mean <- xi_V
  left_sd <- sqrt(Omega_V)

  ret <- list(algorithm = algorithm,
              C_lower = C_lower,
              C_upper = C_upper,
              mean_above_mult = mean_above_mult,
              mean_above_add = mean_above_add,
              sigma_above = sigma_above,
              below = below,
              left_mean = left_mean,
              left_sd = left_sd)

  return(ret)

}

prep_dens_x_given_S_is_C_func_v2 <- function(C_lower, C_upper,xi,Omega,algorithm) {

  C_lower <- unlist(C_lower)
  C_upper <- unlist(C_upper)

  these_U <- (1:length(C_lower))+1
  these_V <- 1

  Omega_U <- Omega[these_U,these_U,drop=F]
  Omega_V <- Omega[these_V,these_V,drop=F]
  Delta <- Omega[these_V,these_U,drop=F]

  xi_U <- xi[these_U]
  xi_V <- xi[these_V]


  mean_above_mult <- t(Delta)%*%solve(Omega_V)
  mean_above_add <- xi_U - t(Delta)%*%solve(Omega_V)%*%xi_V
  sigma_above <- Omega_U - t(Delta)%*%solve(Omega_V)%*%Delta

  mean_below <- xi_U
  sigma_below <- Omega_U

  below <- mvtnorm::pmvnorm(lower = C_lower,upper = C_upper,
                            mean = mean_below,
                            sigma = sigma_below,
                            algorithm = algorithm)


  left_mean <- xi_V
  left_sd <- sqrt(Omega_V)

  ret <- list(algorithm = algorithm,
              C_lower = C_lower,
              C_upper = C_upper,
              mean_above_mult = mean_above_mult,
              mean_above_add = mean_above_add,
              sigma_above = sigma_above,
              below = below,
              left_mean = left_mean,
              left_sd = left_sd)

  return(ret)

}

# Here is the computation function, taking the preparation values as input
compute_dens_x_given_S_is_C_func <- function(x,ret_list) {

  mean_above <- as.vector(ret_list$mean_above_mult%*%x+ret_list$mean_above_add)


  above <- mvtnorm::pmvnorm(lower = ret_list$C_lower,upper = ret_list$C_upper,
                            mean = mean_above,
                            sigma = ret_list$sigma_above,
                            algorithm = algorithm)

  left <- dnorm(x,mean=ret_list$left_mean,sd = ret_list$left_sd)

  dens <- left*above/ret_list$below

  return(dens)

}

# Vectorizing the two functions and checking that they give the same result
vec_dens_x_given_S_is_C_func <- Vectorize(dens_x_given_S_is_C_func,vectorize.args="x")

vec_compute_dens_x_given_S_is_C_func = Vectorize(compute_dens_x_given_S_is_C_func,vectorize.args = "x")

vec_compute_dens_x_given_S_is_C_func_2 = Vectorize(compute_dens_x_given_S_is_C_func)

#aa=outer(x_int_grid,prep_list_all_x_test_C,FUN=vec_compute_dens_x_given_S_is_C_func_2)

vec_compute_dens_x_given_S_is_C_func_rev <- function(ret_list,x){
  vec_compute_dens_x_given_S_is_C_func(x,ret_list)
}
