##### Functions calculating wavespeeds --------------------------------------------------------------------

# Code adapted from Zhang et al. (2011), Teller et al. (2016)
# See Zhang et al. (2011) Table S2 for vital rates

# Note some differences compared to the pubs referenced above
# *We use a vegetation height of 0.15 m instead of 0.5 m
# *We use empirical distributions of wind speeds rather than a lognormal estimation
# *We use distributions of flower height (all flowers or max) instead of a single value

# Function to calculate wave speed
wv.StructuredRate <- function(B0, Ms, disdist, seqmax){
  
  # Internal function to get matrix of dispersal kernel MGFs
  i.getMs <- function(ss){
    
    # Internal function to calculate empirical MGF
    i.MGF <- function(disdist, ss, n){
      return(1/n*sum(exp(ss*disdist)))}
    
    # Calculate empirical MGF
    # Cap out the rare instances where MGF is not finite
    m <- i.MGF(disdist, ss, length(disdist))
    if(m == "Inf"){print(m); m <- 1000}
    Ms[Ms == 0] <- m
    return(Ms)}
  
  # Internal function to get eigenvalue for Hadamard product of MGF and demographic matrices
  i.getRho <- function(Ms, B0){
    Hs <- Ms*B0
    rho <- max(Re(eigen(Hs)$value))
    return(rho)}
  
  # Internal function to calculate c for sequence of s values
  i.cFunc <- function(ss, B0){
    Ms <- i.getMs(ss)
    rho <- i.getRho(Ms, B0)
    cc <- 1/ss * log(rho)
    if(cc == Inf) {cc <- 1e307 + ss}
    return(cc)}
  
  # Internal function to get wavespeed c* by minimising objective s
  i.cStar <- function(B0, seqmax){
    seqmin <- 1e-307
    cc <- optimize(i.cFunc, lower = seqmin, upper = seqmax, maximum = FALSE, B0 = B0)
    return(c(cc$minimum, cc$objective))}
  
  # Return wavespeed
  cs <- i.cStar(B0, seqmax)
  return(cs)}

# Get wavespeeds; use 10000 seeds per simulation
wv.cStar <- function(mat, msMat, heights, species){
  dd <- 1000 #default: 100000
  cstar <- numeric(dd)
  for(i in 1:dd){
    r <- WALD.h(100, heights, species)
    x <- wv.marginalise(r, angle = runif(length(r), 0, 2*pi))
    star <- wv.StructuredRate(mat, msMat, x, 1)
    cstar[i] <- star[2]
    if(i %% 100 == 0){
      shell("cls")
      cat(paste0(i, "/", dd, " (", i/dd*100, "%) complete"))}}
  hist(cstar, main = median(cstar))
  return(cstar)}

# Marginalise dispersal along a single spatial axis
wv.marginalise <- function(r, angle) r*cos(angle)

# Construct demographic matrix from known parameters
wv.dMatrix <- function(v) {
  return(matrix(c(v[1], v[2]*v[13]*v[16]*v[23]*v[19]*v[20], v[3]*v[14]*v[17]*v[23]*v[19]*v[20],    # R1, C1-3 
                  v[4]*v[15]*v[18]*v[23]*v[19]*v[20], v[2]*v[13]*v[16]*v[23]*v[19]*v[20],          # R1, C4-5 
                  v[3]*v[14]*v[17]*v[23]*v[19]*v[20], v[4]*v[15]*v[18]*v[23]*v[19]*v[20],          # R1, C6-7 
                  v[2]*v[13]*v[16]*v[23]*v[19]*v[20], v[3]*v[14]*v[17]*v[23]*v[19]*v[20],          # R1, C8-9 
                  v[4]*v[15]*v[18]*v[23]*v[19]*v[20],                                              # R1, C10
                  0, v[2]*v[13]*v[16]*v[23]*v[19]*v[21]*(1-v[5]-v[6])*v[24],                       # R2, C1-2
                  v[3]*v[14]*v[17]*v[23]*v[19]*v[21]*(1-v[5]-v[6])*v[24],                          # R2, C3
                  v[4]*v[15]*v[18]*v[23]*v[19]*v[21]*(1-v[5]-v[6])*v[24],                          # R2, C4
                  v[2]*v[13]*v[16]*v[23]*v[19]*v[21]*(1-v[5]-v[6])*v[24],                          # R2, C5
                  v[3]*v[14]*v[17]*v[23]*v[19]*v[21]*(1-v[5]-v[6])*v[24],                          # R2, C6
                  v[4]*v[15]*v[18]*v[23]*v[19]*v[21]*(1-v[5]-v[6])*v[24],                          # R2, C7
                  v[2]*v[13]*v[16]*v[23]*v[19]*v[21]*(1-v[5]-v[6])*v[24],                          # R2, C8
                  v[3]*v[14]*v[17]*v[23]*v[19]*v[21]*(1-v[5]-v[6])*v[24],                          # R2, C9
                  v[4]*v[15]*v[18]*v[23]*v[19]*v[21]*(1-v[5]-v[6])*v[24],                          # R2, C10
                  0, v[2]*v[13]*v[16]*v[23]*v[19]*v[21]*v[5]*v[24],                                # R3, C1-2
                  v[3]*v[14]*v[17]*v[23]*v[19]*v[21]*v[5]*v[24],                                   # R3, C3
                  v[4]*v[15]*v[18]*v[23]*v[19]*v[21]*v[5]*v[24],                                   # R3, C4
                  v[2]*v[13]*v[16]*v[23]*v[19]*v[21]*v[5]*v[24],                                   # R3, C5
                  v[3]*v[14]*v[17]*v[23]*v[19]*v[21]*v[5]*v[24],                                   # R3, C6
                  v[4]*v[15]*v[18]*v[23]*v[19]*v[21]*v[5]*v[24],                                   # R3, C7
                  v[2]*v[13]*v[16]*v[23]*v[19]*v[21]*v[5]*v[24],                                   # R3, C8
                  v[3]*v[14]*v[17]*v[23]*v[19]*v[21]*v[5]*v[24],                                   # R3, C9
                  v[4]*v[15]*v[18]*v[23]*v[19]*v[21]*v[5]*v[24],                                   # R3, C10
                  0, v[2]*v[13]*v[16]*v[23]*v[19]*v[21]*v[6]*v[24],                                # R4, C1-2
                  v[3]*v[14]*v[17]*v[23]*v[19]*v[21]*v[6]*v[24],                                   # R4, C3
                  v[4]*v[15]*v[18]*v[23]*v[19]*v[21]*v[6]*v[24],                                   # R4, C4
                  v[2]*v[13]*v[16]*v[23]*v[19]*v[21]*v[6]*v[24],                                   # R4, C5
                  v[3]*v[14]*v[17]*v[23]*v[19]*v[21]*v[6]*v[24],                                   # R4, C6
                  v[4]*v[15]*v[18]*v[23]*v[19]*v[21]*v[6]*v[24],                                   # R4, C7
                  v[2]*v[13]*v[16]*v[23]*v[19]*v[21]*v[6]*v[24],                                   # R4, C8
                  v[3]*v[14]*v[17]*v[23]*v[19]*v[21]*v[6]*v[24],                                   # R4, C9
                  v[4]*v[15]*v[18]*v[23]*v[19]*v[21]*v[6]*v[24],                                   # R4, C10	
                  v[22]*(1-v[5]-v[6]), v[2]*(1-v[13])*(1-v[7]-v[8]), v[3]*(1-v[14])*v[9],          # R5, C1-3
                  v[4]*(1-v[15])*v[11], v[2]*(1-v[13])*(1-v[7]-v[8]),	v[3]*(1-v[14])*v[9],         # R5, C4-6
                  v[4]*(1-v[15])*v[11], v[2]*(1-v[13])*(1-v[7]-v[8]), v[3]*(1-v[14])*v[9],         # R5, C7-9
                  v[4]*(1-v[15])*v[11],                                                            # R5, C10
                  v[22]*v[5], v[2]*(1-v[13])*v[7], v[3]*(1-v[14])*(1-v[9]-v[10]),                  # R6, C1-3
                  v[4]*(1-v[15])*v[12], v[2]*(1-v[13])*v[7], v[3]*(1-v[14])*(1-v[9]-v[10]),        # R6, C4-6
                  v[4]*(1-v[15])*v[12],	v[2]*(1-v[13])*v[7], v[3]*(1-v[14])*(1-v[9]-v[10]),        # R6, C7-9
                  v[4]*(1-v[15])*v[12],                                                            # R6, C10
                  v[22]*v[6], v[2]*(1-v[13])*v[8], v[3]*(1-v[14])*v[10],                           # R7, C1-3
                  v[4]*(1-v[15])*(1-v[11]-v[12]), v[2]*(1-v[13])*v[8], v[3]*(1-v[14])*v[10],       # R7, C4-6
                  v[4]*(1-v[15])*(1-v[11]-v[12]),	v[2]*(1-v[13])*v[8], v[3]*(1-v[14])*v[10],       # R7, C7-9
                  v[4]*(1-v[15])*(1-v[11]-v[12]),                                                  # R7, C10
                  0, v[2]*v[13]*v[16]*v[23]*v[19]*v[21]*(1-v[5]-v[6])*(1-v[24]),                   # R8, C1-2
                  v[3]*v[14]*v[17]*v[23]*v[19]*v[21]*(1-v[5]-v[6])*(1-v[24]),                      # R8, C3
                  v[4]*v[15]*v[18]*v[23]*v[19]*v[21]*(1-v[5]-v[6])*(1-v[24]),                      # R8, C4
                  v[2]*v[13]*v[16]*v[23]*v[19]*v[21]*(1-v[5]-v[6])*(1-v[24]),                      # R8, C5
                  v[3]*v[14]*v[17]*v[23]*v[19]*v[21]*(1-v[5]-v[6])*(1-v[24]),                      # R8, C6
                  v[4]*v[15]*v[18]*v[23]*v[19]*v[21]*(1-v[5]-v[6])*(1-v[24]),                      # R8, C7
                  v[2]*v[13]*v[16]*v[23]*v[19]*v[21]*(1-v[5]-v[6])*(1-v[24]),                      # R8, C8
                  v[3]*v[14]*v[17]*v[23]*v[19]*v[21]*(1-v[5]-v[6])*(1-v[24]),                      # R8, C9
                  v[4]*v[15]*v[18]*v[23]*v[19]*v[21]*(1-v[5]-v[6])*(1-v[24]),                      # R8, C10
                  0, v[2]*v[13]*v[16]*v[23]*v[19]*v[21]*v[5]*(1-v[24]),                            # R9, C1-2
                  v[3]*v[14]*v[17]*v[23]*v[19]*v[21]*v[5]*(1-v[24]),                               # R9, C3
                  v[4]*v[15]*v[18]*v[23]*v[19]*v[21]*v[5]*(1-v[24]),                               # R9, C4
                  v[2]*v[13]*v[16]*v[23]*v[19]*v[21]*v[5]*(1-v[24]),                               # R9, C5
                  v[3]*v[14]*v[17]*v[23]*v[19]*v[21]*v[5]*(1-v[24]),                               # R9, C6
                  v[4]*v[15]*v[18]*v[23]*v[19]*v[21]*v[5]*(1-v[24]),                               # R9, C7
                  v[2]*v[13]*v[16]*v[23]*v[19]*v[21]*v[5]*(1-v[24]),                               # R9, C8
                  v[3]*v[14]*v[17]*v[23]*v[19]*v[21]*v[5]*(1-v[24]),                               # R9, C9
                  v[4]*v[15]*v[18]*v[23]*v[19]*v[21]*v[5]*(1-v[24]),                               # R9, C10
                  0, v[2]*v[13]*v[16]*v[23]*v[19]*v[21]*v[6]*(1-v[24]),                            # R10, C1-2
                  v[3]*v[14]*v[17]*v[23]*v[19]*v[21]*v[6]*(1-v[24]),                               # R10, C3
                  v[4]*v[15]*v[18]*v[23]*v[19]*v[21]*v[6]*(1-v[24]),                               # R10, C4
                  v[2]*v[13]*v[16]*v[23]*v[19]*v[21]*v[6]*(1-v[24]),                               # R10, C5
                  v[3]*v[14]*v[17]*v[23]*v[19]*v[21]*v[6]*(1-v[24]),                               # R10, C6
                  v[4]*v[15]*v[18]*v[23]*v[19]*v[21]*v[6]*(1-v[24]),                               # R10, C7
                  v[2]*v[13]*v[16]*v[23]*v[19]*v[21]*v[6]*(1-v[24]),                               # R10, C8
                  v[3]*v[14]*v[17]*v[23]*v[19]*v[21]*v[6]*(1-v[24]),                               # R10, C9
                  v[4]*v[15]*v[18]*v[23]*v[19]*v[21]*v[6]*(1-v[24])),                              # R10, C10
                nrow = 10, ncol = 10, byrow = TRUE))}





##### Set demographic parameters and matrices ----------------------------------------------------------------------

# Matrix to define which demographic stages disperse (0) or not (1)
msMat <- matrix(c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0,         
                  1, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                  1, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                  1, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                  1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                  1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                  1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                  1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                  1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                  1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
                nrow = 10, ncol = 10, byrow = TRUE)

# Set list of demographic parameters
p <- list(sigma1 = 0.2597,    # Survival of seed in Sb
          sigma2 = 0.2619,    # Survival of S
          sigma3 = 0.6761,    # Survival of M
          sigma4 = 0.8971,    # Survival of L
          gamma3 = 0.2076,    # Growth of establishing seed to M
          gamma4 = 0.0911,    # Growth of establishing seed to L
          gamma32 = 0.8028,   # Growth of surviving, not-bolting S to M
          gamma42 = 0.1268,   # Growth of surviving, not-bolting S to L
          gamma43 = 0.3824,   # Growth of surviving, not-bolting M to L
          rho23 = 0,          # Regression of surviving, not-bolting M to S
          rho24 = 0,          # Regression of surviving, not-bolting L to S
          rho34 = 0,          # Regression of surviving, not-bolting L to M
          beta2 = 0.1932,     # Bolting of surviving S
          beta3 = 0.7143,     # Bolting of surviving M
          beta4 = 1,          # Bolting of surviving L
          p2 = 23.06,         # Number of capitula per S
          p3 = 26.05,         # Number of capitula per M
          p4 = 52.73,         # Number of capitula per L
          i = 236,            # Number of seeds per capitulum
          j = 1,              # Proportion of seeds released
          phi = 0.85,         # Potential seed escaping from floral herbivory
          nu = 0.2333,        # New seed entering SB
          epsilon = 0.2333,   # New seed establishing seedling
          epsilon1 = 0.2333)  # Seed from SB establishing seedling

# Construct demographic/dispersal matrices for each scenario
vc <- c(p$sigma1, p$sigma2, p$sigma3, p$sigma4, p$gamma3, p$gamma4, p$gamma32, p$gamma42, p$rho23,
        p$gamma43, p$rho24, p$rho34, p$beta2, p$beta3, p$beta4, p$p2, p$p3, p$p4, p$phi, p$nu,
        p$epsilon, p$epsilon1, p$i, p$j)
vw <- c(p$sigma1, p$sigma2*1.0935, p$sigma3*1.0935, p$sigma4*1.0935, p$gamma3, p$gamma4, p$gamma32,
        p$gamma42, p$rho23, p$gamma43, p$rho24, p$rho34, p$beta2, p$beta3, p$beta4, p$p2*1.3244,
        p$p3*1.3244, p$p4*1.3244, p$phi, p$nu, p$epsilon*1.2953, p$epsilon1*1.2953, p$i, p$j)

# Get population growth rates
max(Re(eigen(wv.dMatrix(vc))$value)) # Unwarmed, lambda = 363
max(Re(eigen(wv.dMatrix(vw))$value)) # Warmed, lambda = 680

# Simulate wavespeeds
set.seed(34952)
cstarc <- wv.cStar(wv.dMatrix(vc), msMat, ht_CN_NW$Height, "CN")
cstarw <- wv.cStar(wv.dMatrix(vw), msMat, ht_CN_W$Height, "CN")
cstarcmax <- wv.cStar(wv.dMatrix(vc), msMat, ht_CN_NW_max$Max, "CN")
cstarwmax <- wv.cStar(wv.dMatrix(vw), msMat, ht_CN_W_max$Max, "CN")

