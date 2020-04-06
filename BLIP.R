# This function solves BLIF by using EM
# t: n-by-1 vector for the number of hot reviews
# d: n-by-1 vector for the condition of the intro
# z: n-by-1 vector, latent variable show the class of
# the number of followers
# y: n-by-1 vector, latent variable show whether the user is
# an opinion leader of not

# u, v: parameters show the relations between z and y
# x: n-by-m design matrix for random effects
# gamma_0, gamma_1: the probability of the opinion leaders
# lambda/MU parameters for the poisson
# have a related intro
# 2020/01 @Chengdu

# BLIF <- function(t, d, x) {

rm(list = ls())
# source('~/R/PHD/BLIF.R')
User_infor <- read.table("~/R/PHD/User_infor.txt", 
                         header = T, sep = "", stringsAsFactors = F)
User_infor <- User_infor[User_infor$Watched_movies>0,] # Discard the loged out users
User_infor <- User_infor[is.na(as.Date(User_infor$Date_joint)) == 0, ]

load("~/R/PHD/Num_phrase.RData") #The phrase informatio



# Read user_list and discard the loged out users
User_list <- read.table("~/R/PHD/User_list.txt", 
                        header = T, sep = ",", stringsAsFactors = F)
User_list <- User_list[which(is.na(match(User_list$UserID2, 
                                          User_infor$ID)) == 0), ]

# User freq
user_freq <- aggregate(User_list$ID, list(User_list$UserID2), length)
user_freq <- user_freq[order(-user_freq$x), ]

User_infor <- User_infor[match(user_freq$Group.1, User_infor$ID), ]

t <- user_freq$x - 1
d <- User_infor$User_leader

# Chinese phrase
num_phrase$User <- as.character(num_phrase$User)
phrase <- num_phrase[match(User_infor$ID, num_phrase$User), ]##

# Baysian averaging of chinese phrase
b_m <- dim(User_list)[1] / dim(User_infor)[1] #每个用户平均发帖次数
b_c <- sum(phrase$Freq) / dim(User_list)[1] #每个帖子平均成语

phrase_temp <- (phrase$Freq + b_c * b_m) / (phrase$Len + b_m)
phrase <- cbind(phrase, b_mean = phrase_temp)

# Get the number of words in comments
n_words <- nchar(User_list$Short_comments)
temp <- data.frame(User = User_list$UserID2, num = n_words)

# Baysian averaging of words
b_c <- sum(n_words) / length(n_words) #每条评论平均字数

temp <- aggregate(temp$num, list(temp$User), sum)
n_words <- temp$x[match(User_infor$ID, temp$Group.1)] # Average words for each user
n_words <- (n_words + b_c * b_m) / (b_m + phrase$Len)

# Baysian averaging of vote
n_vote <- aggregate(User_list$Comm_vote, 
                    list(User_list$UserID2), mean) # 用于平均热评字数
n_vote <- n_vote[match(User_infor$ID, n_vote$Group.1), ]

b_c <- sum(n_vote$x * user_freq$x) / dim(User_list)[1]
n_vote$x <- (n_vote$x * user_freq$x + b_c * b_m) /
  (phrase$Len + b_m)

# Features
x <- cbind(User_infor$Reviews, User_infor$Watched_movies,
           User_infor$Intro_length, User_infor$Followed,
           n_vote$x, n_words, phrase_temp)
x <- log(x + 1)
x <- scale(x) # Z-score

time1 <- Sys.time()

## Begin optimization

# BLIF(t, d, x)

# Initialization for parameters
n <- length(t)
x <- as.matrix(x)
int <- rep(1, n) # Intercept
x <- cbind(x,int)
colnames(x) <- c("Reviews", "Watched movies",
                 "Intro length", "Followed users",
                 "Ave Votes", "Ave words", 
                 "Ave phrases", "Intercept")
m <- dim(x)[2]

## parameters
lambda_1 <- 10 #quantile(t, 0.95, names = F)
lambda_0 <- 1 #quantile(t, 0.05, names = F)
# sigma_1 <- var(which(t > mu_temp))
# sigma_0 <- var(which(t < mu_temp))
gamma_1 <- 0.5
gamma_0 <- 0.1
u <- 0.8
v <- 0.8
w_old <- w_new <- rep(0, m)
w_old[m] <- w_new[m] <- -3 # Prior = 0.05

g_w <- rep(1, m)

## latent
maxite <- 100
l <- rep(0, maxite) # The lower bound

# Update
for (j in 1 : maxite) {
  
  ## E-step
  
  # N1 <- dnorm(t, mu_1, sigma_1)
  # N0 <- dnorm(t, mu_0, sigma_0)
  
  P1 <- dpois(t, lambda_1)
  P0 <- dpois(t, lambda_0)
  
  q11 <- P1 * u / (P1 * u + P0 * (1 - u))
  q10 <- 1 - q11
  q01 <- P1 * (1 - v) / (P1 * (1 - v) + P0 * v)
  q00 <- 1 - q01
  
  #
  omega <- 
    log((u * P1 + (1 - u) * P0) / ((1 - v) * P1 + v * P0)) + 
    c(log(plogis(x %*% w_new) / (1 - plogis(x %*% w_new)))) + 
    d * log(gamma_1 / gamma_0) + 
    (1 - d) * log((1 - gamma_1) / (1 - gamma_0))
  
  pi <- plogis(omega)
  
  #
  q_z <- pi * q11 + (1 - pi) * q01
  
  ## M-step
  
  lambda_1 <- c(t(q_z) %*% t / sum(q_z))
  lambda_0 <- c(t(1 - q_z) %*% t / sum(1 - q_z))
  
  # sigma_1 <- c(t(t - mu_1)^2 %*% q_z / sum(q_z))
  # sigma_0 <- c(t(t - mu_0)^2 %*% (1 - q_z) / (1 - sum(q_z)))
  
  u <- c(t(pi) %*% q11 / sum(pi)) 
  v <- c(t(1 - pi) %*% q00 / sum(1 - pi))
  
  if (v > 1) {
    v <-  1
  }

  if (u > 1) {
    u <-  1
  }
  
  gamma_1 <- c(t(pi) %*% d / sum(pi))
  gamma_0 <- c(t(1 - pi) %*% d / sum(1 - pi))
  
  ### for w
  
  # w_old <- w_new <- rep(0, m)
  # w_old[m] <- w_new[m] <- -3 # Prior = 0.05
  
  g_w <- 1 ########
  while(max(abs(g_w)) > 1e-6) {
    
    w_old <- w_new
    y_w <- plogis(c(x %*% w_old))
    r_w <- diag(y_w * (1 - y_w))
    g_w <- c(t(x) %*% (y_w - pi))
    w_new <- c(w_old - solve(t(x) %*% r_w %*% x) %*% g_w)
    
  }
  
  y_w <- plogis(c(x %*% w_new))
  
  ## lower bound
  l[j] <- 
    sum(pi * log(u * P1 + (1 - u) * P0 + (u * P1 + (1 - u) * P0 < 0)) + 
          (1 - pi) * log((1 - v) * P1 + v * P0 + ((1 - v) * P1 + v * P0 < 0) ) + 
          pi * log(y_w + (y_w == 0)) + (1 - pi) * log(1 - y_w + (y_w == 1)) + 
          pi * d * log(gamma_1) + pi * (1 - d) * log(1 - gamma_1) + 
          ( 1 - pi) * d * log(gamma_0) + (1 - pi) * (1 - d) * log(1 - gamma_0) -
          (pi * log(pi + (pi == 0)) + (1 - pi) * log(1 - pi + (pi == 1))))
  
  if (j > 2) {
    if (abs((l[j] - l[j - 1]) / l[j - 1]) < 1e-6) {
      break
    }
  }
  
}

leader <- data.frame(User = User_infor$ID, pi = pi)

time2 <- Sys.time()

# Logistic test
x_ <- x[, 1:7]
T_test <- summary(glm(pi ~ x_, family = "binomial"))

total_time <- time2 - time1