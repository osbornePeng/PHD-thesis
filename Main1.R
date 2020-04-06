#rm(list = ls())

# Read Data
User_list <- read.table("~/R/PHD/User_list.txt", header = T, sep = ",", stringsAsFactors = F)
Movie_infor <- read.table("~/R/PHD/Movie_infor.txt", header = T, sep = ",", stringsAsFactors = F)
User_infor <- read.table("~/R/PHD/User_infor.txt", header = T, sep = "", stringsAsFactors = F)

# Discard the log out users
User_infor <- User_infor[User_infor$Watched_movies>0,]
User_infor <- User_infor[is.na(as.Date(User_infor$Date_joint)) == 0, ]

User_list <- User_list[which(is.na(match(User_list$UserID2, User_infor$ID))==0),]

# Change the rank to yes or no in Movie_infor
Movie_infor$Movie_rank[Movie_infor$Movie_rank<40] = 0
Movie_infor$Movie_rank[Movie_infor$Movie_rank>60] = 1

#
User_list$Short_comments <- as.character(User_list$Short_comments)

# Remove the vote = 6 in User_list
User_list <- User_list[User_list$User_rating != 6,]
User_list$User_rating[User_list$User_rating < 6] <- 0
User_list$User_rating[User_list$User_rating > 6] <- 1

# Match the ID
Movie_infor$ID <- 1 : dim(Movie_infor)[1]
temp <- unique(User_list$Movie_name)
User_list$ID <- match(User_list$Movie_name, temp)

## For the positive of user
User_list_sub <- User_list[User_list$User_rating == 1, ] ###

#
userl <- unique(User_list_sub$UserID2)

# n <- length(userl)
# J <- dim(Movie_infor)[1]
# r <- matrix(rep(0, n * J), n)
# 
# for (i in 1 : n) {
#   
#   label <- which((1 - is.na(match(
#     User_list_sub$UserID2, userl[i])))==1)
#   
#   movieid <- User_list_sub$ID[label]
#   
#   r[i, movieid] <- 1
#   
# }

# Build the trainset
rank <- Movie_infor$Movie_rank[User_list_sub$ID] # User T, movie T of N ?
train_set <- data.frame(User = User_list_sub$UserID2, rank = rank)
pi_sub <- leader$pi[match(train_set$User, leader$User)]
train_set <- cbind(train_set, pi_sub)
colnames(train_set)[3] <- "pi"

# Mixture of the Bernoulli
maxiter <- 1
a1 <- 0.8
a0 <- 0.4
n <- dim(train_set)[1]
y <- train_set$rank
p <- train_set$pi
l <- rep(0, maxiter)

# y <- 1 - y ###

for (j in 1 : maxiter) {
  
  # q_z <- p * a1^y * (1 - a1)^(1 - y) / 
  #   ((p * a1^y * (1 - a1)^(1 - y)) + 
  #      (1 - p) * a0^y * (1 - a0)^(1 - y))
  
  q_z <-p
  
  a1 <- c(t(q_z) %*% y / sum(q_z))
  a0 <- c((1 - t(q_z)) %*% y / sum(1 - q_z))
  
  l[j] <- sum(q_z * log(p * a1^y * (1 - a1)^y + (p == 0)) + 
                (1 - q_z ) * log((1 - p) * a0^y * (1 - a0)^y + (p == 1)) -
                (q_z * log(q_z + (q_z ==0)) + (1 - q_z) * 
                   log(1 - q_z  + (q_z ==1))))
  
}

# ave <- sum(y)/length(y)
# 
# 
# l1 <- sum(q_z) * a1
# al1 <- sum(q_z) * ave
# 
# l2 <- sum(q_z) * (1 - a1)
# al2<- sum(q_z) * (1 - ave)
# 
# nl1 <- (length(y) - sum(q_z)) * a0
# anl1 <- (length(y) - sum(q_z)) * ave
# 
# nl2 <- (length(y) - sum(q_z)) * (1 - a0)
# anl2 <- (length(y) - sum(q_z)) * (1 - ave)
# 
# chi_p <- (l1 - al1)^2/al1 + (l2 - al2)^2/al2 + 
#   (nl1 - anl1)^2/anl1 + (nl2 - anl2)^2/anl2

# Chi-square test



tab1 <- sum(q_z) * a1
tab2 <- sum(q_z) * (1 - a1)
tab3 <- (n - sum(q_z)) * a0
tab4 <- (n - sum(q_z)) * (1 - a0)

leader_recommend <- as.table(cbind(c(tab1, tab3), 
                                   c(tab2, tab4)))
dimnames(leader_recommend) <- list(c("Leader recommend", "Non-leader recommend"), 
                                   c("True", "False"))




## For the negative of User

User_list_sub <- User_list[User_list$User_rating == 0, ] ###

#
userl <- unique(User_list_sub$UserID2)

# n <- length(userl)
# J <- dim(Movie_infor)[1]
# r <- matrix(rep(0, n * J), n)
# 
# for (i in 1 : n) {
#   
#   label <- which((1 - is.na(match(
#     User_list_sub$UserID2, userl[i])))==1)
#   
#   movieid <- User_list_sub$ID[label]
#   
#   r[i, movieid] <- 1
#   
# }

# Build the trainset
rank <- Movie_infor$Movie_rank[User_list_sub$ID]
train_set <- data.frame(User = User_list_sub$UserID2, rank = rank)
pi_sub <- leader$pi[match(train_set$User, leader$User)]
train_set <- cbind(train_set, pi_sub)
colnames(train_set)[3] <- "pi"

# Mixture of the Bernoulli
maxiter <- 100
b1 <- 0.8
b0 <- 0.2
n <- dim(train_set)[1]
y <- train_set$rank
p <- train_set$pi
l <- rep(0, maxiter)

y <- 1 - y ###

for (j in 1 : maxiter) {
  
  # q_z <- p * b1^y * (1 - b1)^(1 - y) / 
  #   ((p * b1^y * (1 - b1)^(1 - y)) + 
  #      (1 - p) * b0^y * (1 - b0)^(1 - y))
  
  q_z <- p
  
  b1 <- c(t(q_z) %*% y / sum(q_z))
  b0 <- c((1 - t(q_z)) %*% y / sum(1 - q_z))
  
  l[j] <- sum(q_z * log(p * b1^y * (1 - b1)^y + (p == 0)) + 
                (1 - q_z ) * log((1 - p) * b0^y * (1 - b0)^y + (p == 1)) -
                (q_z * log(q_z + (q_z ==0)) + (1 - q_z) * 
                   log(1 - q_z  + (q_z ==1))))
  
}

# Chi-square test

tab1 <- sum(q_z) * b1
tab2 <- sum(q_z) * (1 - b1)
tab3 <- (n - sum(q_z)) * b0
tab4 <- (n - sum(q_z)) * (1 - b0)

leader_non_recommend <- as.table(cbind(c(tab1, tab3), 
                                       c(tab2, tab4)))
dimnames(leader_non_recommend) <- list(c("Leader non-recommend", "Non-leader non-recommend"), 
                                       c("True", "False"))

# movie user

tab1 <- leader_recommend[1, 1] + leader_recommend[2, 1]
tab2 <- leader_recommend[1, 2] + leader_recommend[2, 2]
tab3 <- leader_non_recommend[1, 1] + leader_non_recommend[2, 1]
tab4 <- leader_non_recommend[1, 2] + leader_non_recommend[2, 2]

user_movie <- as.table(cbind(c(tab1, tab3), 
                             c(tab2, tab4)))
dimnames(user_movie) <- list(c("User recommend", "User non-recommend"), 
                             c("True", "False"))

# Leader movie

tab1 <- leader_recommend[1, 1]
tab2 <- leader_recommend[1, 2]
tab3 <- leader_non_recommend[1, 1]
tab4 <- leader_non_recommend[1, 2]

leader_movie <- as.table(cbind(c(tab1, tab3), 
                               c(tab2, tab4)))
dimnames(leader_movie) <- list(c("Leader recommend", "Leader non-recommend"), 
                               c("True", "False"))

# Non leader movie

tab1 <- leader_recommend[2, 1]
tab2 <- leader_recommend[2, 2]
tab3 <- leader_non_recommend[2, 1]
tab4 <- leader_non_recommend[2, 2]

non_leader_movie <- as.table(cbind(c(tab1, tab3), 
                                   c(tab2, tab4)))
dimnames(non_leader_movie) <- list(c("Non-Leader recommend", "Non-Leader non-recommend"), 
                                   c("True", "False"))



