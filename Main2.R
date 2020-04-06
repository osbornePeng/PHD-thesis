#rm(list = ls())

# Read Data
User_list <- read.table("~/R/PHD/User_list.txt", header = T, sep = ",")
Movie_infor <- read.table("~/R/PHD/Movie_infor.txt", header = T, sep = ",")
User_infor <- read.table("~/R/PHD/User_infor.txt", header = T, sep = "")

# Discard the log out users
User_infor <- User_infor[User_infor$Watched_movies>0,] 
User_infor <- User_infor[is.na(as.Date(User_infor$Date_joint)) == 0, ]
User_list <- User_list[which(is.na(match(User_list$UserID2, User_infor$ID))==0),]

# Change the rank to yes or no in MOvie_infor
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




## For the positive of Movie

#User_list_sub <- User_list[User_list$User_rating == 1, ] ###
temp <- Movie_infor$ID[Movie_infor$Movie_rank == 1]
label <- match(User_list$ID, temp)
User_list_sub <- User_list[!is.na(label),]

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
# rank <- Movie_infor$Movie_rank[User_list_sub$ID]
train_set <- data.frame(User = User_list_sub$UserID2, rating = User_list_sub$User_rating)
pi_sub <- leader$pi[match(train_set$User, leader$User)]
train_set <- cbind(train_set, pi_sub)
colnames(train_set)[3] <- "pi"

# Mixture of the Bernoulli

maxiter <- 1
a1 <- 0.8
a0 <- 0.2
n <- dim(train_set)[1]
y <- train_set$rating
p <- train_set$pi
l <- rep(0, maxiter)

# y <- 1 - y ###

for (j in 1 : maxiter) {
  
  q_z <- p * a1^y * (1 - a1)^(1 - y) / 
    ((p * a1^y * (1 - a1)^(1 - y)) + 
       (1 - p) * a0^y * (1 - a0)^(1 - y))
  
  q_z <- p
  
  a1 <- c(t(q_z) %*% y / sum(q_z))
  a0 <- c((1 - t(q_z)) %*% y / sum(1 - q_z))
  
  l[j] <- sum(q_z * log(p * a1^y * (1 - a1)^y + (p == 0)) + 
                (1 - q_z ) * log((1 - p) * a0^y * (1 - a0)^y + (p == 1)) -
                (q_z * log(q_z + (q_z ==0)) + (1 - q_z) * 
                   log(1 - q_z  + (q_z ==1))))
  
}

# Chi-square test

tab1 <- sum(q_z) * a1
tab2 <- sum(q_z) * (1 - a1)
tab3 <- (n - sum(q_z)) * a0
tab4 <- (n - sum(q_z)) * (1 - a0)

movie_recommended <- as.table(cbind(c(tab1, tab3), 
                                  c(tab2, tab4)))
dimnames(movie_recommended) <- list(c("Leader", "Non-leader"), 
                                  c("Recommend", "Non-recommend"))




## For the negative of Movie

#User_list_sub <- User_list[User_list$User_rating == 1, ] ###
temp <- Movie_infor$ID[Movie_infor$Movie_rank == 0]
label <- match(User_list$ID, temp)
User_list_sub <- User_list[!is.na(label),]
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
# rank <- Movie_infor$Movie_rank[User_list_sub$ID]
train_set <- data.frame(User = User_list_sub$UserID2, rating = User_list_sub$User_rating)
pi_sub <- leader$pi[match(train_set$User, leader$User)]
train_set <- cbind(train_set, pi_sub)
colnames(train_set)[3] <- "pi"

# Mixture of the Bernoulli
maxiter <- 100
b1 <- 0.8
b0 <- 0.2
n <- dim(train_set)[1]
y <- train_set$rating
p <- train_set$pi
l <- rep(0, maxiter)

y <- 1 - y ###

for (j in 1 : maxiter) {
  
  q_z <- p * b1^y * (1 - b1)^(1 - y) / 
    ((p * b1^y * (1 - b1)^(1 - y)) + 
       (1 - p) * b0^y * (1 - b0)^(1 - y))
  
  q_z <- p
  
  b1 <- c(t(q_z) %*% y / sum(q_z))
  b0 <- c((1 - t(q_z)) %*% y / sum(1 - q_z))
  
  l[j] <- sum(q_z * log(p * b1^y * (1 - b1)^y + (p == 0)) + 
                (1 - q_z ) * log((1 - p) * b0^y * (1 - b0)^y + (p == 1)) -
                (q_z * log(q_z + (q_z ==0)) + (1 - q_z) * 
                   log(1 - q_z  + (q_z ==1))))
  
}

tab1 <- sum(q_z) * b1
tab2 <- sum(q_z) * (1 - b1)
tab3 <- (n - sum(q_z)) * b0
tab4 <- (n - sum(q_z)) * (1 - b0)

movie_non_recommended <- as.table(cbind(c(tab1, tab3), 
                                  c(tab2, tab4)))
dimnames(movie_non_recommended) <- list(c("Leader", "Non-leader"), 
                                  c("Non-recommend", "recommend"))


# movie user

tab1 <- movie_recommended[1, 1] + movie_recommended[2, 1]
tab2 <- movie_recommended[1, 2] + movie_recommended[2, 2]
tab3 <- movie_non_recommended[1, 1] + movie_non_recommended[2, 1]
tab4 <- movie_non_recommended[1, 2] + movie_non_recommended[2, 2]

movie_user <- as.table(cbind(c(tab1, tab3), 
                                 c(tab2, tab4)))
dimnames(movie_user) <- list(c("Movie recommended", "Movie non-recommended"), 
                                 c("True", "False"))

# Movie leader

tab1 <- movie_recommended[1, 1]
tab2 <- movie_recommended[1, 2]
tab3 <- movie_non_recommended[1, 1]
tab4 <- movie_non_recommended[1, 2]

movie_leader <- as.table(cbind(c(tab1, tab3), 
                               c(tab2, tab4)))
dimnames(movie_leader) <- list(c("Movie recommended", "Movie non-recommended"), 
                               c("True of leader", "False of leader"))

# Movie non-leader
tab1 <- movie_recommended[2, 1]
tab2 <- movie_recommended[2, 2]
tab3 <- movie_non_recommended[2, 1]
tab4 <- movie_non_recommended[2, 2]

movie_non_leader <- as.table(cbind(c(tab1, tab3), 
                                   c(tab2, tab4)))
dimnames(movie_non_leader) <- list(c("Movie recommended", "Movie non-recommended"), 
                                   c("True of non-leader", "False of non-leader"))
