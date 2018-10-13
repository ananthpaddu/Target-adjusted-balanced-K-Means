#' @title Balanced Kmeans function
#'
#' @description This function implements balanced K-Means algorithm where the minimum and maximum number of observations in each cluster and the power ratings of each observation are taken as constraints.
#'
#'
#' @param data dataset containing numerical columns of observations to be clustered.
#' @param k number of output clusters required; default=2.
#' @param standardize standardization of observation values required; by default=FALSE.
#' @param max_iter maximum iterations to be made for clustering; default=20.
#' @return A dataframe object with Balanced k clusters
#' @import lpSolve
#' @import dummies
#' @import stats
#' @export
#'
#' @examples
balKmeans <- function (data, k = 2, standardize = FALSE, max_iter = 20) {

  data <- data.frame(data)

  # Standardize (if neccessary)
  if (standardize) {
    for (i in 1:ncol(data)) {
      sigma_val <- sd(as.numeric(as.vector(data[,i])))
      mean_val <- mean(as.numeric(as.vector(data[,i])))
      if (sigma_val != 0) {
        data[,colnames(data)[i]] <- (as.numeric(as.vector(data[,i])) - mean_val)/sigma_val
      } else {
        warning('The variable does not posses any variation to standardize')
      }
    }
  }

  center_index <- sample(1:nrow(data), k)
  centers <- data[center_index,!(colnames(data) %in% c('cluster'))]

  data$cluster <- 0
  iter <- 1
  balanced_flag <- FALSE

  while (iter < max_iter) {
    print(paste('iteration:', iter))
    cluster_dist <- vector(mode = 'double', length = k)
    cluster_dist_matrix <- NULL
    for (i in 1:nrow(data)) {
      for (j in 1:k) {
        cluster_dist[j] <- dist(rbind(data[i,!(colnames(data) %in% c('cluster'))], centers[j,!(colnames(data) %in% c('cluster'))]))
      }
      cluster_dist_matrix <- cbind(cluster_dist_matrix, cluster_dist)
      data[i, 'cluster'] <- which.min(cluster_dist)
    }


    if (balanced_flag) {
      #balanced_flag <- FALSE
      print('balancing clusters')

      na <- data.frame(table(data$cluster))
      na <- na$Freq

      nb <- rep(floor(nrow(data)/k), (k - 1))
      nb <- c(nb, (nrow(data) - sum(nb)))

      if (identical(sort(na), nb)) {
        print('clusters balanced')
        iter <- max_iter
      } else {
        a <- matrix(dummy(data$cluster), nrow = nrow(data), ncol = k)
        b <- matrix(0, nrow = nrow(data), ncol = k)

        c <- 1/sqrt(na*nb)

        cluster_dist_matrix <- cluster_dist_matrix/max(cluster_dist_matrix)

        objective.in  <- matrix((-1*(t(cluster_dist_matrix))), ncol = 1)

        col_cond <- matrix(diag(k), k, k*nrow(data))
        col_choice <- NULL
        for (i in 1:k) {
          col_choice <- c(col_choice, rep(i,nrow(data)))
        }
        col_cond <- diag(k)
        col_cond <- col_cond[,col_choice]

        const.mat <- rbind(diag(nrow = k*nrow(data)), diag(nrow = k*nrow(data)), matrix(diag(nrow(data)), nrow(data), k*nrow(data)), col_cond)
        #nrow(const.mat)
        #ncol(const.mat)

        const.rhs <- c(rep(1, k*nrow(data)), rep(0, k*nrow(data)), rep(1, nrow(data)), rep(floor(nrow(data)/k), k))

        const.dir  <- c(rep('<=', k*nrow(data)), rep('>=', k*nrow(data)), rep('=', nrow(data)), rep('>=', k))

        optimum <-  lp(direction="max",  objective.in, const.mat, const.dir,  const.rhs)

        if(optimum$status) {
          warnings("Couldn't find a optimized balaced class")
        }

        best_sol <- optimum$solution
        best_sol <- matrix(best_sol, ncol = k)
        data$cluster <- max.col(best_sol)
      }
    }

    center_past <- centers
    for (j in 1:k) {
      centers[k,] <- colMeans(data[data$cluster == k,!(colnames(data) %in% c('cluster'))])
    }

    if (iter == max_iter) {
      warning('Reached last iteration')
    }
    if (identical(centers, center_past)) {
      print('clusters generated')
      if (balanced_flag) {
        print('clusters generated and balanced')
        iter <- max_iter
      } else {
        print('now balancing the clusters')
        balanced_flag = TRUE
        iter <- iter + 1
      }
    } else {
      iter <- iter + 1
      print('still looping')
    }
  }


  return(data)
}

