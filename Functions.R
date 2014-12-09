KnightSummary <- setRefClass(
  "KnightSummary",
  fields = list(
    move_matrix = "matrix",
    list_of_moves = "list",
    spills = "numeric"
    )
  )

BoardSummary <- setRefClass(
  "BoardSummary",
  fields = list(
    over = "numeric",
    under = "ANY",
    spillage = "numeric",
    num_knights = "numeric",
    coverage_mat = "matrix",
    knight_locs = "ANY",
    empty_locs = "ANY"
    )
  )

getSuccessorState <- function(board, board_info){
  rand <- runif(1,0,1)
  if(rand > .9){return(getSuccessorStateRMK(board, board_info$knight_locs))}
  return(getSuccessorStateSwap(board, board_info$knight_locs, board_info$empty_locs))
}

getSuccessorStateRMK <- function(board, k_locs){
  row_num <- runif(1,1,nrow(k_locs))
  coords <- k_locs[row_num,]
  newboard <- removeKnight(coords[1], coords[2], board)
  return(newboard)
}

getSuccessorStateSwap <- function(board, k_locs, e_locs){
  row_num_k <- runif(1,1,nrow(k_locs))
  coords_k <- k_locs[row_num_k,]
  row_num <- runif(1,1,nrow(e_locs))
  coords <- e_locs[row_num,]
  newboard <- swapKnight(coords[1], coords[2], coords_k[1], coords_k[2], board)
  return(newboard)
}

swapKnight <- function(i,j, ki, kj, board){
  board[i,j] = 1
  board[ki,kj] =0
  return(board)
}

removeKnight <- function(i,j,board){
  board[i,j] <- 0
  return(board)
}

#Returns a chessboard filled with 1s and 0s representing knights and blanks
getBoard <- function(n,m, init=T){
  if (init==T){
    #return a matrix filled with knights
    return(matrix(rep(1,n*m), n, m))
  }
}

getBoardSummary <- function(board){
  n <- nrow(board)
  m <- ncol(board)
  
  o_cov <- 0
  u_cov <- 0
  t_spills <- 0
  n_knights <- 0
  c_mat <- matrix(rep(0,n*m), n, m)
  knights <- table(c())
  empties <- table(c())
  
  for(i in 1:n){
    for(j in 1:m){
      if(board[i,j] == 1){
        ksum <- getKnightSummary(i,j,board)
        t_spills <- t_spills + ksum$spills
        n_knights <- n_knights + 1
        c_mat <- c_mat + ksum$move_matrix
        knights <- rbind(knights, c(i,j))
      }
      else{ empties <- rbind(empties, c(i,j))}
    }
  }
  for(i in 1:n){
    for(j in 1:m){
      if(c_mat[i,j] < 1){ u_cov <- u_cov + 1}
    }
  }
  c_mat <- c_mat - 1
  o_cov <- calcOvercoverage(c_mat)
  #print(o_cov)
  #print(ksum$move_matrix)
 # print(c_mat)
  ret <- BoardSummary$new(over = o_cov, under = u_cov, spillage = t_spills, num_knights = n_knights, coverage_mat = (c_mat + 1), knight_locs = knights, empty_locs = empties)
  return(ret)
}

calcOvercoverage <- function(matrix){
  total <- 0
  for(i in 1:nrow(matrix)){
    for(j in 1:ncol(matrix)){
      total <- total + matrix[i,j]
    }
  }
  return(total)
}

getKnightSummary <- function(i,j,board){
  n = nrow(board)
  #print(n)
  m = ncol(board)
  #print(m)
  moves = matrix(rep(0,m*n),n,m)
  invalid_move <- 0
  move_list <- list()
  if(onBoard(i-1,j-2,n,m)){moves[i-1,j-2] <- 1
  }
  
  else{invalid_move <- (invalid_move <- invalid_move+1)}
  if(onBoard(i-1,j+2,n,m)){moves[i-1,j+2] <- 1
  
  }
  else{invalid_move <- (invalid_move <- invalid_move+1)}
  if(onBoard(i-2,j-1,n,m)){moves[i-2,j-1] <- 1
  
  }
  else{invalid_move <- (invalid_move <- invalid_move+1)}
  if(onBoard(i-2,j+1,n,m)){moves[i-2,j+1] <- 1
  
  }
  else{invalid_move <- (invalid_move <- invalid_move+1)}
  if(onBoard(i+1,j-2,n,m)){moves[i+1,j-2] <- 1
  
  }
  else{invalid_move <- (invalid_move <- invalid_move+1)}
  if(onBoard(i+1,j+2,n,m)){moves[i+1,j+2] <- 1
  
  }
  else{invalid_move <- (invalid_move <- invalid_move+1)}
  if(onBoard(i+2,j+1,n,m)){moves[i+2,j+1] <- 1
  
  }
  else{invalid_move <- (invalid_move <- invalid_move+1)}
  if(onBoard(i+2,j-1,n,m)){moves[i+2,j-1] <- 1
  
  }
  else{invalid_move <- (invalid_move <- invalid_move+1)}
  

  
  for (x in 1:n){
    for (y in 1:m){
      if(moves[x,y] == 1){move_list[[length(move_list)+1]] <- list(x,y)}
    }
  }
  
  ks <- KnightSummary$new(move_matrix = moves, list_of_moves = move_list, spills = invalid_move)
  
  #print(ks)
  
  return(ks)
}

#is (i,j) on our nxm board?
onBoard <- function(i,j, n,m){
  if(i > n || i < 1){return(F)}
  if(j > m || j < 1){return(F)}
  return(T)
}
