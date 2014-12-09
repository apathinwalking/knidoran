
source("../Functions.R")

cool <- function(temperature){
  return(temperature*.9990)
}

KDSA_init <- function(n, m, temp, num_trials, out_name){
  board <- getBoard(n,m)
  board_info <- getBoardSummary(board)
  info <- SAReturn$new(scores = c(board_info$over), temperatures = c(temp), final_board = board, best = 9999999999999, rand_swaps.x = c(), rand_swaps.y = c())
  KDSA(temp, num_trials, board, out_name, info)
}

KDSA <- function(temp, num_trials, init_board, out_name, info){
  board_a <- init_board
  board_a_info <- getBoardSummary(board_a)
  board_b <- getSuccessorState(board_a, board_a_info)
  board_b_info <- getBoardSummary(board_b)
  for(i in 1:num_trials){
    info$under[i] <- board_a_info$under[i]
    info$over[i] <- board_a_info$over[i]
    info$n_knights[i] <- board_a_info$num_knights[i]
    print(board_a)
    delta <- ((board_a_info$over + 1000*board_a_info$under) - (board_b_info$over + 1000*board_b_info$under))
    e <- exp(-delta/temp)
    if(delta < 0){
      board_a <- board_b
      board_a_info <- board_b_info
    }
    else if(runif(1,0,1) < e){
      board_a <- board_b
      board_a_info <- board_b_info
      info$rand_swaps.x <- c(info$rand_swaps.x, i+1)
      info$rand_swaps.y <- c(info$rand_swaps.x, board_a_info$over+1)
    }
    board_b <- getSuccessorState(board_a, board_a_info)
    board_b_info <- getBoardSummary(board_b)
    info$scores <- c(info$scores, board_a_info$over)
    temp <- cool(temp)
    info$temperatures <- c(info$temperatures, temp)
    print(board_a_info$over)
  }
  info$temperatures <- c(info$temperatures, temp)
  info$final_board <- board_a
  return(info)
  #info$scores <- c(info$scores, board_info$over)
}



SAReturn <- setRefClass(
  "SAReturn",
  fields = list(
    scores = "numeric",
    temperatures = "numeric",
    final_board = "matrix",
    best = "numeric",
    #best_board = "matrix",
    n_knights = "numeric",
    over = "numeric",
    under = "numeric",
    rand_swaps.x = "ANY",
    rand_swaps.y = "ANY"
  )
)
  
#outrm <- KDSA_init(20,20, 1000, 300, "out.csv")
#outsw <- KDSA(1000,init_board=good_3$final_board, 300, info=SAReturn$new())