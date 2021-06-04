## Class constructors, with classes in them: 

# ship class
ship <- function(name, size, position) {
  if(!is.numeric(size)){
    stop("size must be numeric")
  }
  if(!is.character(name)){
    stop("name must be character")
  }
  if(!is.character(position)){
    stop("position must be character")
  }
  if(!length(position) == 2){
    stop("position vector must be 2 long")
  }
  
  structure(list(name = name, size = size, position = position, hits = rep(0, size), sunk = 0), class = "ship")
  
}

# make default ships
default_ships <- function() {
  #return list of 5, each is a ship (list)
  list(ship("Aircraft_Carrier", 5, c("","")), ship("Battleship", 4, c("","")), ship("Destroyer", 3, c("","")), ship("Submarine", 3, c("","")), ship("Patrol_Boat", 2, c("","")))
} 

# fleet class
fleet <- function(admiral, ocean = c(10,10), ships = NULL) {
  # class validation
  if(!is.character(admiral)){
    stop("admiral must be character")
  }
  if(!is.vector(ocean)){
    stop("ocean must be a vector")
  }
  if(!length(ocean) == 2){
    stop("ocean must be a vector of length 2")
  }
  if(!(length(ships) == 5) & !(is.null(ships))) {
    stop("ships must be length 5")
  }
 
  # check if ships input is null, if it is return default ships
  if(is.null(ships)) {
    return(structure(list(admiral = admiral, ocean = ocean, ships = default_ships()), class = "fleet"))
  }
  structure(list(admiral = admiral, ocean = ocean, ships = ships), class = "fleet")
}

# position all ships
position_fleet <- function(fleet, positions = NULL) {
  
  # if the player enters their own positions
  if(!is.null(positions)) {
    fleet$ships[[1]]$position <- positions[1:2]
    fleet$ships[[2]]$position <- positions[3:4]
    fleet$ships[[3]]$position <- positions[5:6]
    fleet$ships[[4]]$position <- positions[7:8]
    fleet$ships[[5]]$position <- positions[9:10]
  } else { # if the positions are NULL
    
    num_rows <- fleet[[2]][1]
    num_cols <- fleet[[2]][2]
    row_names <- letters[1:num_rows]
    
    orientation <- sample(c("horizontal","vertical"), 1)
    
    if(orientation == "horizontal") {
      # sample 5 letters (without replacement), and 5 numbers (with replacement)
      rows <- sample(letters[1:num_rows], 5, replace = FALSE)
      cols <- sample(1:num_cols, 5, replace = TRUE)
      
      for(i in 1:5) {
        
        starting_pos_row <- rows[i]
        starting_pos_col <- cols[i]
        ship_size <- fleet[3][[1]][[i]][["size"]]
        
        # keep ship in same row
        ending_pos_row <- starting_pos_row
        
        # place ship to the right, if ship goes off board, place to left
        if((starting_pos_col - 1 + ship_size) > num_cols) {
          ending_pos_col <- starting_pos_col - (ship_size - 1)
        } else {
          ending_pos_col <- starting_pos_col + (ship_size - 1)
        }
        
        fleet[3][[1]][[i]][["position"]] <- c(paste(starting_pos_row, starting_pos_col, sep = "-"), paste(ending_pos_row, ending_pos_col, sep = "-"))
        
      }
    }
    
    if(orientation == "vertical") {
      # sample 5 letters (with replacement), and 5 numbers (without replacement)
      rows <- sample(letters[1:num_rows], 5, replace = TRUE)
      cols <- sample(1:num_cols, 5, replace = FALSE)
      
      for(i in 1:5) {
        
        starting_pos_row <- rows[i]
        num_start_pos_row <- which(starting_pos_row == letters)
        starting_pos_col <- cols[i]
        ship_size <- fleet[3][[1]][[i]][["size"]]
        
        # keep ship in same col
        ending_pos_col <- starting_pos_col
        
        # place ship above, if ship goes off board, place below 
        
        if((num_start_pos_row - 1 + ship_size) > num_rows) {
          num_ending_pos_row <- num_start_pos_row - (ship_size - 1)
        } else {
          num_ending_pos_row <- num_start_pos_row + (ship_size - 1)
        }
        
        ending_pos_row <- letters[num_ending_pos_row]
        
        fleet[3][[1]][[i]][["position"]] <- c(paste(starting_pos_row, starting_pos_col, sep = "-"), paste(ending_pos_row, ending_pos_col, sep = "-"))
        
      }
    }
  }

  fleet
}

# battleship function
battleship <- function(fleets = list()) {
  if(length(fleets) < 2) {
    fleet_p1 <- fleet("Player_1", c(10,10), NULL)
    fleet_p2 <- fleet("Player_2", c(10,10), NULL)
    fleet_p1 <- position_fleet(fleet_p1)
    fleet_p2 <- position_fleet(fleet_p2)
    fleets <- list(fleet_p1, fleet_p2)
  }
  history<-tibble(from = character(), to = character(), target = character(), hit = character())
  structure(list(fleets = fleets, history = history), class = ("battleship"))
}

# human function
human <- function() {
  shot <- readline(prompt="Enter position to fire: ")
  shot
}

## Gameplay
play_bs <- function(players = c("human", "ai_304787099"), battleship, strengths = c(0,0)) {
  
  if(is.null(battleship)) {
    battleship <- battleship()
  }
  
  turns <- rep(0,2)
  
  i <- 0
  
  while(TRUE) {
    player <- (i %% 2) + 1 # index of which player is going
    # assign opponent player
    if(player == 2) {
      opponent <- 1
    } else {
      opponent <- 2
    }
    
    
    # increase turn for player
    turns[player] <- turns[player] + 1
    
    #Get fire position
    if(players[player] == "human") {
      shot_pos <- human()
    } else {
      shot_pos <- ai_304787099(battleship, strengths[player])
    }
    
    
    #update battleship
    battleship <- update_battleship(battleship, player, opponent, shot_pos)
    
    # get admiral name of opponent 
    if(player == 1) {
      opp_admiral <- battleship[[1]][[2]][[1]]
    } else {
      opp_admiral <- battleship[[1]][[1]][[1]]
    }
    
    # get number of hits of opponent made
    player_history <- filter(battleship[["history"]], from == opp_admiral)
    past_hits <- sum(as.numeric(player_history[["hit"]]))
    
    # patrol boat for opponent
    turns_opp <- turns["opponent"]
    patrol_boat <- battleship[[1]][[opponent]][[3]][[5]][[5]]
    
    # check if won
    sunks_opp <- rep(NA, 5)
    sunks_player <- rep(NA, 5)
    for(j in 1:5) {
      sunks_opp[j] <- battleship[[1]][[opponent]][[3]][[j]][[5]]
      sunks_player[j] <- battleship[[1]][[player]][[3]][[j]][[5]]
    }
    
    # store order of ships sunk for opponent
    last_sunk <- rep(NA, 1)
    for(k in 1:5) {
      if(battleship[[1]][[opponent]][[3]][[k]][[5]] == 1) {
        last_sunk[1] <- battleship[[1]][[opponent]][[3]][[k]][[2]]
      }
    }
   
  
    # if game is won, return info 
    if(sum(sunks_opp) == 5) {
      winner <- battleship[1][[1]][[player]][[1]]
      unsunk <- 5 - sum(sunks_player)
      patrol_boat_winner <- sunks_player[5]
      to_return <- list("Winner" = winner, "Winner_Turns" = turns[player], "Unsunk_Ships_Winner" = unsunk, 
                        "Hits_losing" = past_hits, "Patrol_Boat_Winner" = patrol_boat_winner, "Last_Ship_Opponent" = last_sunk)
      return(to_return)
    }
    
    # increase number of turns after turn has been taken
    i <- i + 1
    
  }
}

# AI with two strengths
ai_304787099 <- function(battleship, strength = 9, fleet = NULL) {
  
  if(!is.null(fleet)) {
    if(!(is.list(fleet) & (length(fleet) == 3))) {
      stop("Fleet must be a list of length 3!")
    }
    
    fleet <- position_fleet(fleet)
    return(fleet)
  }
  
  if(dim(battleship[["history"]])[1] <= 1) {
    shot_pos <- "a-1"
    return(shot_pos)
  }
  
  # get admiral names
  p1 <- battleship[[1]][[1]][[1]]
  p2 <- battleship[[1]][[2]][[1]]
  
  # place ships if fleet object passed
  #position_fleet(fleet)
  
  # get row dimension from history aka last row
  histo <- battleship[["history"]]
  dims <- dim(histo)
  nrows <- dims[1]
  
  # find current player
  last_player <- histo[[nrows, 1]]
  if(last_player == p1) {
    player <- 2
    admiral <- p2
  } else {
    player <- 1
    admiral <- p1
  }
  
  # info for ocean
  num_rows <- battleship[[1]][[player]][[2]][1] # ocean rows
  num_cols <- battleship[[1]][[player]][[2]][2] # ocean cols
  rows <- letters[1:num_rows]
  cols <- 1:num_cols
  
  # get past targets of current player
  player_history <- filter(histo, from == admiral)
  nrow_player <- dim(player_history)[1]
  past_targets <- player_history[["target"]]
  
  # convert past targets into single number on ocean
  numeric_past_targets <- rep(NA, nrow_player)
  for(i in 1:nrow_player) {
    past_target <- past_targets[i]
    row_target <- substr(past_target, 1, 1)
    numeric_row <- which(letters %in% row_target)
    col_target <- as.numeric(substr(past_target, 3, 4))
    
    numeric_past_target <- ((numeric_row - 1) * 10) + col_target
    numeric_past_targets[i] <- numeric_past_target
  }
  
  # find all possible targets on ocean
  poss_shots <- 1:(num_rows * num_cols)
  
  # find all available targets
  available_targets <- poss_shots[!(poss_shots %in% numeric_past_targets)]
  
  if(strength == 0) {
    # naive condition, randomly pick shots as long as they haven't been picked before
    
    numeric_shot_pos <- sample(available_targets, 1)
    
    numeric_row_idx <- ceiling(numeric_shot_pos/10)
    row_idx <- letters[numeric_row_idx]
    col_idx <- numeric_shot_pos %% 10
    if(col_idx == 0) {
      col_idx <- col_idx + 10
    }

    shot_pos <- paste(row_idx, col_idx, sep = "-")
    return(shot_pos)
  }
  
  if(strength == 9) {
    # smart condition
    # see if player's last move was a hit
    last_move_numeric <- numeric_past_targets[nrow_player]
    
    # if last move was a hit then try spaces around it
    if((as.numeric(player_history[["hit"]][[nrow_player]]) == 1)) {
      # first try to the right
      if(sum(available_targets %in% (last_move_numeric + 1)) == 1) {
        numeric_shot_pos <- last_move_numeric + 1
      } else { # then try to the left
        if(sum(available_targets %in% (last_move_numeric - 1)) == 1) {
          numeric_shot_pos <- last_move_numeric - 1
        } else { # then try above
          if(sum(available_targets %in% (last_move_numeric - 10)) == 1) {
            numeric_shot_pos <- last_move_numeric - 10
          } else { # then try below
            if(sum(available_targets %in% (last_move_numeric + 10)) == 1) {
              numeric_shot_pos <- last_move_numeric + 10
            } else { # if none are available randomly sample
              numeric_shot_pos <- sample(available_targets, 1)
            }
          }
        }
      }
    } else {
      numeric_shot_pos <- sample(available_targets, 1)
    }
    
    numeric_row_idx <- ceiling(numeric_shot_pos/10)
    row_idx <- letters[numeric_row_idx]
    col_idx <- numeric_shot_pos %% 10
    if(col_idx == 0) {
      col_idx <- col_idx + 10
    }
    
    shot_pos <- paste(row_idx, col_idx, sep = "-")
    return(shot_pos)
    
  }
  shot_pos # return value
}
attr(ai_304787099,'alt') <- '304787099'

# helper function to update battleship each turn
update_battleship <- function(battleship, player, opponent, shot_pos) {
  
  # store shot elements
  shot_row <- substr(shot_pos, 1,1)
  shot_col <- as.numeric(substr(shot_pos, 3, nchar(shot_pos)))
  
  # if shot was a hit or not
  hit <- 0
  
  # update the other player's fleet object
  for(j in 1:5) {
    # checking ship j
    ship_j <- battleship[[1]][[opponent]][[3]][[j]]
    
    ship_pos_beg <- ship_j[[3]][1]
    ship_pos_end <- ship_j[[3]][2]
    
    if(substr(ship_pos_beg, 1, 1) == substr(ship_pos_end, 1, 1)) {
      # horizontal check
      ship_row<-substr(ship_pos_beg,1,1) #row index of fleet
      ship_col_beg <- substr(ship_pos_beg, 3, nchar(ship_pos_beg)) #col index 1
      ship_col_end <- substr(ship_pos_end, 3, nchar(ship_pos_end)) #col index 2
      
      if(ship_col_beg > ship_col_end) {
        temp <- ship_col_beg
        ship_col_beg <- ship_col_end
        ship_col_end <- temp
      }
      
      #Check if the fire was a hit
      if((shot_row == ship_row) & (shot_col >= ship_col_beg) & (shot_col <= ship_col_end)) {
        hit <- 1
        original_hits <- ship_j[[4]]
        hit_index <- which((ship_col_beg:ship_col_end) %in% shot_col)
        original_hits[hit_index] <- 1
        ship_j[[4]] <- original_hits
        # check if sunk
        if(all(as.logical(original_hits)) == 1) {
          ship_j[[5]] <- 1
        }
        #need to put ship_j back in battleship
      }
    } else {
      # vertical check
      ship_col <- substr(ship_pos_beg,3,nchar(ship_pos_beg)) #col index of fleet
      ship_row_beg <- substr(ship_pos_beg, 1, 1) #row index 1
      ship_row_end <- substr(ship_pos_end, 1, 1) #row index 2
      
      if(ship_row_beg > ship_row_end) {
        temp <- ship_row_beg
        ship_row_beg <- ship_row_end
        ship_row_end <- temp
      }
      
      ship_seq <- which(letters %in% ship_row_beg):which(letters %in% ship_row_end)
      
      #Check if the fire was a hit
      if((shot_col == ship_col) & (shot_row >= ship_row_beg) & (shot_row <= ship_row_end) ) {
        hit <- 1
        original_hits <- ship_j[[4]]
        hit_index <- which(ship_seq %in% which(letters %in% shot_row))
        original_hits[hit_index] <- 1
        ship_j[[4]] <- original_hits
        # check if sunk
        if(all(as.logical(original_hits)) == 1) {
          ship_j[[5]] <- 1
        }
        
      }
    }
   
    # put ship_j back in battleship
    battleship[[1]][[opponent]][[3]][[j]] <- ship_j 
    
  }
  # update history
  battleship[["history"]] <- add_row(battleship[["history"]], from = battleship[[1]][[player]][[1]], to = battleship[[1]][[opponent]][[1]], target = shot_pos, hit = hit)
  
  battleship
}

## ------------------------------------------------------------------------------------------------------------

## Create methods

print.ship <- function(ship) {
  name <- paste("Name:",ship$name, sep = " ")
  size <- paste("Size:", ship$size, sep = " ")
  position <- paste("Position:", ship$position, sep = " ")
  hits <- paste("Number of hits on ship:", sum(ship$hits), sep = " ")
  sunk <- paste("Is the ship sunk?", as.logical(ship$sunk), sep = " ")
  print(list(name, size, position, hits, sunk))
}

print.fleet <- function(fleet) {
  name <- paste("Admiral:", fleet$admiral, sep = " ")
  ocean <- paste("Ocean Size:", fleet$ocean[1], fleet$ocean[2], sep = " ")
  ship1 <- paste("Ship 1:", fleet$ships[[1]][1], "Size:", fleet$ships[[1]][2], "Position:", fleet$ships[[1]][3], "Hits:", fleet$ships[[1]][4], "Sunk?", fleet$ships[[1]][5], sep = " ")
  ship2 <- paste("Ship 2:", fleet$ships[[2]][1], "Size:", fleet$ships[[2]][2], "Position:", fleet$ships[[2]][3], "Hits:", fleet$ships[[2]][4], "Sunk?", fleet$ships[[2]][5], sep = " ")
  ship3 <- paste("Ship 3:", fleet$ships[[3]][1], "Size:", fleet$ships[[3]][2], "Position:", fleet$ships[[3]][3], "Hits:", fleet$ships[[3]][4], "Sunk?", fleet$ships[[3]][5], sep = " ")
  ship4 <- paste("Ship 4:", fleet$ships[[4]][1], "Size:", fleet$ships[[4]][2], "Position:", fleet$ships[[4]][3], "Hits:", fleet$ships[[4]][4], "Sunk?", fleet$ships[[4]][5], sep = " ")
  ship5 <- paste("Ship 5:", fleet$ships[[5]][1], "Size:", fleet$ships[[5]][2], "Position:", fleet$ships[[5]][3], "Hits:", fleet$ships[[5]][4], "Sunk?", fleet$ships[[5]][5], sep = " ")
  print(list(name, ocean, ship1, ship2, ship3, ship4, ship5))
}

print.battleship <- function(battleship) {
  print(battleship[[1]][1])
  print(battleship[[1]][2])
}

plot.fleet <- function(fleet) {
  # info for ocean
  num_rows <- fleet[[2]][[1]] # ocean rows
  num_cols <- fleet[[2]][[2]] # ocean cols
  rows <- letters[1:num_rows]
  cols <- 1:num_cols
  
  par(bg = 'light blue')
  plot(1:num_cols, 1:num_rows, type = "n", xaxt = "n", yaxt = "n", main = "Fleet", xlab = "", ylab = "", col = "blue")  # set up coord. system
  axis(1, at=seq(1, num_cols, by=1))
  axis(2, at=1:num_rows, labels=letters[num_rows:1])
  grid(nx = num_cols, ny = num_rows, col = "blue", lty = "solid", lwd = par("lwd"), equilogs = TRUE) # plot gridlines
  
  for(i in 1:5) {
    ship_pos_start <- fleet[3][[1]][[i]][["position"]][[1]]
    ship_pos_end <- fleet[3][[1]][[i]][["position"]][[2]]
    
    ship_pos_start_row <- which(letters %in% substr(ship_pos_start, 1, 1))
    ship_pos_start_col <- as.numeric(substr(ship_pos_start, 3, 4))
    ship_pos_end_row <- which(letters %in% substr(ship_pos_end, 1, 1))
    ship_pos_end_col <- as.numeric(substr(ship_pos_end, 3, 4))
    
    if(ship_pos_start_row > ship_pos_end_row) {
      temp <- ship_pos_start_row
      ship_pos_start_row <- ship_pos_end_row
      ship_pos_end_row <- temp
    }
    if(ship_pos_start_col > ship_pos_end_col) {
      temp <- ship_pos_start_col
      ship_pos_start_col <- ship_pos_end_col
      ship_pos_end_col <- temp
    }
    
    x1 <- ship_pos_start_col
    x2 <- ship_pos_end_col
    y1 <- (num_rows + 1) - ship_pos_start_row 
    y2 <- (num_rows + 1) - ship_pos_end_row
    
    if(ship_pos_start_col == ship_pos_end_col) {
      rect(x1 + 0.5, y1 + 0.5, x2 - 0.5, y2 - 0.5, col = 'gray', lwd = 1.5)
      points(rep(x1, length(y1:y2)), c(y1:y2))
    }
    if(ship_pos_start_row == ship_pos_end_row) {
      rect(x1 - 0.5, y1 - 0.5, x2 + 0.5, y2 + 0.5, col = 'gray', lwd = 1.5)
      points(c(x1:x2), rep(y1, length(x1:x2)))
    }
    
  }
}

plot.battleship <- function(battleship) {
  par(mfrow=c(1,2))
  plot(battleship[[1]][[1]])
  plot(battleship[[1]][[2]])
  
}

summary.fleet <- function(fleet) {
  sunk <- sum(as.numeric(fleet$ships[[1]][5], fleet$ships[[2]][5], fleet$ships[[3]][5], fleet$ships[[4]][5], fleet$ships[[5]][5]))
  num_sunks <- paste("Number of Sunk Ships: ", sunk)
  num_alive <- paste("Number of Alive Ships: ", 5-sunk)
  print(c(num_sunks, num_alive))
}

summary.battleship <- function(battleship) {
  summary.fleet(battleship$fleets[1])
  summary.fleet(battleship$fleets[2])
}

## ------------------------------------------------------------------------------------------------------------

# Simulations
Battleship1 <-battleship()

naive_naive <- data.frame(list(1), numeric(1), numeric(1), numeric(1), numeric(1), numeric(1))
naive_smart <- data.frame(list(1), numeric(1), numeric(1), numeric(1), numeric(1), numeric(1))
smart_naive <- data.frame(list(1), numeric(1), numeric(1), numeric(1), numeric(1), numeric(1))
smart_smart <- data.frame(list(1), numeric(1), numeric(1), numeric(1), numeric(1), numeric(1))

for(i in 1:1000) {
  naive_naive[i,] <- play_bs(players = c("ai_304787099", "ai_304787099"), Battleship1, strengths = c(0,0))
  naive_smart[i,] <- play_bs(players = c("ai_304787099", "ai_304787099"), Battleship1, strengths = c(0,9))
  smart_naive[i,] <- play_bs(players = c("ai_304787099", "ai_304787099"), Battleship1, strengths = c(9,0))
  smart_smart[i,] <- play_bs(players = c("ai_304787099", "ai_304787099"), Battleship1, strengths = c(9,9))
  
}

# Questions
length(which(naive_naive[,1] == "Player_1"))
min_turns <- min(naive_naive[,2], na.rm = TRUE)
max_turns <- max(naive_naive[,2], na.rm = TRUE)
summary(naive_naive[,3])
summary(naive_naive[,4])
sum(naive_naive[,5], na.rm = TRUE)/1000
length(which(naive_naive[,6] == 2))

length(which(naive_smart[,1] == "Player_1"))
min_turns <- min(naive_smart[,2], na.rm = TRUE)
max_turns <- max(naive_smart[,2], na.rm = TRUE)
summary(naive_smart[,3])
summary(naive_smart[,4])
sum(naive_smart[,5], na.rm = TRUE)/1000
length(which(naive_smart[,6] == 2))

length(which(smart_naive[,1] == "Player_1"))
min_turns <- min(smart_naive[,2], na.rm = TRUE)
max_turns <- max(smart_naive[,2], na.rm = TRUE)
summary(smart_naive[,3])
summary(smart_naive[,4])
sum(smart_naive[,5], na.rm = TRUE)/1000
length(which(smart_naive[,6] == 2))

length(which(smart_smart[,1] == "Player_1"))
min_turns <- min(smart_smart[,2], na.rm = TRUE)
max_turns <- max(smart_smart[,2], na.rm = TRUE)
summary(smart_smart[,3])
summary(smart_smart[,4])
sum(smart_smart[,5], na.rm = TRUE)/1000
length(which(smart_smart[,6] == 2))

## ------------------------------------------------------------------------------------------------------------

# Handicapped Games
# 1) 10x10 vs. 10x11
fleet1 <- position_fleet(fleet("Player 1", ocean = c(10,10), ships = NULL))
fleet2 <- position_fleet(fleet("Player 2", ocean = c(10,11), ships = NULL))
Battleship2 <- battleship(fleets = list(fleet1, fleet2))

handicapped1 <- matrix(data = NA, nrow = 1000, ncol = 5)

for(i in 1:1000) {
  handicapped1[i,] <- as.numeric(play_bs(players = c("ai_304787099", "ai_304787099"), Battleship2, strengths = c(9,0)))
}

which(handicapped1[,1] == 2)
min_turns <- min(handicapped1[,2])
max_turns <- max(handicapped1[,2])
sum(handicapped1[,5])/1000

# 2) switch destroyer and patrol boat
ship1 <- ship("ship1", 5, c("a-5", "a-9"))
ship2 <- ship("ship2", 4, c("b-5", "b-8"))
ship3 <- ship("ship3", 3, c("c-5", "c-7"))
ship4 <- ship("ship4", 3, c("e-2", "e-4"))
ship5 <- ship("ship5", 3, c("j-1", "j-3"))

ship6 <- ship("ship6", 5, c("a-5", "a-9"))
ship7 <- ship("ship7", 4, c("b-5", "b-8"))
ship8 <- ship("ship8", 3, c("c-5", "c-7"))
ship9 <- ship("ship9", 2, c("e-2", "e-3"))
ship10 <- ship("ship10", 2, c("j-1", "j-2"))

fleet3 <- fleet("Player 1", ocean = c(10,10), ships = list(ship1,ship2,ship3,ship4,ship5))
fleet4 <- fleet("Player 2", ocean = c(10,10), ships = list(ship6,ship7,ship8,ship9,ship10))
Battleship2 <- battleship(fleets = list(fleet3, fleet4))

handicapped2 <- matrix(data = NA, nrow = 1000, ncol = 5)

for(i in 1:1000) {
  handicapped2[i,] <- as.numeric(play_bs(players = c("ai_304787099", "ai_304787099"), Battleship2, strengths = c(9,0)))
}

which(handicapped2[,1] == 2)
min_turns <- min(handicapped2[,2])
max_turns <- max(handicapped2[,2])
sum(handicapped2[,5])/1000

# 3) swap destroyer with patrol boat AND swap aircraft carrier and battleship
ship1 <- ship("ship1", 5, c("a-5", "a-9"))
ship2 <- ship("ship2", 5, c("b-5", "b-9"))
ship3 <- ship("ship3", 3, c("c-5", "c-7"))
ship4 <- ship("ship4", 3, c("e-2", "e-4"))
ship5 <- ship("ship5", 3, c("j-1", "j-3"))

ship6 <- ship("ship6", 4, c("a-5", "a-8"))
ship7 <- ship("ship7", 4, c("b-5", "b-8"))
ship8 <- ship("ship8", 3, c("c-5", "c-7"))
ship9 <- ship("ship9", 2, c("e-2", "e-3"))
ship10 <- ship("ship10", 2, c("j-1", "j-2"))

fleet3 <- fleet("Player 1", ocean = c(10,10), ships = list(ship1,ship2,ship3,ship4,ship5))
fleet4 <- fleet("Player 2", ocean = c(10,10), ships = list(ship6,ship7,ship8,ship9,ship10))
Battleship2 <- battleship(fleets = list(fleet3, fleet4))

handicapped3 <- matrix(data = NA, nrow = 1000, ncol = 5)

for(i in 1:1000) {
  handicapped3[i,] <- as.numeric(play_bs(players = c("ai_304787099", "ai_304787099"), Battleship2, strengths = c(9,0)))
}

which(handicapped3[,1] == 2)
min_turns <- min(handicapped3[,2])
max_turns <- max(handicapped3[,2])
sum(handicapped3[,5])/1000
