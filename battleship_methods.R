## Create methods

print.ship <- function(ship){
  name <- paste("Name:",ship$name, sep = " ")
  size <- paste("Size:", ship$size, sep = " ")
  position <- paste("Position:", ship$position, sep = " ")
  hits <- paste("Number of hits on ship:", sum(ship$hits), sep = " ")
  sunk <- paste("Is the ship sunk?", as.logical(ship$sunk), sep = " ")
  print(c(name, size, position, hits, sunk))
}

print.fleet <- function(fleet){
  #print.ship(fleet$ships[1])
  name <- paste("Admiral:", fleet$admiral, sep = " ")
  ocean <- paste("Ocean Size:", fleet$ocean[1], fleet$ocean[2], sep = " ")
  ship1 <- paste("Ship 1:", fleet$ships[[1]][1], fleet$ships[[1]][2], fleet$ships[[1]][3], fleet$ships[[1]][4], fleet$ships[[1]][5], sep = " ")
  ship2 <- paste("Ship 2:", fleet$ships[[2]][1], fleet$ships[[2]][2], fleet$ships[[2]][3], fleet$ships[[2]][4], fleet$ships[[2]][5], sep = " ")
  ship3 <- paste("Ship 3:", fleet$ships[[3]][1], fleet$ships[[3]][2], fleet$ships[[3]][3], fleet$ships[[3]][4], fleet$ships[[3]][5], sep = " ")
  ship4 <- paste("Ship 4:", fleet$ships[[4]][1], fleet$ships[[4]][2], fleet$ships[[4]][3], fleet$ships[[4]][4], fleet$ships[[4]][5], sep = " ")
  ship5 <- paste("Ship 5:", fleet$ships[[5]][1], fleet$ships[[5]][2], fleet$ships[[5]][3], fleet$ships[[5]][4], fleet$ships[[5]][5], sep = " ")
  print(c(name, ocean, ship1, ship2, ship3, ship4, ship5))
}

print.battleship <- function(battleship){
  print.fleet(battleship$fleets[1])
  print.fleet(battleship$fleets[2])
}
