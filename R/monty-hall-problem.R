#' @title
#'   Create a new Monty Hall Problem game.
#'
#' @description
#'   `create_game()` generates a new game that consists of two doors 
#'   with goats behind them, and one with a car.
#'
#' @details
#'   The game setup replicates the game on the TV show "Let's
#'   Make a Deal" where there are three doors for a contestant
#'   to choose from, one of which has a car behind it and two 
#'   have goats. The contestant selects a door, then the host
#'   opens a door to reveal a goat, and then the contestant is
#'   given an opportunity to stay with their original selection
#'   or switch to the other unopened door. There was a famous 
#'   debate about whether it was optimal to stay or switch when
#'   given the option to switch, so this simulation was created
#'   to test both strategies. 
#'
#' @param ... no arguments are used by the function.
#' 
#' @return The function returns a length 3 character vector
#'   indicating the positions of goats and the car.
#'
#' @examples
#'   create_game()
#'
#' @export
create_game <- function()
{
    a.game <- sample( x=c("goat","goat","car"), size=3, replace=F )
    return( a.game )
} 



#' @title
#' Contestants selects a door
#'
#' @description
#' Contestant will randomly select a door 1, 2, or 3.
#'
#' @details
#' Each door will randomly have a goat or car behind 
#' it with a total of 2 goats and 1 car.
#'
#' @param  ... no arguments are used by the function.
#'
#' @return 
#' The function returns a length of 1 numeric vector 
#' indicating that one door has been selected.
#'
#' @examples
#' select_door()
#'
#' @export
select_door <- function( )
{
  doors <- c(1,2,3) 
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}



#' @title
#' Host opens a goat door
#'
#' @description
#' The host will select a door the contestant did not select.
#'
#' @details
#' The host will open a goat door the contestant did not select.
#'
#' @param  ... no arguments are used by the function.
#'
#' @return 
#' The function returns a length of 1 numeric vector indicating 
#' that a one door has been selected that is different from
#' the door that the contestant selected.
#'
#' @examples
#' open_goat_door( game, a.pick)
#'
#' @export
open_goat_door <- function( game, a.pick )
{
   doors <- c(1,2,3)
   # if contestant selected car,
   # randomly select one of two goats 
   if( game[ a.pick ] == "car" )
   { 
     goat.doors <- doors[ game != "car" ] 
     opened.door <- sample( goat.doors, size=1 )
   }
   if( game[ a.pick ] == "goat" )
   { 
     opened.door <- doors[ game != "car" & doors != a.pick ] 
   }
   return( opened.door ) # number between 1 and 3
}



#' @title
#' Change doors
#'
#' @description
#' The contestant decides if they want to change doors.
#'
#' @details
#' After seeing the host open one of the goat doors,
#' the contestant will decide if they want to stay with
#' the door they first chose or switch to the remaining
#' door that has not been chosen yet.
#'
#' @param
#' If contestant stays (stay=T) 
#' If contestant does not stay (stay=F)
#'
#' @return 
#' The function returns a length of 1 numeric vector indicating 
#' that a one door has been selected that is different from
#' the door that the host opened.
#'
#' @examples
#'   change_door(stay=T, opened.door, a.pick)
#'   change_door(stay=F, opened.door, a.pick)

#' @export
change_door <- function( stay=T, opened.door, a.pick )
{
   doors <- c(1,2,3) 
   
   if( stay )
   {
     final.pick <- a.pick
   }
   if( ! stay )
   {
     final.pick <- doors[ doors != opened.door & doors != a.pick ] 
   }
  
   return( final.pick )  # number between 1 and 3
}



#' @title
#' Determine if contestant has won
#'
#' @description
#' Contestant wins if they chose a door with a car.
#'
#' @details
#' After the contestant decides to stay or switch doors,
#' the final pick will either be a goat or car. If a car
#' is behind the final chosen door, then the contestant is 
#' a winner.
#'
#' @param ... no arguments are used by the function.
#'
#' @return 
#' If the final.pick is a car then the return is "WIN"
#' If the final.pick is a door then the return is "LOSE"
#'
#' @examples
#' determine_winner <- function( final.pick, game )
#'
#' @export
determine_winner <- function( final.pick, game )
{
   if( game[ final.pick ] == "car" )
   {
      return( "WIN" )
   }
   if( game[ final.pick ] == "goat" )
   {
      return( "LOSE" )
   }
}





#' @title
#' Let's Play the Monty Hall game!
#'
#' @description
#' This game plays the entire Monty Hall game
#' from start to finish.
#' 
#' @details
#' Within this game, all functions: create_game(),
#' select_door(), open_goat_door(), change_door(),
#' and determine_winner() will run all together.
#'
#' @param ... no arguments are used by the function.
#'
#' @return 
#' A list of the resuls of each function will be returned.
#'
#' @examples
#' play_game()
#' @export
play_game <- function( )
{
  new.game <- create_game()
  first.pick <- select_door()
  opened.door <- open_goat_door( new.game, first.pick )

  final.pick.stay <- change_door( stay=T, opened.door, first.pick )
  final.pick.switch <- change_door( stay=F, opened.door, first.pick )

  outcome.stay <- determine_winner( final.pick.stay, new.game  )
  outcome.switch <- determine_winner( final.pick.switch, new.game )
  
  strategy <- c("stay","switch")
  outcome <- c(outcome.stay,outcome.switch)
  game.results <- data.frame( strategy, outcome,
                              stringsAsFactors=F )
  return( game.results )
}






#' @title
#' Play the Monty Hall Many Times 
#'
#' @description
#' This program will play the Monty Hall game
#' multiple times.
#'
#' @details
#' The function is set up to play the Monty Hall
#' game "n" number of times. And "n" is set at 100
#' times. So the game will loop the plays and play 
#' the game 100 times.
#'
#' @param 
#' "n" could be set to whatever number you want.
#'
#' @return 
#' A table will be returned with your results after
#' the game is played 100 times.
#'
#' @examples
#' play_n_games <- function( n=100 )
#'
#' @export
play_n_games <- function( n=100 )
{
  
  library( dplyr )
  results.list <- list()   # collector
  loop.count <- 1

  for( i in 1:n )  # iterator
  {
    game.outcome <- play_game()
    results.list[[ loop.count ]] <- game.outcome 
    loop.count <- loop.count + 1
  }
  
  results.df <- dplyr::bind_rows( results.list )

  table( results.df ) %>% 
  prop.table( margin=1 ) %>%  # row proportions
  round( 2 ) %>% 
  print()
  
  return( results.df )

}
