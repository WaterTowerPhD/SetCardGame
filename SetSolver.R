# Set Solver

color <- c('green','red','purple')
number <- c(1,2,3)
shape <- c('diamond','oval','squiggle')
fill <- c('solid','striped','blank')

numCards <- 12

colCard <- c(); numCard <- c(); shapeCard <- c(); fillCard <- c(); data <- c()
for(iNum in 1:numCards){
  print('[ 1 ] [ 2 ] [ 3 ] [ 4 ]')
  print('[ 5 ] [ 6 ] [ 7 ] [ 8 ]')
  print('[ 9 ] [ 10] [ 11] [ 12]')
  # correctChoice <- FALSE
  # while(!correctChoice){
    tempNumCard <- readline(prompt=paste0("Card ",iNum,", Number of Shapes: 1[1], 2[2], 3[3]? "))
    numCard <- c(numCard,as.numeric(tempNumCard))
    tempColCard <- readline(prompt=paste0("Card ",iNum,", Color of Shapes: green[1], red[2], purple[3]? "))
    colCard <- c(colCard,as.numeric(tempColCard))
    tempFillCard <- readline(prompt=paste0("Card ",iNum,", Fill of Shapes: solid[1], striped[2], blank[3]? "))
    fillCard <- c(fillCard,as.numeric(tempFillCard))
    tempShapeCard <- readline(prompt=paste0("Card ",iNum,", Shape of Shapes: diamond[1], oval[2], squiggle[3]? "))
    shapeCard <- c(shapeCard,as.numeric(tempShapeCard))
    print(paste0('Selected Card: ',number[as.numeric(tempNumCard)],' ',color[as.numeric(tempColCard)],' ',fill[as.numeric(tempFillCard)],' ',shape[as.numeric(tempShapeCard)]))
  # }
}
data <- data.frame(cbind(CardID = as.character(c(1:numCards)),Number = number[numCard],Color = color[colCard],Fill = fill[fillCard], Shape = shape[shapeCard]))
print(data)

combinations <- combn(1:numCards,3)
solutionSets <- c()
solution <- FALSE
for(iComb in 1:dim(combinations)[2]){
  numCheck <- length(unique(data[combinations[,iComb],'Number']))==3 | length(unique(data[combinations[,iComb],'Number']))==1
  colCheck <- length(unique(data[combinations[,iComb],'Color']))==3 | length(unique(data[combinations[,iComb],'Color']))==1
  shapeCheck <- length(unique(data[combinations[,iComb],'Shape']))==3 | length(unique(data[combinations[,iComb],'Shape']))==1
  fillCheck <- length(unique(data[combinations[,iComb],'Fill']))==3 | length(unique(data[combinations[,iComb],'Fill']))==1
  if(numCheck && colCheck && shapeCheck && fillCheck){
    solution <- TRUE
    solutionSets <- rbind(solutionSets,as.numeric(data[combinations[,iComb],'CardID']))
  }
}
solutionSets <- data.frame(solutionSets)
colnames(solutionSets) <- c('CardID_1','CardID_2','CardID_3')

correctChoice <- FALSE
while(!correctChoice){
  choice <- readline(prompt=paste0("Do you ONLY want to know if there is an answer [yes] or [no]? "))
  if(choice == 'yes' || choice == 'Yes' || choice == 'YES'){
    if(solution){
      print('There is a set!')
      choice <- readline(prompt=paste0("Do you want me to display all solution sets [yes] or [no]? "))
      if(choice == 'yes' || choice == 'Yes' || choice == 'YES'){
        print(solutionSets)
      }
    }
    else{
      print('There is no set...pick another card')
    }
    correctChoice <- TRUE
  } else if(choice != 'no' || choice != 'No' || choice != 'NO'){
      print('Choice not regonized...try again...')
      }
}





