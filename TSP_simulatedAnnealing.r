
###############################################################################
#       TSP using simulated annealing                                         #
#       By:Nigatu Adossa                                                      #
#         Student no. :95542                                                              #
##############################################################################

#set the working directory to the folder that contain the r code and the csv file
setwd('/Users/babyawet/Desktop/Courses/Tools for Inteligent data analysis/TSP_simulated_annealingEdited')


#read the distance matrix from the csv file
distance<-as.matrix(read.csv('Distance.csv',sep='\t', header=TRUE))
distance<-unlist(strsplit(distance,";"))
distanceMatrix<-matrix(distance,ncol=15)
header<-distanceMatrix[1,]
distanceMatrix<-distanceMatrix[2:16,]
colnames(distanceMatrix)<-header
row.names(distanceMatrix)<-header
#################################################################
#                 simulated annealing algorithm                 #           
########################################################################################################
#1. pick an initial solution                                                                            #
#2. set an initial temperature                                                                          #
#3. choose the next neighbour of the current solution:                                                  #
#  if the neighbour is ???better??? make that neighbour the current solution                                #
#  if the neighbour is ???worse???, probabilistically make this neighbour                                   #
#       the current solution, based on the current temperature andhow much ???worse??? the neighbour is     #
#4. decrease the temperature slightly                                                                   #
#5. go to 3.                                                                                            #
########################################################################################################

#calculating all possible routes
allRout<-function(citylist){
  
  listOfArrivalCity<-sample(citylist,length(citylist))#permituates the list of starting cities
  listOfDestinationCity<-sample(citylist, length(citylist)) #permituates the list of destination cities
  allrout<-list()
  z<-1
  for(i in listOfArrivalCity){
    
    for(j in listOfDestinationCity){
   
      allrout[z]<-paste(i,",",j)
      z=z+1

    }
    
  }
  return(allrout)#returnes all possible routs
}

#all possible moves at a time returned as a row in the matrix
moveOperater<-function(solutionSpace){
  
  allPossibleRouts<-unlist(allRout(solutionSpace))
  routs<-matrix(data=NA,nrow=length(allPossibleRouts),ncol=2)
  for(i in 1:length(allPossibleRouts)){
    routs[i,]<-c(gsub(" ","",(unlist(strsplit(allPossibleRouts[i],",")))))
    
  }
  return(routs)
}
#swap one city from teh solution space
swapCity<-function(solutionState,city1,city2){
  
  city1Index<-which(solutionState==city1)
  city2Index<-which(solutionState==city2) 
  solutionState[city1Index]<-city2
  solutionState[city2Index]<-city1
  
  return(solutionState)
}
#calculate the acceptance probability
acceptanceProbability<-function(prev_score,next_score, temp){
  
    if(prev_score > next_score)
        return(1.0)
    else
      return(exp(-abs(next_score-prev_score)/temp))
}

#cooling schedule based on the the initial temprature and the alpha value selected.
coolingSchedul<-function(initialTemp, alpha){
  
  coolSch<-c()
  T=initialTemp
  while(T>.0001){
    
    T=alpha*T
    coolSch<-c(coolSch,T)
  }
  coolSch<-coolSch[!is.na(coolSch)]#remove the initial na from the vector
  return(coolSch)
  
}
#calculating the initial solution space

initialFunction<-function(solutionSpace){
  
  initialSolution<-sample(solutionSpace,length(solutionSpace))
  
  return(initialSolution)
                          
}
#objective function calculting the score of the whole path
objectiveFunction<-function(distMatrix, tourList){
  
  total=0
  tourList<-c(tourList,tourList[1])
  num_cities=length(tourList)
  for(i in 1:(num_cities-1)){
    j=(i+1)
    city_i=tourList[i]
    city_j=tourList[j]
    total=total+as.numeric(distMatrix[city_i,city_j])
  }
  
  return(total)
}

#we need to define the current solution and current score as a global variable
currentSolution<-initialFunction(header)# the header hear is the list of the all cites
#print(currentSolution)
currentScore<-objectiveFunction(distanceMatrix,currentSolution)#calculats the score of current solution
                ################################
                #simulated annealing algorithum #
                ################################
simulatedAnneling<-function(start_temp,alpha,max_iteration){
  counter=1
  Iteration<-c()
  TotalDistance<-c()
  Temprature<-c()
  Trip<-c()
  cooling_Schedule<-coolingSchedul(start_temp,alpha)# get the cooling schedule
  for(temp in cooling_Schedule){ #iterate in the cooling schedule
    done=FALSE
    for(i in 1:nrow(moveOperater(currentSolution))){
      if(counter>max_iteration){ #number of itheration greater than the maximum iteration then stope the itheration
        
        done=TRUE
        break
      }
      #calculate the next solution by changing one move at a time
      nextSolution<-swapCity(currentSolution,moveOperater(currentSolution)[i,1],moveOperater(currentSolution)[i,2])
      #calculate the score of the next solution
      next_score=objectiveFunction(distanceMatrix,nextSolution)
      counter=counter+1 # increament the counter by one 
      #now we can decide weather we should accept or reject 
      #the new solution based on acceptance probability
      acceptance<-acceptanceProbability(currentScore,next_score,temp)
      if(runif(1)<acceptance){  #if acceptance probability geater than randome number between 0 & 1, 
                                #we accept it as a current solution and go for the next solution
        currentSolution<-nextSolution
        currentScore<-next_score
        break
      }    

    }
    
    if(done){ #if we finnished our iteration the break the loop 
      
      break
    }
    Iteration<-c(Iteration,counter)
    trip<-paste(c(currentSolution,currentSolution[1]),collapse="->")
    Trip<-c(Trip,trip)
    TotalDistance<-c(TotalDistance,currentScore)
    Temprature<-c(Temprature,temp)

  }
  
  result<-data.frame()
  result<-cbind(Iteration,Temprature,TotalDistance,Trip)
  
  return(result)
  
}

#################################################################################
#               Testing the program                                             #
#       Lets use:
#                starting temprature=10,
#                alpha=.5
#               maximum iteration=10
#
################################################################################
simulatedAnneling(10,0.5,10)

#-----------------------------RESULT--------------------------------------------------

#    Iteration Temprature TotalDistance
#[1,] "2"       "5"        "4754"       
#[2,] "4"       "2.5"      "4491"       
#[3,] "5"       "1.25"     "4373"       
#[4,] "6"       "0.625"    "4196"       
#[5,] "7"       "0.3125"   "4034"       
#[6,] "8"       "0.15625"  "4034"       
#[7,] "11"      "0.078125" "3949"       
#                                               Trip                                                                                                                                           
#[1,] "Helsinki->Rovaniemi->Lappeenranta->Joensuu->Pori->Espoo->Vaasa->Lahti->Oulu->Kuopio->Vantaa->Tampere->Turku->Jyv\212skyl\212->Kotka->Helsinki"
#[2,] "Joensuu->Rovaniemi->Lappeenranta->Helsinki->Pori->Espoo->Vaasa->Lahti->Oulu->Kuopio->Vantaa->Tampere->Turku->Jyv\212skyl\212->Kotka->Joensuu" 
#[3,] "Joensuu->Rovaniemi->Lappeenranta->Helsinki->Pori->Oulu->Vaasa->Lahti->Espoo->Kuopio->Vantaa->Tampere->Turku->Jyv\212skyl\212->Kotka->Joensuu" 
#[4,] "Joensuu->Rovaniemi->Lappeenranta->Helsinki->Jyv\212skyl\212->Oulu->Vaasa->Lahti->Espoo->Kuopio->Vantaa->Tampere->Turku->Pori->Kotka->Joensuu" 
#[5,] "Joensuu->Rovaniemi->Lappeenranta->Helsinki->Jyv\212skyl\212->Oulu->Vaasa->Lahti->Kuopio->Espoo->Vantaa->Tampere->Turku->Pori->Kotka->Joensuu" 
#[6,] "Joensuu->Rovaniemi->Lappeenranta->Helsinki->Jyv\212skyl\212->Oulu->Vaasa->Lahti->Kuopio->Espoo->Vantaa->Tampere->Turku->Pori->Kotka->Joensuu" 
#[7,] "Joensuu->Vaasa->Lappeenranta->Helsinki->Jyv\212skyl\212->Oulu->Rovaniemi->Lahti->Kuopio->Espoo->Vantaa->Tampere->Turku->Pori->Kotka->Joensuu" 





