
########################################################################################################################
#     TSP using a genetic algorithm                                                                                     #
#     By: Nigatu Adossa                                                                                                 #
# INSTRUCTION: To get the result of this program use the folowing function with giving parameters in the correct order: #
#  															#	
#           TSPinGA(cityList,noOfGeneration, mutationRate, totalNoOfEachGeneration)                                     #
#															#
#															#
#########################################################################################################################

#set the working directory to the folder that contain the distance matrix

#setwd('/Users/babyawet/Desktop/Courses/Tools for Inteligent data analysis/TSP_GA')
distance<-as.matrix(read.csv('Distance.csv',sep='\t', header=TRUE))
distance<-unlist(strsplit(distance,";"))
distanceMatrix<-matrix(distance,ncol=15)
header<-distanceMatrix[1,]
distanceMatrix<-distanceMatrix[2:16,]
colnames(distanceMatrix)<-header
row.names(distanceMatrix)<-header

#------------------------------------------------------------------------------
#The fitness function calculates the distance of the whole trip that is the fitness of the solution
fitnessFunction<-function(distMatrix, tourList){
  
  fitness=0
  tourList<-c(tourList,tourList[1])
  num_cities=length(tourList)
  for(i in 1:(num_cities-1)){
    j=(i+1)
    city_i=tourList[i]
    city_j=tourList[j]
    fitness=fitness+as.numeric(distMatrix[city_i,city_j])
  }
  myfittness<-list(fitness,tourList)
  
  return(myfittness)
}

#--------------------------------------------------------------
# For a given number of solution population one one solution will be selected based on the fitness
selection=function(population){
  distance<-c()
  for(individual in 1:nrow(population)){
    
    fit<-fitnessFunction(distanceMatrix,population[individual,])
    fitscore<-fit[[1]]
    
    distance[individual]<-fitscore
  }
  fittest<-population[which(min(distance)==distance),]
  fittest<-unique(fittest)
  return(fittest)
}
#-----------------------------------------------------------------------
#Given two solutions the crossover funnction will apply a permitution of solution to give one new solution as a child
crossingOver<-function(routA,routB){
  
  geneAExpressed=TRUE
  geneBExpressed=TRUE
  len=length(routA)
  randCity<-sample(routA,1)
  x<-which(routA==randCity)
  y<-which(routB==randCity)
  nextGeneration<-c()
  
  while(geneAExpressed==TRUE | geneBExpressed==TRUE){
    
    x = (x - 1)
    if(x<=0){
      geneAExpressed=FALSE
      geneBExpressed=FALSE
      x<-len
    }
    else{
      x = x %% len
    }
    
    y = (y + 1) 
    if(y==len){
      y<-len
    }
    else if(y>len){
      geneAExpressed=FALSE
      geneBExpressed=FALSE
      y = y %% len
    }
    else{
      y = y %% len
    }
    
    if(geneAExpressed==TRUE){
      
      if((routA[x] %in% nextGeneration)==FALSE){
        
        nextGeneration<-c(nextGeneration,routA[x])
      }
      else{
        
        geneAExpressed==FALSE
      }
      
    }
    
    if(geneBExpressed==TRUE){
      
      if((routB[y] %in% nextGeneration)==FALSE){
        
        nextGeneration<-c(nextGeneration,routB[y])
      }
      else{
        
        geneBExpressed==FALSE
      }
      
    }
    
  }
  
  if(length(nextGeneration)<len){
    
    remaining<-c()
    for(i in routA){
      if((i %in% nextGeneration)==FALSE){
        remaining<-c(remaining,i)
      }
    }
    
    remaining<-sample(remaining, length(remaining))
    nextGeneration<-c(nextGeneration,remaining)
  } 
  
  return(nextGeneration)
  
}
# Based on the mutation rate the given solution will be changed
mutation=function(solutionSpace, p){

  if(runif(1)<p){
    i<-0
    j<-0
    mutatedSolutionSpace<-solutionSpace
    while(i==j){
    
      i<-sample(1:length(solutionSpace),1)
      j<-sample(1:length(solutionSpace),1)
    }
    if(i>j){
      temp<-i
      i<-j
      j<-temp
    }
    while(i<j){
      temp<-mutatedSolutionSpace[i]
      mutatedSolutionSpace[i]<-mutatedSolutionSpace[j]
      mutatedSolutionSpace[j]<-temp
      i<-i+1
      j<-j-1
      }
    
    return(mutatedSolutionSpace)
  }else{
    return(solutionSpace)
  }
}
#Given two solution this function will crossover and mutate the new solution based on mutation rate
reproduction=function(parentA,parentB, mutationRate){
  
  child<-crossingOver(parentA,parentB)
  child<-mutation(child,mutationRate)
  return(child)
}
# This function will creat a population of individual solution given that two solution and the number of children in one population
population<-function(father, mother, mutationRate,noOfchild){
  
  
  #pop<-matrix(data=NA,nrow=noOfchild,ncol=length(father))
  pop<-data.frame()
  
  for(i in 1:noOfchild){
    pop<-rbind(reproduction(father,mother,mutationRate))
  }
  return(pop)
}

###############################################
#Given that lists of all cities(solution), number of generation(repetation), mutation rate 
#and total number of individuals(solutions) under each generation
TSPinGA=function(cityList,noOfGeneration, mutationRate, totalNoOfEachGeneration){
  
  father<-cityList #list of all cities
  mother<-sample(father,length(father))
  bestSolution<-fitnessFunction(distanceMatrix, father)[[1]] 
  
  counter<-1
  TotalDistance<-c()
  Routtrip<-c()
  Generation<-c()
  
  
  while(counter<noOfGeneration){
    
    population<-population(father, mother, mutationRate,totalNoOfEachGeneration)
    father<-selection(population)
    fitnessSCore<-fitnessFunction(distanceMatrix, father)[[1]]
    fitestTrip<-fitnessFunction(distanceMatrix, father)[[2]]
    if(fitnessSCore<bestSolution){
      bestSolution<-fitnessSCore
      mother<-father
      #print(paste("------------",counter,"generation solution-----------------"))
      
      #print(paste("-----------Total Distance = ", bestSolution, "------------------"))
      #print("------------the rout of the trip-----------")
      trip<-paste(fitestTrip,collapse="->")
      #print(trip)
      
      #myresult<-rbind(myresult,counter,bestSolution,trip)
      Generation<-c(Generation,counter)
      TotalDistance<-c(TotalDistance,bestSolution)
      Routtrip<-c(Routtrip,trip)
      
      }
    
    
    counter=counter+1
   
  }
  
  myresult<-data.frame()
  myresult<-cbind(Generation,TotalDistance,Routtrip)
  
 return(myresult)
}

#####################################################################################
#         Lets test the program with:
#         cityList=header
#         noOfGeneration=100
#         mutationRate=0.7 
#         totalNoOfEachGeneration=20, total number of individuals in each generation
#######################################################################################


TSPinGA(header,100,0.7,20)

#    mygeneration  myscore                  mytrip                                                                                                                                         
#[1,] "1"          "3879"  "Espoo->Turku->Kuopio->Vaasa->Kotka->Lappeenranta->Vantaa->Rovaniemi->Oulu->Joensuu->Pori->Jyv\212skyl\212->Tampere->Helsinki->Lahti->Espoo"   
#[2,] "2"          "3792"  "Espoo->Kuopio->Oulu->Rovaniemi->Lahti->Tampere->Lappeenranta->Joensuu->Jyv\212skyl\212->Vantaa->Kotka->Pori->Helsinki->Vaasa->Turku->Espoo"   
#[3,] "3"          "3615"  "Lahti->Lappeenranta->Rovaniemi->Joensuu->Oulu->Jyv\212skyl\212->Kuopio->Vaasa->Helsinki->Turku->Tampere->Pori->Kotka->Espoo->Vantaa->Lahti"   
#[4,] "4"          "3580"  "Tampere->Kotka->Turku->Espoo->Helsinki->Vantaa->Lahti->Jyv\212skyl\212->Oulu->Pori->Kuopio->Vaasa->Rovaniemi->Joensuu->Lappeenranta->Tampere" 
#[5,] "5"          "3365"  "Turku->Helsinki->Kotka->Vantaa->Tampere->Lahti->Pori->Vaasa->Jyv\212skyl\212->Oulu->Rovaniemi->Kuopio->Espoo->Lappeenranta->Joensuu->Turku"   
#[6,] "6"          "3224"  "Helsinki->Vantaa->Turku->Tampere->Pori->Vaasa->Espoo->Lahti->Lappeenranta->Rovaniemi->Oulu->Jyv\212skyl\212->Kotka->Joensuu->Kuopio->Helsinki"
#[7,] "7"          "3179"  "Tampere->Vaasa->Turku->Espoo->Vantaa->Lahti->Helsinki->Lappeenranta->Kotka->Kuopio->Pori->Oulu->Rovaniemi->Joensuu->Jyv\212skyl\212->Tampere" 
#[8,] "14"         "2886"  "Tampere->Espoo->Turku->Vaasa->Rovaniemi->Oulu->Pori->Joensuu->Kuopio->Jyv\212skyl\212->Lappeenranta->Kotka->Helsinki->Vantaa->Lahti->Tampere" 



