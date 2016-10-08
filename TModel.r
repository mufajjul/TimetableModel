###############################################################################################################################################################
# Timetable Model
# Author Mufy 
# Date 24/09/2016
# Description --  This script models the teaching hours per academic based on their teaching load.  The data for the model is extracted from the timetabling 
#  spreadsheet.

##############################################################################################################################################################

#library(readxl)
library(xlsx)
require(xlsx)
#library(RODBC)
library(hashmap)


home.dr<- getwd()

#Set path to the timetable
setwd("/Users/mufy/Dropbox/teaching/UEL/2015-2016/timetable")

#Change the filename if changes or updated
file.name <- "CSI-2016-17-v9-final.xlsx"
sheet.name <- "ML"

tAllocation <- read.xlsx(file.name, 4, header=FALSE, keepFormulas=FALSE, startRow = 2, endRow =25)
timetable <- read.xlsx(file.name, 3, header=FALSE, keepFormulas=FALSE, startRow = 2, endRow =187)

# Get the current list of staff names 
staffNames <- tAllocation[1]

#get all the teaching allocated staff names 
allocatedStaffNamesOnTimetable <- timetable[12];


#List to hold all the calculated values

#TODO - Find a way to initilize
staffAllocationMap <-  hashmap("","")
staffAllocationMap$erase("")


# Functions for calculating all the teaching hours 

calculateLectureHours <- function(semester, hours, npar=TRUE,print=TRUE){
	
	hourCalc <- 0
	
	#cat ("\n calc semester: ", semester, " : hours: ", hours, "\n")
	
	if (toString(semester) == "1 & 2"){
		
		hourCalc =	(2* hours * 24)/43
	#	cat ("\n hour calc: ", hourCalc, "\n")
		return (hourCalc)
		
	}else{
		hourCalc =	(2*hours * 12)/43
	#	cat ("\n hour calc: ", hourCalc, "\n")
		return (hourCalc)
		
	}
}

calculateTutorialHours <- function(semester, hours, npar=TRUE,print=TRUE){
	
	hourCalc <- 0
	
	if (semester == "1 & 2"){		
		hourCalc <-	(1.5* hours * 24)/43
	}else{
		hourCalc <-	(1.5* hours * 12)/43
	}
	
	return (hourCalc)
}

#TODO - Should take into account the module length, currently only module size
calculateModuleLeadership <- function(semester, moduleSize, npar=TRUE,print=TRUE){
		
	small <- 25
	medium <- 75
		
	#cat ("\n module size: ", moduleSize, "\n")	
	
	if (as.numeric(moduleSize) < small) {
		return (1)
	}else if ((as.numeric(moduleSize) > small) & (as.numeric(moduleSize) < medium)){
		return (1.5)
	}else{
		return (2)
	}
}


allocationSummary <- function (){
    
    cat ("\n############################### Summary ########################### \n")


    keys <- staffAllocationMap$keys()
    
    for (key in keys){
        
            cat ("staff: ", key, "  ====>  allocation: ", staffAllocationMap$find(key), "\n")
    }
    
    cat ("\n################################################################### \n")

    
}


#TODO -- Not very efficient algorithm, improve the algorith from 0(N^2) to (NLogN) 
#IMPROVEMENT -- consider using HashMAP


for (i in 1: nrow (staffNames)){
	print (toString(staffNames[i,1]))
	
	totalAllocation <- 0
	totalModuleLeadership <-0
	
	for (j in 1: nrow(allocatedStaffNamesOnTimetable)) {
		if (toString(staffNames[i,1]) == toString(allocatedStaffNamesOnTimetable[j,1])){
			if (toString(timetable[j:j,4]) == "Lecture"){
							
				#calculate mornalized teaching time 			
				normalizedHours <- calculateLectureHours (toString(timetable[j:j,5]), (as.numeric(toString(timetable[j:j,9])))*24) 
				totalAllocation <- totalAllocation + normalizedHours							

				# calculate module leadership 
				moduleLeadership <- calculateModuleLeadership(toString(timetable[j:j,5]), (as.numeric(toString(timetable[j:j,11]))))			
				totalModuleLeadership <- totalModuleLeadership+ moduleLeadership;
							
				cat ("current lec allocation: ", normalizedHours, "total allocation: ",totalAllocation, " module code: ",toString(timetable[j:j,2]), "module size:", (as.numeric(toString(timetable[j:j,11]))),  " ;semester: ", toString(timetable[j:j,5]), "; hours: ", (as.numeric(toString(timetable[j:j,9])))*24, "module leadership: ", moduleLeadership, "\n")
			}else{

				normalizedHours <- calculateLectureHours (toString(timetable[j:j,5]), (as.numeric(toString(timetable[j:j,9])))*24) 
				totalAllocation <- totalAllocation + normalizedHours

				cat ("current tut allocation: ", normalizedHours, "total allocation: ",totalAllocation, " module code: ",toString(timetable[j:j,2]), "module size:", (as.numeric(toString(timetable[j:j,11]))), " ;semester: ", toString(timetable[j:j,5]), "; hours: ", (as.numeric(toString(timetable[j:j,9])))*24, "\n")				
			}
			
			#Handle joint module leaders 
		}else{ 
		
			if (regexpr("/", toString(allocatedStaffNamesOnTimetable[j,1]))[1]!=-1){
				namesList <- unlist(strsplit(toString(allocatedStaffNamesOnTimetable[j,1]), "/"))

				for (k in 1: length(namesList)){
				
					if (toString(staffNames[i,1]) == namesList[k]){
						if (toString(timetable[j:j,4]) == "Lecture"){
							
							#calculate mornalized teaching time 			
							normalizedHours <- calculateLectureHours (toString(timetable[j:j,5]), ((as.numeric(toString(timetable[j:j,9])))*24)/length(namesList)) 
							totalAllocation <- totalAllocation + normalizedHours							

								cat ("Shared current lec allocation: ", normalizedHours, "total allocation: ",totalAllocation, " module code: " , toString(timetable[j:j,2]), "module size:", (as.numeric(toString(timetable[j:j,11]))), " ;semester: ", toString(timetable[j:j,5]), "; hours: ", (as.numeric(toString(timetable[j:j,9])))*24, "module leadership: ", moduleLeadership, "\n")


							# calculate module leadership, only the first person is the module leader 
							if (k ==1){				
								moduleLeadership <- calculateModuleLeadership(toString(timetable[j:j,5]), (as.numeric(toString(timetable[j:j,11]))))			
								totalModuleLeadership <- totalModuleLeadership+ moduleLeadership;
							
								cat ("Shared current lec allocation, with module leadership: ", normalizedHours, "total allocation: ",totalAllocation, " module code: " , toString(timetable[j:j,2]), "module size:", (as.numeric(toString(timetable[j:j,11]))), " ;semester: ", toString(timetable[j:j,5]), "; hours: ", (as.numeric(toString(timetable[j:j,9])))*24, "module leadership: ", moduleLeadership, "\n")
							}					
						}else{

							normalizedHours <- calculateLectureHours (toString(timetable[j:j,5]), ((as.numeric(toString(timetable[j:j,9])))*24)/length(namesList)) 
							totalAllocation <- totalAllocation + normalizedHours

							cat ("Shared current tut allocation: ", normalizedHours, "total allocation: ",totalAllocation, " module code: " ,toString(timetable[j:j,2]),  "module size:", (as.numeric(toString(timetable[j:j,11]))), " ;semester: ", toString(timetable[j:j,5]), "; hours: ", (as.numeric(toString(timetable[j:j,9])))*24, "\n")				
						}
						
					break	
					}
				}				
			}
		}
		
	}
	
	totalAllocation <- totalAllocation + totalModuleLeadership;
 	cat (" Total Allocation time with module leadership for: ",toString(staffNames[i,1]), " is: ", totalAllocation, "\n")
    
    staffAllocationMap$insert(toString(staffNames[i,1]),totalAllocation)

    totalAllocation <- 0
	totalModuleLeadership <-0
    
  		
}

allocationSummary()

#Set the homedirect to the default working directory
setwd(home.dr)








