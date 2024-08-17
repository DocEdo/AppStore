######### choice set with position ######### 
library(AlgDesign)
set.seed(1000)
# generate full factorial
# gen.factorial(number of levels,number of attributes,attribute names)
full_alg<-gen.factorial(levels=c(2,24,24),nVars=3,varNames=c("rf", "appinfo","posit"),factors="all")
# nTrials = sample size?
part_alg<-optFederov(~., full_alg, nTrials = 240)
part_alg_result <- part_alg$design
as.data.frame(table(part_alg_result$rf))
as.data.frame(table(part_alg_result$appinfo))
as.data.frame(table(part_alg_result$posit))

library(choiceDes)
#generate full factorial
full_des <- dcm.design.gencand(c(2,24,24))
# dcm.design(cand, nb, sets, alts, fname=NULL, Rd=20, print=TRUE)
# design from a single candidate set, with 1 version, 72 tasks, 1 option
# 1 option including 4 alternatives, eg. [2,4,1,3]
# The final choice set in Thesis
part_des <- dcm.design(c(24,24), 1, 72, 1)
part_des_result <- part_des$levels
as.data.frame(table(part_des_result$X1))
as.data.frame(table(part_des_result$X2))

######### create choice sets ######### 
## method 1, with AlgDesign -----------------------------
# there might be repeated options in one choice set
# http://stackoverflow.com/questions/16568250/d-efficient-balanced-design-with-r

library(AlgDesign)
# define attributes and levels
desVarNames <- c("distr", "total","appinfo")
desLevels <- c(2,2,4)
n <- 4       #number of choice sets
desOpt <- 4  #num option per choice set
set.seed(1000)

#generate full factorial
dat<-gen.factorial(desLevels,length(desLevels),varNames=desVarNames,factors="all")
destT <- optFederov(~., dat, nTrials = (n*desOpt))
alg <- optBlock(~.,destT$design,c(4,4,4,4))

## method 2, with choiceDes -----------------------------
# there might be repeated options in one choice set
# if encounting error below, that means it run out of loop
# Error: is.atomic(x) is not TRUE
# In addition: Warning message:
# In mean.default(bal.ps) : argument is not numeric or logical: returning NA

library(choiceDes)

# design from a single candidate set, with 4 tasks, 4 options
des1 <- dcm.design(c(2,2), 1, 4, 4)
des1 <- dcm.design(c(24,24), 24, 1, 4)

# typical MaxDiff design with 4 items, 4options, 1 version, 4 tasks
des2 <- tradeoff.des(4, 4, 1, 4)

library(plyr) # for rename column name
## method 3-----------------------------
# from paper: (2008)_design and analysis of choice experiments using R a brief introduction
# from http://r.789695.n4.nabble.com/Design-of-experiments-for-Choice-Based-Conjoint-Analysis-CBC-td2254142.html
# false to write.csv, if it run out of loop 
distr_lvl <-  c(1,2) #1=J, 2=U
total_lvl <-  c(1,2) #1= Low, 2=High

# full factorial design
levels<-c(length(distr_lvl), length(total_lvl))
ffd <- gen.factorial(levels = levels,varNames = c("distr", "total"),factors="all")

rowNum <- function (dataframe_input) { #function to assign new row numbers after ordering
  dataframe_input$new_order<-0
  for (i in 1:nrow(dataframe_input)) {
    dataframe_input$new_order[i]<-i
  }
  return(dataframe_input)
}

makeDesign <- function(ffd_design) {
  for(r in 1:100) {
    print (paste("try #",r))
    set.seed(54321+r)
    alt1<-ffd_design
    alt2<-alt1
    alt3<-alt1
    alt4<-alt1
    alt2 <- rename(alt2, c("distr" = "distr2","total" = "total2"))
    alt3 <- rename(alt3, c("distr" = "distr3","total" = "total3"))
    alt4 <- rename(alt4, c("distr" = "distr4","total" = "total4"))

    # random selection
    alt1 <- transform(alt1, r1 = runif(nrow(alt1)))
    alt2 <- transform(alt2, r2 = runif(nrow(alt2)))
    alt3 <- transform(alt3, r3 = runif(nrow(alt3)))
    alt4 <- transform(alt4, r4 = runif(nrow(alt4)))

    alt1_sort <- alt1[order(alt1$r1),]
    alt2_sort <- alt2[order(alt2$r2),]
    alt3_sort <- alt3[order(alt3$r3),]
    alt4_sort <- alt4[order(alt4$r4),]

    alt1_sort <- rowNum(alt1_sort)
    alt2_sort <- rowNum(alt2_sort)
    alt3_sort <- rowNum(alt3_sort)
    alt4_sort <- rowNum(alt4_sort)

    # design formation
    merge1 <- merge(alt1_sort,alt2_sort,by = "new_order")
    merge2 <- merge(merge1,alt3_sort,by = "new_order")
    merge3 <- merge(merge2,alt4_sort,by = "new_order")
    survey_design <- merge3[,c(2,3,5,6,8,9,11,12)] #put all 4 sets in same data frame

    countFlawedTasks = 0
    for (ii in 1:nrow(survey_design)) {
      profile1 = as.numeric(paste(survey_design[ii,1:2],collapse = ""))
      profile2 = as.numeric(paste(survey_design[ii,3:4],collapse = ""))
      profile3 = as.numeric(paste(survey_design[ii,5:6],collapse = ""))
      profile4 = as.numeric(paste(survey_design[ii,7:8],collapse = ""))
      if (profile1==profile2 || profile1==profile3 || profile1==profile4 || profile2==profile3 || profile2==profile4 || profile3==profile4) {
        countFlawedTasks = countFlawedTasks + 1
      }
    }
    if (countFlawedTasks == 0) {
      write.csv (survey_design, file = "surveyDesign.csv")
      print (paste("Found feasible design after try #", r))
      return (paste(profile1, profile2, profile3, profile4, sep = " "))
      break # stop loop
    }
  }
}

makeDesign(ffd)
