library(mirt)
library(lavaan)
library(stringr)
library(dplyr)

# Importing Data from Github
HCP <- data.table::fread('https://raw.githubusercontent.com/benlistyg/gender_mediation/master/HCP%20gender%20data.csv')

# Hypointens have all zeros, removing these values since the models won't run when a mediator has 0 variance.
HCP$FS_R_WM_Hypointens_Vol <- NULL
HCP$FS_L_NonWM_Hypointens_Vol <- NULL
HCP$FS_R_NonWM_Hypointens_Vol <- NULL
HCP$FS_L_WM_Hypointens_Vol <- NULL

# Subsetting Narcissm items that don't require reverse scoring.
# Adding back SubjectID, FamilyID, and Gender to dataset
  # Will join on these variables further down.
# Items 6, 11, 21, 26, 36, 41, 51, 56 don't need reverse scoring
N <- HCP %>% 
  select(NEORAW_06,
         NEORAW_11,
         NEORAW_21,
         NEORAW_26,
         NEORAW_36,
         NEORAW_41,
         NEORAW_51,
         NEORAW_56) %>% 
  apply(X = ., MARGIN = 2, FUN = function(x) recode(x, 
                                                    SA = 5,
                                                    A  = 4,
                                                    N  = 3,
                                                    D  = 2,
                                                    SD = 1)) %>% 
  as.data.frame() %>% 
  cbind(Subject = HCP$Subject,
        Family_ID = HCP$Family_ID,
        Gender = HCP$Gender,
        .) %>% 
  .[complete.cases(.),]

# Subsetting Narcissm items that DO require reverse scoring (see Rollock and Lui (2015) for reference of items, 
        # https://sci-hub.tw/https://doi.org/10.1177/1073191115590854)
# Adding back SubjectID, FamilyID, and Gender to dataset
# Will join on these variables further down.
# Items 1, 16, 31, and 46 need reverse scoring
Reversed_N <- HCP %>% 
  select(NEORAW_01,
         NEORAW_16,
         NEORAW_31,
         NEORAW_46) %>% 
  apply(X = ., MARGIN = 2, FUN = function(x) recode(x, 
                                                    SA = 1,
                                                    A  = 2,
                                                    N  = 3,
                                                    D  = 4,
                                                    SD = 5)) %>% 
  as.data.frame() %>% 
  cbind(Subject = HCP$Subject,
        Family_ID = HCP$Family_ID,
        Gender = HCP$Gender,
        .) %>% 
  .[complete.cases(.),]

# Joining normal and reversed scored items back together into full_N data frame.
# Row sum is included to ensure items are adequately sum-scored compared to the NEOFAC_N variable
  # Scoring involves making items range from 0 to 4 as opposed to 1 to 5
# This process is repeated for all Big 5 traits
full_N <- merge(N, Reversed_N, by = c('Subject','Family_ID','Gender'))
full_N$rowsum <- full_N[,-c(1:3)] %>% rowSums(., na.rm = T)

        
# Extraversion
# Items 2, 7, 17, 22, 32, 37, 47, 52 don't need reverse scoring
E <- HCP %>% 
  select(NEORAW_02,
         NEORAW_07,
         NEORAW_17,
         NEORAW_22,
         NEORAW_32,
         NEORAW_37,
         NEORAW_47,
         NEORAW_52) %>% 
  apply(X = ., MARGIN = 2, FUN = function(x) recode(x, 
                                                    SA = 5,
                                                    A  = 4,
                                                    N  = 3,
                                                    D  = 2,
                                                    SD = 1)) %>% 
  as.data.frame() %>% 
  cbind(Subject = HCP$Subject,         
        Family_ID = HCP$Family_ID,         
        Gender = HCP$Gender,
        .) %>% 
  .[complete.cases(.),]

# Items 12, 27, 42, and 57 need to be reverse scored
Reversed_E <- HCP %>% 
  select(NEORAW_12,
         NEORAW_27,
         NEORAW_42,
         NEORAW_57) %>% 
  apply(X = ., MARGIN = 2, FUN = function(x) recode(x, 
                                                    SA = 1,
                                                    A  = 2,
                                                    N  = 3,
                                                    D  = 4,
                                                    SD = 5)) %>% 
  as.data.frame() %>% 
  cbind(Subject = HCP$Subject,         
        Family_ID = HCP$Family_ID,         
        Gender = HCP$Gender,
        .) %>% 
  .[complete.cases(.),]

full_E <- merge(E, Reversed_E, by = c('Subject','Family_ID','Gender'))
full_E$rowsum <- full_E[,-c(1:3)] %>% rowSums(., na.rm = T)

# Openness to Experience
# Items 3, 8, 18, 23, 33, 38, 48 need reverse scoring
Reversed_O <- HCP %>% 
  select(NEORAW_03,
         NEORAW_08,
         NEORAW_18,
         NEORAW_23,
         NEORAW_33,
         NEORAW_38,
         NEORAW_48) %>% 
  apply(X = ., MARGIN = 2, FUN = function(x) recode(x, 
                                                    SA = 1,
                                                    A  = 2,
                                                    N  = 3,
                                                    D  = 4,
                                                    SD = 5)) %>% 
  as.data.frame() %>% 
  cbind(Subject = HCP$Subject,         
        Family_ID = HCP$Family_ID,         
        Gender = HCP$Gender,
        .) %>% 
  .[complete.cases(.),]

# Items 13, 28, 43, 53, 58 don't need reverse scoring
O <- HCP %>% 
  select(NEORAW_13,
         NEORAW_28,
         NEORAW_43,
         NEORAW_53,
         NEORAW_58) %>% 
  apply(X = ., MARGIN = 2, FUN = function(x) recode(x, 
                                                    SA = 5,
                                                    A  = 4,
                                                    N  = 3,
                                                    D  = 2,
                                                    SD = 1)) %>% 
  as.data.frame() %>% 
  cbind(Subject = HCP$Subject,         
        Family_ID = HCP$Family_ID,         
        Gender = HCP$Gender,
        .) %>% 
  .[complete.cases(.),]

full_O <- merge(O, Reversed_O, by = c('Subject','Family_ID','Gender'))
full_O$rowsum <- full_O[,-c(1:3)] %>% rowSums(., na.rm = T)

# Agreeableness
# Items 9, 14, 24, 29, 39, 44, 54, 59 need reverse scoring
Reversed_A <- HCP %>% 
  select(NEORAW_09,
         NEORAW_14,
         NEORAW_24,
         NEORAW_29,
         NEORAW_39,
         NEORAW_44,
         NEORAW_54,
         NEORAW_59) %>% 
  apply(X = ., MARGIN = 2, FUN = function(x) recode(x, 
                                                    SA = 1,
                                                    A  = 2,
                                                    N  = 3,
                                                    D  = 4,
                                                    SD = 5)) %>% 
  as.data.frame() %>% 
  cbind(Subject = HCP$Subject,         
        Family_ID = HCP$Family_ID,         
        Gender = HCP$Gender,
        .) %>% 
  .[complete.cases(.),]

# Items 4, 19, 39, 49 don't need reverse scoring
A <- HCP %>% 
  select(NEORAW_04,
         NEORAW_19,
         NEORAW_34,
         NEORAW_49) %>% 
  apply(X = ., MARGIN = 2, FUN = function(x) recode(x, 
                                                    SA = 5,
                                                    A  = 4,
                                                    N  = 3,
                                                    D  = 2,
                                                    SD = 1)) %>% 
  as.data.frame() %>% 
  cbind(Subject = HCP$Subject,         
        Family_ID = HCP$Family_ID,         
        Gender = HCP$Gender,
        .) %>% 
  .[complete.cases(.),]

full_A <- merge(A, Reversed_A, by = c('Subject','Family_ID','Gender'))
full_A$rowsum <- full_A[,-c(1:3)] %>% rowSums(., na.rm = T)

# Conscientiousness
# Items 5, 10, 20, 25, 35, 40, 50, 60 don't need reverse scoring
C <- HCP %>% 
  select(NEORAW_05,
         NEORAW_10,
         NEORAW_20,
         NEORAW_25,
         NEORAW_35,
         NEORAW_40,
         NEORAW_50,
         NEORAW_60) %>% 
  apply(X = ., MARGIN = 2, FUN = function(x) recode(x, 
                                                    SA = 5,
                                                    A  = 4,
                                                    N  = 3,
                                                    D  = 2,
                                                    SD = 1)) %>% 
  as.data.frame() %>% 
  cbind(Subject = HCP$Subject,         
        Family_ID = HCP$Family_ID,         
        Gender = HCP$Gender,
        .) %>% 
  .[complete.cases(.),]

# Items 15, 30, 45, 55 need to be reverse scored
Reversed_C <- HCP %>% 
  select(NEORAW_15,
         NEORAW_30,
         NEORAW_45,
         NEORAW_55) %>% 
  apply(X = ., MARGIN = 2, FUN = function(x) recode(x, 
                                                    SA = 1,
                                                    A  = 2,
                                                    N  = 3,
                                                    D  = 4,
                                                    SD = 5)) %>% 
  as.data.frame() %>% 
  cbind(Subject = HCP$Subject,         
        Family_ID = HCP$Family_ID,         
        Gender = HCP$Gender,
        .) %>% 
  .[complete.cases(.),]

full_C <- merge(C, Reversed_C, by = c('Subject','Family_ID','Gender'))
full_C$rowsum <- full_C[,-c(1:3)] %>% rowSums(., na.rm = T)

#####

# IRT Scoring #
# Removing the sumscore, subjectid, familyid, and gender to IRT score the items
# Each big 5 is scored independently
# Missing items are removed.

N_model <- mirt(full_N[,-c(1:3,ncol(full_N))],1,technical = list(removeEmptyRows=TRUE))
E_model <- mirt(full_E[,-c(1:3,ncol(full_E))],1,technical = list(removeEmptyRows=TRUE))
O_model <- mirt(full_O[,-c(1:3,ncol(full_O))],1,technical = list(removeEmptyRows=TRUE))
A_model <- mirt(full_A[,-c(1:3,ncol(full_A))],1,technical = list(removeEmptyRows=TRUE))
C_model <- mirt(full_C[,-c(1:3,ncol(full_C))],1,technical = list(removeEmptyRows=TRUE))

#####

# Combining Subject ID, FamilyID, Gender, and IRT Thetas into long dataframe.
        # `fscores` is from the MIRT package and returns IRT theta values from a fitted IRT model

B5_data <- rbind(
  data.frame(
    Subject = N$Subject,
    Family_ID = N$Family_ID,
    N_scores = N_model %>% fscores,
    Gender = N$Gender,
    B5 = 'N'),
  
  data.frame(
    Subject = E$Subject,
    Family_ID = E$Family_ID,
    E_scores = E_model %>% fscores,
    Gender = E$Gender,
    B5 = 'E'),
  
  data.frame(
    Subject = O$Subject,
    Family_ID = O$Family_ID,
    O_scores = O_model %>% fscores,
    Gender = O$Gender,
    B5 = 'O'),
  
  data.frame(
    Subject = A$Subject,
    Family_ID = A$Family_ID,
    A_scores = A_model %>% fscores,
    Gender = A$Gender,
    B5 = 'A'),
  
  data.frame(
    Subject = C$Subject,
    Family_ID = C$Family_ID,
    C_scores = C_model %>% fscores,
    Gender = C$Gender,
    B5 = 'C'))

# Creating long dataframe of brain data (based on the variables that have "Thck", "Vol" or "Area" in their name
        # 260 in total
# Joining brain data with B5 data on subjectid and family ID

Brain_data <- HCP %>% 
  select(Subject,
         Family_ID,
         contains("Thck"),
         contains("Vol"),
         contains("Area")) %>% 
  reshape2::melt(id.vars = c('Subject',"Family_ID")) %>% 
  `colnames<-`(c("Subject","Family_ID","Brain_Area",'Brain_Value'))

full_data <- merge(B5_data,
                   Brain_data, 
                   by=c("Subject","Family_ID"))

#####

# Function to iterate over long dataframe
# This function takes in a Big 5 Trait and Brain Area
# Subsets the long dataframe (full_data)
# Fits a multilevel mediation model
        # Family ID is the cluster
# Returns parameter estimates for the fitted model
# Takes around 15 minutes to run over all 1300 combinations of B5 x Brain Region
model_function <- function(personality,
                           brain_area,
                           ...){
  
  
  
  in_function_data <- full_data %>% 
    filter(B5 == personality,
           Brain_Area == brain_area) %>% 
    mutate(Gender = recode(Gender, 
                           'M' = 0, # Recoding males as a 0
                           'F' = 1))# Recoding females as a 1
  
  Data <- data.frame(X = in_function_data$Gender, 
                     Y = scale(in_function_data$F1), # Z-scoring the IRT thetas, interpret this as SD increase / decrease
                     M = scale(in_function_data$Brain_Value), #Z-scoring the brain region, interpret this as SD increase / decrease
                     Family_ID = in_function_data$Family_ID)
  
  model <- 'level: 1
            # direct effect
             Y ~ c*X
           # mediator
             M ~ a*X
             Y ~ b*M
           # indirect effect (a*b)
             ab := a*b
           # total effect
             total := c + (a*b)
          
          level: 2
           # direct effect
             Y ~ c*X
           # mediator
             M ~ a*X
             Y ~ b*M
           # indirect effect (a*b)
             ab := a*b
           # total effect
             total := c + (a*b)
         '
  # Using robust maximum likelihood for estimation
  fit <- sem(model, 
             data = Data,
             cluster = 'Family_ID',
             estimator = 'MLR')
  
  #Adding back the brain region and personality trait to the parameter estimates dataframe
  cbind(
    parameterestimates(fit, standardized = T),
    personality,
    brain_area) %>% 
    return() %>% 
    print()
}

#####

#Conditions is a dataframe with all possible combinations of B5 and Brain Region
        # This is what model_function iterates over
conditions <- expand.grid(unique(full_data$B5),
                          unique(full_data$Brain_Area)) %>% 
  `colnames<-`(c('personality',"brain_area"))

# This is the part that does the row-wise iteration
        # final_data contains all the lavaan parameter estimates from the 1300 combinations
        
final_data <- plyr::mdply(.data = conditions, 
                          .fun = model_function)

#####











