library(mirt)
library(lavaan)
library(stringr)

HCP <- data.table::fread('C:/Users/Ben/Downloads/HCP gender data.csv')

N <- HCP %>% 
  select(NEORAW_01,
         NEORAW_06,
         NEORAW_11,
         NEORAW_16,
         NEORAW_21,
         NEORAW_26,
         NEORAW_31,
         NEORAW_36,
         NEORAW_41,
         NEORAW_46,
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

E <- HCP %>% 
  select(NEORAW_02,
         NEORAW_07,
         NEORAW_12,
         NEORAW_17,
         NEORAW_22,
         NEORAW_27,
         NEORAW_32,
         NEORAW_37,
         NEORAW_42,
         NEORAW_47,
         NEORAW_52,
         NEORAW_57) %>% 
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

O <- HCP %>% 
  select(NEORAW_03,
         NEORAW_08,
         NEORAW_13,
         NEORAW_18,
         NEORAW_23,
         NEORAW_28,
         NEORAW_33,
         NEORAW_38,
         NEORAW_43,
         NEORAW_48,
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

A <- HCP %>% 
  select(NEORAW_04,
         NEORAW_09,
         NEORAW_14,
         NEORAW_19,
         NEORAW_24,
         NEORAW_29,
         NEORAW_34,
         NEORAW_39,
         NEORAW_44,
         NEORAW_49,
         NEORAW_54,
         NEORAW_59) %>% 
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

C <- HCP %>% 
  select(NEORAW_05,
         NEORAW_10,
         NEORAW_15,
         NEORAW_20,
         NEORAW_25,
         NEORAW_30,
         NEORAW_35,
         NEORAW_40,
         NEORAW_45,
         NEORAW_50,
         NEORAW_55,
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

#####

N_model <- mirt(N[,-c(1:3)],1,technical = list(removeEmptyRows=TRUE))
E_model <- mirt(E[,-c(1:3)],1,technical = list(removeEmptyRows=TRUE))
O_model <- mirt(O[,-c(1:3)],1,technical = list(removeEmptyRows=TRUE))
A_model <- mirt(A[,-c(1:3)],1,technical = list(removeEmptyRows=TRUE))
C_model <- mirt(C[,-c(1:3)],1,technical = list(removeEmptyRows=TRUE))

#####

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

model_function <- function(personality,
                          brain_area,
                          ...){
  
  in_function_data <- full_data %>% 
    filter(B5 == personality,
           Brain_Area == brain_area) %>% 
    mutate(Gender = recode(Gender, 
           M = 0,
           F = 1))
  
  Data <- data.frame(X = in_function_data$Gender, 
                     Y = in_function_data$F1, 
                     M = scale(in_function_data$Brain_Value),
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
  fit <- sem(model, 
             data = Data,
             cluster = 'Family_ID',
             estimator = 'MLR')
  
  cbind(
    parameterestimates(fit, standardized = T),
    personality,
    brain_area) %>% 
    return() %>% 
    print()
}

#####

conditions <- expand.grid(unique(full_data$B5),
                          unique(full_data$Brain_Area)) %>% 
  `colnames<-`(c('personality',"brain_area"))

final_data <- plyr::mdply(.data = conditions, 
            .fun = model_function)

#####





























