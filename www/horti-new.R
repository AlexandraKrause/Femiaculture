library(readr)
library(decisionSupport)
library (DiagrammeR)
library(tidyverse)
library(ggplot2)
library(plyr)
library(dplyr)


#### Building the model ####


input_estimates <- data.frame(variable = c("Market_price_SmallFarmer", 
                                           "Compost_cost", "Peat_cost"),
                              lower = c(0.9, 0.34, 1.8),
                              median = NA,
                              upper = c(1.9, 0.34, 1.8),
                              distribution = c("posnorm", 
                                               "const","const"),
                              label = c("Price (EU/Salat)", 
                                        "Cost for food waste/Salat",
                                        "Cost for substrate/Salat"),
                              Description = c("Price in Euro per Salat",
                                              "Cost for food waste/Salat",
                                              "Cost for substrate/Salat"))

input_estimates <- input_estimates %>%
  mutate(variable = as.character(variable),
         distribution = as.character(distribution),
         label = as.character(label),
         Description = as.character(Description))


model_function <- function(){
  
  Compost <- c(rep (0,5), vv(var_mean = Compost_cost, var_CV = 1, 
                              n = 5))
  
  Peat <- c(rep (0,5), vv(var_mean = Peat_cost, var_CV = 1, 
                           n = 5))
  ##way1
  # Food waste
  Compost <- Market_price_SmallFarmer - Compost
  
  ### Estimate the NPV from the model ####
  
  NPV_no_branch<- discount(Compost,discount_rate = 1, calculate_NPV = TRUE)
  
  ##way2
  
  # Peat
  Peat <- Market_price_SmallFarmer - Peat
  
  ### Estimate the NPV from the model ####
  
  NPV_Peat<- discount(Peat,discount_rate = 1, calculate_NPV = TRUE)
  
  ###final NPV
  
  NPV_decision_profit <- NPV_Peat - NPV_no_branch
  
  return(list(NPV_no_branch =  NPV_no_branch,
              NPV_Peat = NPV_Peat,
              NPV_decision_profit = NPV_decision_profit))
}



#### Run the Monte Carlo simulation using the model function ####
#chile_mc_simulation <- mcSimulation(estimate = as.estimate(input_estimates),
#                                    model_function = model_function,
#                                    numberOfModelRuns = 800,
#                                    functionSyntax = "plainNames")

chile_mc_simulation <- decisionSupport::mcSimulation(
  estimate = decisionSupport::as.estimate(input_estimates),
  model_function = decision_function,
  numberOfModelRuns = 800,
  functionSyntax = "plainNames"
)

#change to 10.000 runs in the end

chile_mc_simulation


decisionSupport::plot_distributions(mcSimulation_object = chile_mc_simulation, 
                                    vars = c("NPV_Peat",
                                             "NPV_no_branch",
                                             "NPV_decision_profit"
                                    ),
                                    method = 'smooth_simple_overlay', 
                                    colors = c("purple3", "pink2", "gray32", "rosybrown1",
                                               "gray34", "gray35", "gray36", "gray37"),
                                    base_size = 7)

#decisionSupport::plot_distributions(mcSimulation_object = chile_mc_simulation,
#                                    vars = c("Peat", "Compost"),
#                                    method = 'smooth_simple_overlay',
#                                    colors = c("purple3", "pink2", "gray32", "rosybrown1",
#                                                   "gray34", "gray35", "gray36", "gray37"))"
#                                    base_size = 12)

                                   
                                    
