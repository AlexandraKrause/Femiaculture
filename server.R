

#### Building the model ####


#paying and receiving 10 years
#coefficient of variation is 1 (var_cv = var_slight)

model_function <- function(){

Compost <- c(rep (0,5), vv(var_mean = Compost_cost, var_CV = 1, 
                             n = 5))
  
Peat <- c(rep (0,5), vv(var_mean = Peat_cost, var_CV = 1, 
                          n = 5))
  
##way1
# Food waste
Compost <- Market_price_SmallFarmer - Compost

### Estimate the NPV from the model ####

NPV_Compost<- discount(Compost,discount_rate = 1, calculate_NPV = TRUE)

##way2

# Peat
Peat <- Market_price_SmallFarmer - Peat

### Estimate the NPV from the model ####

NPV_Peat<- discount(Peat,discount_rate = 1, calculate_NPV = TRUE)

###final NPV

NPV_decision_profit <- NPV_Peat - NPV_Compost

return(list(NPV_Peat = NPV_Peat,
            NPV_Compost =  NPV_Compost,
            NPV_decision_profit = NPV_decision_profit))
}


#### Run the Monte Carlo simulation using the model function ####

server <- function(input,output) {
  

  dataSource <- reactive({
    
    input_estimates <- data.frame(variable = c("Market_price_SmallFarmer",
                                               "Compost_cost", "Peat_cost"),
                                  lower = c(min(input$slider), input$substrate, input$peat),
                                  median = NA,
                                  upper = c(max(input$slider), input$substrate, input$peat),
                                  distribution = c("posnorm",
                                                   "const","const"),
                                  label = c("Market Price",
                                            "Cost food waste per salad",
                                            "Cost peat per salad"),
                                  Description = c("Salad market Price for small farmers in Euros per salad",
                                                  "Cost for food waste per salad",
                                                  "Cost for peat per Salad"))
    
    input_estimates <- input_estimates %>%
      mutate(variable = as.character(variable),
             distribution = as.character(distribution),
             label = as.character(label),
             Description = as.character(Description))
    
    
  })
  
  output$table1 <- renderTable({
    dataSource()
  })
  
  chile_mc_simulation <- reactive({
    
    chile_mc_simulation <- mcSimulation(
      estimate = as.estimate(dataSource()),
      model_function = model_function,
      numberOfModelRuns = 800,
      functionSyntax = "plainNames"
    )
#decisionsupport: raus, da nur ein packet, wo mcsimulatuon drin ist
#() da reactive function!
#model runs auf 10.000 am schluss
    
  })
  
  
  output$plot1 <- renderPlot({
    
    decisionSupport::plot_distributions(mcSimulation_object = chile_mc_simulation(), 
                                        vars = c("NPV_Peat",
                                                 "NPV_Compost"
                                        ),
                                        method = 'smooth_simple_overlay', 
                                        colors = c("purple3", "pink2", "gray32", "rosybrown1",
                                                   "gray34", "gray35", "gray36", "gray37"),
                                        base_size = 12)
    
  })
  
}

