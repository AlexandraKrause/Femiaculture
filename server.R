

#### Building the model ####

### Start of the function ###
#paying and receiving 10 years
#coefficient of variation is 1 (var_cv = var_slight)


decision_function <- function(x, varnames){
  
  #Risk
  
  safety_payout <- chance_event(safety_risk, 1, 0, n = payout_months)
  safety_inv <- chance_event(safety_risk, 1, 0, n = investment_months)
  
  #Education
  Education_investment_A <-Education_investment * (1-safety_inv)
  
  Education_investment <- c(vv(var_mean = Education_investment_A, 
                               var_CV = var_slight, 
                               n = investment_months), rep(0,payout_months))
  #Economy
  
  Economy_payout_A <- Economy_payout * (1-safety_payout)
  
  Economy_payout <- c(rep (0,investment_months), 
                      vv(var_mean = Economy_payout_A, 
                         var_CV = var_slight, 
                         n = payout_months))
  
  Economy_investment_A <- Economy_investment * (1-safety_inv)
  
  Economy_investment <- c(vv(var_mean = Economy_investment_A, 
                             var_CV = var_slight, 
                             n = investment_months), rep(0,payout_months))
  
  #Status Quo Resources
  SQ_Resources_investment <- c(vv(var_mean = SQ_Resources_investment, 
                                  var_CV = var_slight, 
                                  n = investment_months), rep(0,payout_months))
  
  SQ_Resources_payout <- c(rep (0,investment_months),
                           vv(var_mean = SQ_Resources_payout, 
                              var_CV = var_slight, 
                              n = payout_months))
  #Empowerment Resources
  Empowerment_Resources_investment <- c(vv(var_mean = 
                                             Empowerment_Resources_investment, 
                                           var_CV = var_slight, 
                                           n = investment_months), 
                                        rep(0,payout_months))
  
  Empowerment_Resources_payout <- c(rep (0,investment_months),
                                    vv(var_mean = Empowerment_Resources_payout, 
                                       var_CV = var_slight, 
                                       n = payout_months))
  #Status Quo monthly Workforce
  
  SQ_Workforce_investment_A <-  SQ_Workforce_investment * (1-safety_inv)
  
  SQ_Workforce_investment <- c( vv(var_mean = SQ_Workforce_investment_A, 
                                   var_CV = var_slight, 
                                   n = investment_months), 
                                rep(0,payout_months))
  
  SQ_Workforce_payout <- c(rep (0,investment_months),
                           vv(var_mean = SQ_Workforce_payout, 
                              var_CV = var_slight, 
                              n = payout_months))
  
  #Empowerment monthly Workforce
  
  Empowerment_Workforce_investment <- c( vv(var_mean = 
                                  Empowerment_Workforce_investment, 
                                  var_CV = var_slight, 
                                  n = investment_months), rep(0,payout_months))
  
  Empowerment_Workforce_payout <- c(rep (0,investment_months),
                                    vv(var_mean = Empowerment_Workforce_payout, 
                                       var_CV = var_slight, 
                                       n = payout_months))
  
  # Husband's investment
  
  SQ_Husband_Workforce_investment <- c( vv(var_mean = 
                                             SQ_Husband_Workforce_investment, 
                                           var_CV = var_slight, 
                                           n = investment_months),
                                        rep(0,payout_months))
  
  Husband_Empowerment_Workforce_investment <- c(rep (0,investment_months),
                                       vv(var_mean = 
                                       Husband_Empowerment_Workforce_investment, 
                                       var_CV = var_slight, 
                                       n = payout_months))
  
  
### Computing the decision and status Quo  pathways ###

  ##Status Quo pathway##
  
  PartA <- SQ_Workforce_payout + SQ_Resources_payout 
  PartB <- SQ_Resources_investment + SQ_Workforce_investment 
  + SQ_Husband_Workforce_investment
  Profit_SQ <- (PartA -PartB)
  
# It can be dangerous to use the money for herself, instead of the family.
# Women might be dependent on their husbands for health care and food. 
# This calculation shows how much money a woman would
# have for health care and food investments (= workforce investment)
  

### Estimate the NPV from the model ###
  
#Computing the Status Quo NPV (Net present value)#
  
NPV_no_empowerment_branch <- discount(Profit_SQ,
                            discount_rate = discount_rate, calculate_NPV = TRUE) 


###Decision Pathway###

##Empowerment pathway##

PartA <- Economy_payout + Empowerment_Resources_payout + 
         Empowerment_Workforce_payout
PartB <- Education_investment  
         + Economy_investment + Empowerment_Resources_investment + 
         Empowerment_Workforce_investment
         + Husband_Empowerment_Workforce_investment

# Safety risks occur for: Education and economy investments since time away
# from female connotated tasks might risk violence. 
# having her own money and not giving it to the husband or family might also
# be a risk for violence. 

# Husband's investment into food and health care (workforce investment)
# might be smaller within the empowerment pathway than status quo.


### Estimate the NPV from the model ####

#Computing the Empowerment NPV (Net present value)#

Empowerment_profit <-  (PartA - PartB)

NPV_Empowerment_profit <- discount(Empowerment_profit,
                          discount_rate = discount_rate, calculate_NPV = TRUE)
NPV_decision_profit_with_empowerment <- NPV_Empowerment_profit - 
                                        NPV_no_empowerment_branch

####Return list####

return(list(NPV_no_empowerment_branch =  NPV_no_empowerment_branch,
    NPV_Empowerment_profit = NPV_Empowerment_profit, 
    NPV_decision_profit_with_empowerment = NPV_decision_profit_with_empowerment,
    Cashflow_decision_empowerment =  Empowerment_profit
            
)) 

}

#### Run the Monte Carlo simulation using the model function ####

server <- function(input,output) {
  

  dataSource <- reactive({
    
    input_estimates <- data.frame(variable = c("Education_investment",
                             "Economy_investment", "Economy_payout",
                                     "SQ_Resources_investment", 
                                     "SQ_Resources_payout",
                                     "Empowerment_Resources_investment", 
                                     "Empowerment_Resources_payout",
                                     "SQ_Workforce_investment",
                                     "SQ_Workforce_payout",
                                     "Empowerment_Workforce_investment",
                                     "Empowerment_Workforce_payout",
                                     "SQ_Husband_Workforce_investment",
                                     "Husband_Empowerment_Workforce_investment",
                                     "var_slight", "discount_rate",
                                     "payout_months", "investment_months",
                                     "safety_risk"),
                                  lower = c(min(input$slider1),min(input$slider2),
                                            min(input$slider3),min(input$slider4),
                                            min(input$slider5),min(input$slider6),
                                            min(input$slider7),min(input$slider8),
                                            min(input$slider9),min(input$slider10),
                                            min(input$slider11),min(input$slider12),
                                            min(input$slider13),min(input$slider14),
                                            min(input$slider15),min(input$slider16),
                                            min(input$slider17),min(input$slider18)
                                            ),
                                  median = NA,
                                  upper = c(max(input$slider1),max(input$slider2),
                                            max(input$slider3),max(input$slider4),
                                            max(input$slider5),max(input$slider6),
                                            max(input$slider7),max(input$slider8),
                                            max(input$slider9),max(input$slider10),
                                            max(input$slider11),max(input$slider12),
                                            max(input$slider13),max(input$slider14),
                                            max(input$slider15),max(input$slider16),
                                            max(input$slider17),max(input$slider18)
                                            ),
                                  distribution = c("posnorm","posnorm",
                                                   "posnorm","posnorm",
                                                   "posnorm","posnorm",
                                                   "posnorm","posnorm",
                                                   "posnorm","posnorm",
                                                   "posnorm","posnorm",
                                                   "posnorm",
                                                   "const","const",
                                                   "const","const",
                                                   "const"),
                                  label = c("Education investment (Dollar/Month)",
                     "Economy investment (Dollar/Month)",
                     "Economy payout (Dollar/Month)",
                     "Status Quo Resources investment (Dollar/Month)",
                     "Status Quo Resources payout (Dollar/Month)",
                     "Empowerment Resources investment (Dollar/Month)",
                     "Empowerment payout (Dollar/Month)",
                     "Status Quo Workforce investment (Dollar/Month)",
                     "Status Quo Workforce payout (Dollar/Month)",
                     "Empowerment Workforce investment (Dollar/Month)",
                     "Empowerment Workforce payout (Dollar/Month)",
                     "Status Quo Husband's Workforce investment (Dollar/Month)",
                     "Husband's Workforce investment (Dollar/Month)",
                     "Coefficient of variation",
                     "Discout rate",
                     "Months of receiving money (Dollar/Month)",
                     "Months of paying into empowerment efforts (Dollar/Month)",
                     "Risk Safety"),
                                  Description = c("Education investment",
                                    "Economy investment",
                                    "Economy payout",
                                    "Status Quo Resources investment",
                                    "Status Quo Resources payout",
                                    "Empowerment Resources investment",
                                    "Empowerment Resources payout",
                                    "Status Quo Workforce investment",
                                    "Status Quo Workforce payout",
                                    "Empowerment Workforce investment",
                                    "Empowerment Workforce payout",
                                    "SQ Husband's Workforce investment",
                                    "Husband's Workforce investment",
                                    "Coefficient of variation",
                                    "Discout rate",
                                    "Months of receiving money",
                                    "Months of paying into empowerment efforts",
                                    "Risk Safety"))
    
    input_estimates <- input_estimates %>% 
      mutate(variable = as.character(variable),
             distribution = as.character(distribution),
             label = as.character(label),
             Description = as.character(Description))
    
    #10,1,50,30,20,30,200,50,30,50,300,
    #50,10,1,1,9,3,0.5
    
    #50,20,200,100,90,100,300,100,100,
    #100,1000,100,50,1,1,9,3,0.5
    
  })
  
  output$table1 <- renderTable({
    dataSource()
  })
  
  chile_mc_simulation <- reactive({
    
#    chile_mc_simulation <- mcSimulation(
#      estimate = as.estimate(dataSource()),
#      model_function = model_function,
#      numberOfModelRuns = 800,
#      functionSyntax = "plainNames"
      
      chile_mc_simulation <- mcSimulation(
        estimate = as.estimate(dataSource()),
        model_function = decision_function,
        numberOfModelRuns = 10000,
        functionSyntax = "plainNames"
      )
      
#decisionsupport: raus, da nur ein packet, wo mcsimulatuon drin ist
#() da reactive function!
#model runs auf 10.000 am schluss
    
  })
  
  
  output$plot1 <- renderPlot({
    
    decisionSupport::plot_distributions(mcSimulation_object = chile_mc_simulation(), 
                                        vars = c("NPV_decision_profit_with_empowerment",
                                                 "NPV_no_empowerment_branch"),
                                        method = 'smooth_simple_overlay', 
                                        colors = c("purple3", "pink2", "gray32", "rosybrown1",
                                                   "gray34", "gray35", "gray36", "gray37"),
                                        base_size = 13)
  })
  
}

