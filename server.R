#### Building the model ####

#To see the EVPI within the app, a programmer must remove the hashtags
#within the computation in server.R  and the box within the ui.R.
#Code of the EVPI box:
  #box(
  #  title ="EVPI", status = "primary", solidHeader = TRUE, collapside = TRUE,
  #  plotOutput("plot5")),
#The EVPI computation takes much time for the page to load, 
#so it is initially not included.

#### Start of the function ####

decision_function <- function(x, varnames){
  
  #Risk
  
  safety_payout <- chance_event(safety_risk, 1, 0, n = payout_months,
                                one_draw =TRUE)
  safety_inv <- chance_event(safety_risk, 1, 0, n = investment_months,
                             one_draw =TRUE)
  
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
  
  
  ### Computing the decision and status quo  pathways ###
  
  ##Status Quo pathway##
  
  #  PartA <- SQ_Workforce_payout + SQ_Resources_payout 
  #  PartB <- SQ_Resources_investment + SQ_Workforce_investment 
  #  + SQ_Husband_Workforce_investment
  #  Profit_SQ <- (PartA -PartB)
  
  PartA <- (SQ_Workforce_payout
            + SQ_Resources_payout)*safety_payout
  PartB1 <- SQ_Resources_investment 
  PartB2 <- (SQ_Workforce_investment 
             + SQ_Husband_Workforce_investment)*safety_inv
  PartB <- PartB1 + PartB2
  Profit_SQ <- (PartA -PartB)
  
  #It can be dangerous to use the money for herself, instead of the family.
  #Women might be dependent on their husbands for health care and food. 
  #This calculation shows how much money a woman would
  #have for health care and food investments (= workforce investment)
  
  
  ### Estimate the NPV from the model ###
  
  #Computing the Status Quo NPV (Net present value)#
  
  NPV_no_empowerment_branch <- discount(Profit_SQ,
                                        discount_rate = discount_rate, calculate_NPV = TRUE) 
  
  
  ###Decision Pathway###
  
  ##Empowerment pathway##
  
  PartA <- (Economy_payout
            + Empowerment_Resources_payout  
            + Empowerment_Workforce_payout
            + Empowerment_Workforce_investment)*safety_payout
  PartB1 <- Empowerment_Resources_investment  
  
  PartB2 <- (Education_investment + Economy_investment
             + Husband_Empowerment_Workforce_investment)*safety_inv
  
  PartB <- PartB1 + PartB2
  
  Empowerment_profit <-  (PartA - PartB)
  
  ### Estimate the NPV from the model ####
  
  #Computing the Empowerment NPV (Net present value)#
  
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
#Use the reactive function to have  shiny app that updates itself when the user 
#changes values. Below this part, the values need to be inserted which
#a user should be able to change. Above this function, all Decision Analysis
#code parts that should not be changed are inserted.

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
    
    # The input estimates are:
    #10,1,50,30,20,30,200,50,30,50,300,
    #50,10,1,1,9,3,0.5
    
    #50,20,200,100,90,100,300,100,100,
    #100,1000,100,50,1,1,9,3,0.5
    
  })
  # Table showing the values, updating itself when the user changes them.
  output$table1 <- renderTable({
    dataSource()
  })
  
  
  chile_mc_simulation <- reactive({
    
    
    chile_mc_simulation <- mcSimulation(
      estimate = as.estimate(dataSource()),
      model_function = decision_function,
      numberOfModelRuns = 10000,
      functionSyntax = "plainNames"
    )
    
    
  })
  
  #Net present value
  
  
  output$plot1 <- renderPlot({
    
    decisionSupport::plot_distributions(mcSimulation_object = chile_mc_simulation(), 
                                        vars = c("NPV_decision_profit_with_empowerment",
                                                 "NPV_no_empowerment_branch"),
                                        method = 'smooth_simple_overlay', 
                                        colors = c("purple3", "pink2", "gray32", "rosybrown1",
                                                   "gray34", "gray35", "gray36", "gray37"),
                                        base_size = 13)
  })
  #Boxplot 
  
  output$plot2 <- renderPlot({
    
    decisionSupport::plot_distributions(mcSimulation_object = chile_mc_simulation(), 
                                        vars = c("NPV_decision_profit_with_empowerment"
                                        ),
                                        method = 'boxplot', 
                                        base_size = 7)
  })
  #Cashflow
  
  output$plot3 <- renderPlot({
    
    Cashflow <- plot_cashflow(mcSimulation_object = chile_mc_simulation(),
                              cashflow_var_name = "Cashflow_decision_empowerment",
                              x_axis_name = "Month",
                              y_axis_name = "Cashflow in Dollar",
                              color_25_75 = "green4",
                              color_5_95 = "green1",
                              color_median = "red")
    Cashflow
    
  })
  #PLS (Partial least square regression)
  
  output$plot4 <- renderPlot({
    
    pls_result_1 <- plsr.mcSimulation(object = chile_mc_simulation(),
                                      resultName = "NPV_decision_profit_with_empowerment",
                                      ncomp = 1)
    
    plot_pls(pls_result_1, threshold = 0.8, input_table = dataSource())
    
  })
  
  #EVPI Table
  
  #  output$table2 <- renderTable({
  #  mcSimulation_table <- data.frame(chile_mc_simulation()$x,
  #                                 chile_mc_simulation()$y[1:3])
  #  mcSimulation_table
  
  
  #})
  
  #EVPI Plot
  
  #  output$plot5 <- renderPlot({
  
  #  mcSimulation_table <- data.frame(chile_mc_simulation()$x,
  #                                     chile_mc_simulation()$y[1:3])
  
  #  plot_evpi<-plot_evpi((multi_EVPI(mc = mcSimulation_table, 
  #                                   first_out_var = "NPV_Empowerment_profit")
  #  ),
  #                       decision_vars = "NPV_decision_profit_with_empowerment")
  #  plot_evpi
  
  
  
  #The evpi part of the function was:
  
  #evpi <- multi_EVPI(mc = mcSimulation_table, 
  #                   first_out_var = "NPV_Empowerment_profit")
  
  #EVPI was not by default included into the shiny app, since its calculation
  #takes very long.
  #The hashtags have to be eliminated within this source code to make it work.
  
  #  })
} 