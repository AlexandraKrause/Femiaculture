#install.packages("shiny")
#install.packages("shinydashboard")
#install.packages("readr")
#install.packages("decisionSupport")
#install.packages("DiagrammeR")
#install.packages("tidyverse")
#install.packages("ggplot2")
#install.packages("plyr")
#install.packages("dplyr")
#install.packages("rsconnect")


library(shiny)
library(shinydashboard)
library(readr)
library(decisionSupport)
library(DiagrammeR)
library(tidyverse)
library(ggplot2)
library(plyr)
library(dplyr)
library(rsconnect)

ui <- dashboardPage(skin =  "purple",
                    dashboardHeader(title = "Femiaculture",
                                    dropdownMenuOutput("messageMenu")),
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem("Decision analysis basics", tabName = "dashboard", icon = icon("brain",class =NULL, lib="font-awesome" )),
                        menuItem("The project", tabName = "The project", icon = icon("brain",class =NULL, lib="font-awesome" ),
                                 menuSubItem("Methodology", tabName = "Methodology", icon = icon("paper-plane",class =NULL, lib="font-awesome" )),
                                 menuSubItem("Map", tabName = "Map", icon = icon("pencil-alt",class =NULL, lib="font-awesome")),
                                 menuSubItem("Model", tabName = "Model", icon = icon("pencil-alt",class =NULL, lib="font-awesome")),
                                 menuSubItem("Status", tabName = "Status", icon = icon("map-marker-alt",class =NULL, lib="font-awesome" )),
                                 menuSubItem("Empowerment", tabName = "Empowerment", icon = icon("venus",class =NULL, lib="font-awesome" )),
                                 menuSubItem("Risk", tabName = "Risk", icon = icon("exclamation-triangle",class =NULL, lib="font-awesome" )),
                                 menuSubItem("Resources", tabName = "Resources", icon = icon("seedling",class =NULL, lib="font-awesome" )),
                                 menuSubItem("Calculations", tabName = "Calculations", icon = icon("bar-chart-o",class =NULL, lib="font-awesome" )),
                                 menuSubItem("Impressum", tabName = "Impressum", icon = icon("star",class =NULL, lib="font-awesome" ))
                                 # You can change the order of the 'tabItems' above here. The order within the rest of the code has no impact.
                        ))
                    ),
                    dashboardBody(
                      tags$head(
                        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
                        tags$link(rel = "stylesheet", type = "text/css", href = "custom2.css")),
                      #first tab content
                      tabItems(
                        tabItem(tabName = "dashboard",
                                fluidRow(
                                  box(
                                    title ="NPVs", status = "primary", solidHeader = TRUE, collapside = TRUE,
                                    plotOutput("plot1", width="100%")
                                  ),
                                  box(
                                    title = "Femiaculture", status = "primary",solidHeader = TRUE, collapside = TRUE,
                                    width = 6, 
                                    tags$strong("Welcome To Femiaculture by Alexandra Krause", style = "font-size:20px;"),
                                    tags$br(tags$i(p("If the app looks distorted, please try another browser. Please do not use Windows Edge."))), 
                                    p("I build this website during my master thesis focusing on factors influencing rural
                                    farm-women's empowerment. Later, I applied the Decision Analysis methodology.
                                    To understand the underlying empowerment system, I conducted
                                    a literature research, connected to researchers and visited conferences.
                                    Furthermore, I developed a model based on the literature reserach, which visualizes the system (see the  \"Map\"-section). 
                                    I conducted (see the  \"Methodology\"-section)
                                    a generalized statistical model, suggesting it as a 
                                    measurement technique of the researched factors. See",
                                    tags$a(href="https://github.com/AlexandraKrause/DA-Code_Femiaculture", "https://github.com/AlexandraKrause/DA-Code_Femiaculture"),
                                    "for further information."),
                                    p("
                                      So, by using the sliders in the upper part of this page, researchers can use the model for their own case studies.
                                      Decision Analysis also bears the possibility for actual change in the form of information for farm women.
                                      By using Shiny apps like these, online information systems can be developed also for them.
                                      Therefore, researchers need to adapt the model to local situations by using the sliders. Researchers can also decide to
                                      change the website's code to 
                                      rural women's user needs and provide the app to them. Find more information in my master thesis."),
                                    p("Please try it out yourself: You see the initial graph left on the page - if you changed the values on the slider, 
                                      you see the chart slowly changing.
                                      So, what should a farm women decide to do? 
                                      Should she empower herself or choose not to change her status quo situation?
                                      Please use the sliders below and see how the graph changes: Focus on the x-axis, which shows the monetary range
                                      farm women can expect for either option. Depending on the amount of money the farm woman could earn,
                                      she should decide on either the one or the other option. 
                                      Also, the decision is influenced by the costs of the influencing factors like education, the outcome benefits like
                                      health and food/ nutrition, and the risk of being unsafe.
                                      As you see, input estimates are needed: Well estimated numbers to insert into such models. 
                                      Since the model is run 10.000 times and 
                                      coincidences are also calculated, the visual might look a bit different each time you use the application.
                                      Beneath the sliders and the text, a table also shows the current values."),
                                    p("To find out more, please click on \"The Project\" and read the following texts on this website.
                                      Down on this page further calculations like a cashflow can be done. 
                                      Find further information about these in my master thesis."),
                                    div(
                                      # use HTML by wrapping it with this function. 
                                      # Use a bootstrap or Font Awesome icons 
                                      # Font awesome: (https://fontawesome.com/search?q=mother&s=solid%2Cbrands)
                                      # Bootstrap: (https://icons.getbootstrap.com/)
                                      # Select the svg code on the website
                                      # re-size and arrange the icon by changing the "viewBox" inputs.
                                      # It defines which area on the page the svg covers
                                      HTML('<svg xmlns="http://www.w3.org/2000/svg" viewBox="-700 -30 1900 600"><!--! Font Awesome Pro 6.1.1 by @fontawesome - https://fontawesome.com License - https://fontawesome.com/license (Commercial License) Copyright 2022 Fonticons, Inc. --><path d="M144 80C144 35.82 179.8 0 224 0C268.2 0 304 35.82 304 80C304 124.2 268.2 160 224 160C179.8 160 144 124.2 144 80zM436.8 382.8L373.5 461.1C356.9 482.7 326.7 486 306 469.5C288.4 455.4 283.3 431.3 292.5 411.7L291.7 411.6C252.8 406.1 217.4 386.5 192 356.8V320C192 302.3 177.7 288 160 288C142.3 288 128 302.3 128 320V368C128 368.8 128 369.6 128.1 370.4L229.5 421.1C253.2 432.9 262.8 461.8 250.9 485.5C239.1 509.2 210.2 518.8 186.5 506.9L27.21 427.3C26.11 426.7 25.02 426.2 23.95 425.5C19.04 422.7 14.79 419.1 11.3 414.1C6.732 409.5 3.492 403.3 1.683 396.6C-1.576 384.6-.1811 371.4 6.459 359.9C7.098 358.8 7.776 357.8 8.489 356.7L75.56 256.1C102.3 216.1 147.2 192 195.4 192H270.6C317.1 192 360.7 214.5 387.8 252.3L438.5 323.2C440.7 326.2 442.5 329.4 443.9 332.7C446.9 339.3 448.2 346.4 447.1 353.5C447.7 364.1 443.8 374.5 436.8 382.8V382.8zM276 288C251.7 288 232 307.7 232 332C232 356.3 251.7 376 276 376C300.3 376 320 356.3 320 332C320 307.7 300.3 288 276 288z"/></svg>'),
                                    )),
                                  #width="100px" height="100px" 
                                  box(
                                    title = "Controls", status = "primary", solidHeader = TRUE, collapside = TRUE,
                                    width = 12,
                                    p("Within the model, a farm woman who chooses to change her status quo first must 
                                    invest in some form of education or training. This investment is estimated here."),
                                    tags$br(sliderInput("slider1", "Education investment (Dollar/Month):", 1, 1000, c(1,3),step=1)),
                                    p("Afterward, an investment in the form of a credit,
                                    a (new) paid job, or additional income is estimated.
                                    This investment includes every monthly expanse that leads to an economic payoff in the future, 
                                    like transport or child care costs. 
                                    Education about this investment possibility took place within the first step"),
                                    tags$br(sliderInput("slider2", "Economy investment (Dollar/Month):", 1, 1000, c(3,10),step=1)),
                                    p("Here the range of the monetary gain from the prior investment is calculated."),
                                    tags$br(sliderInput("slider3", "Economy payout (Dollar/Month):", 1, 1000, c(30,90),step=1)),
                                    p("Here, the agricultural resource investment range per month is calculated
                                    for the status quo option - a life living from farm resources without
                                    empowerment intervention."),
                                    tags$br(sliderInput("slider4", "Status Quo Resources investment (Dollar/Month):", 1, 1000, c(1,10),step=1)),
                                    p("Following is the monthly payout of the resource investment a farm woman can expect while 
                                    not changing the status quo."),
                                    tags$br(sliderInput("slider5", "Status Quo Resources payout (Dollar/Month):", 1, 1000, c(1,20),step=1)),
                                    p("This could be chicken, seeds, processing material, technology, etc."),
                                    tags$br(sliderInput("slider6", "Empowerment Resources investment (Dollar/Month):", 1, 1000, c(10,20),step=1)),
                                    p("Following is the monthly payout of the resource investment a farm woman can expect while 
                                    changing her situation. This could be monetary gains from eggs, processed food and other."),
                                    tags$br(sliderInput("slider7", "Empowerment Resources payout (Dollar/Month):", 1, 1000, c(10,50),step=1)),
                                    p("Following are the monthly investments a farm woman could make on nutritious food and health care
                                    investments like contraceptives or savings for doctor's visits while in a status quo scenario."),
                                    tags$br(sliderInput("slider8", "Status Quo Workforce investment (Dollar/Month):", 1, 1000, c(1,20),step=1)),
                                    p("Investments in nutritious food and health care can lead to higher workforce ability. 
                                    A farm woman's daily work hours are calculated and put in monetary values. 
                                    Here the value ranges for the status quo pathway are calculated."),
                                    tags$br(sliderInput("slider9", "Status Quo Workforce payout (Dollar/Month):", 1, 1000, c(5,50),step=1)),
                                    p("The monthly investments a farm woman could make on nutritious food and health care
                                    investments like contraception or savings for doctor's visits while being in an empowerment scenaro."),
                                    tags$br(sliderInput("slider10", "Empowerment Workforce investment (Dollar/Month):", 1, 1000, c(5,40),step=1)),
                                    p("Investments into nutritious food and health care can lead to higher workforce ability. 
                                    A farm woman's daily work hours are calculated here and put in monetary values. 
                                    Here the value ranges for the empowerment pathway are calculated."),
                                    tags$br(sliderInput("slider11", "Empowerment Workforce payout (Dollar/Month):", 1, 1000, c(80,100),step=1)),
                                    p("A husband might share some money with this wife, the farm woman, for health care or food investments.
                                    This money is calculated here for the status quo pathway."),
                                    tags$br(sliderInput("slider12", "SQ Husband's Workforce investment (Dollar/Month):", 1, 1000, c(1,20),step=1)),
                                    p("The Husband's workforce investment for the empowerment pathway is calculated here."),
                                    tags$br(sliderInput("slider13", "Husband's Workforce investment (Dollar/Month):", 1, 1000, c(1,15),step=1)),
                                  ###
                                    tags$br(tags$strong("The following five inputs are constant. Please set the slider
                                    to one number instead of a range for the application to work properly.", style = "color:purple;")),
                                    tags$br(tags$strong("Please wait a second for 
                                    the error warning to disappear after changing the following inputs.", style = "color:purple;")),
                                  tags$br(),
                                  tags$br(p("The next value is used for time series that include a variation.")),
                                    tags$br(sliderInput("slider14", "Coefficient of variation:", 0, 1, c(1,1),step=0.1)),
                                    p("The discount rate can indicate the decision maker's willingness
                                    to invest in long-term outcomes."),
                                    tags$br(sliderInput("slider15", "Discout rate:", 1, 5, c(1,1),step=0.1)),
                                    p("The following two parameters calculate how long a farm woman has to invest
                                    until she can receive the payback."),
                                    tags$br(sliderInput("slider16", "Months of receiving money:", 1, 36, c(9,9),step=1)),
                                    tags$br(sliderInput("slider17", "Months of paying into empowerment efforts:", 1, 36, c(3,3),step=1)),
                                    p("Here the percentage risk of unsafe conditions ending the empowerment possibility 
                                    and stopping the intervention is calculated."),
                                    tags$br(sliderInput("slider18", "Risk Safety [%]:", 0.01, 1, c(0.06,0.06),step=0.01)),
                                    p("Here the percentage risk of unsafe conditions within the status quo pathway,
                                    ending the pathway is calculated. A woman then looses complete control over farm income
                                    and does not receive a workforce investment from her husband."),
                                    tags$br(sliderInput("slider19", "Status Quo Risk Safety [%]:", 0.01, 1, c(0.04,0.04),step=0.01))
                                  )),
                                
                                fluidRow(
                                  box(
                                    title ="Self Adjusting Table", status = "primary", background = "navy", solidHeader = TRUE, collapside = TRUE, width = 12,
                                    tableOutput("table1"))),
                               #Checking the values
                               # fluidRow(
                               # box(
                               #   title ="Min,1st Quan.,Median, Mean, 3rd Qu., Max of status quo", status = "primary", background = "navy", solidHeader = TRUE, collapside = TRUE, width = 12,
                               #   tableOutput("table2")),
                               # box(
                               #   title ="Min,1st Qu.,Median, Mean, 3rd Qu., Max of decision", status = "primary", background = "navy", solidHeader = TRUE, collapside = TRUE, width = 12,
                               #   tableOutput("table3"))),
                                fluidRow(
                                  box(
                                    title ="Boxplot", status = "primary", solidHeader = TRUE, collapside = TRUE,
                                    plotOutput("plot2", width="100%")), 
                                  box(
                                    title ="Cashflow", status = "primary", solidHeader = TRUE, collapside = TRUE,
                                    plotOutput("plot3", width="100%")),
                                  box(
                                    title ="PLS", status = "primary", solidHeader = TRUE, collapside = TRUE,
                                    plotOutput("plot4", width="90%")),
                                  #box(
                                  #  title ="EVPI", status = "primary", solidHeader = TRUE, collapside = TRUE,
                                  #  plotOutput("plot5")),
                                  #box(
                                  #  title ="EVPI Table", status = "primary", background = "navy", solidHeader = TRUE, collapside = TRUE, width = 12,
                                  #  tableOutput("table2"))
                                  #EVPI was not by default included into the shiny app, since its calculation
                                  #takes very long.
                                  #The hashtags have to be eliminated within this source code to make it work.
                                ),
                                tags$a(href="http://inresgb-lehre.iaas.uni-bonn.de/impressum/", "Impressum", style = "font-size:25px;",style = "color: black;")
                        ),
                        # Second tab content
                        tabItem(tabName = "The Project",
                                h2("Widgets tab content"),
                                menuItem("Source code", 
                                         href = "https://github.com/rstudio/shinydashboard/")
                        ),
                        # Third tab content
                        tabItem(tabName = "Methodology",
                                h3("Methodology"),
                                fluidRow(
                                  box(
                                    title = "Decision Analysis", status = "primary",solidHeader = TRUE, collapside = TRUE,
                                    width = 12,
                                    p("Decision analysis implies actual help for decision-makers and is a powerful tool for system change. 
                                    It provides a quantitative assessment of interviews and workshops leading to the conduction of a stochastic model.
                                    The result is a holistic model of a complex system that also incorporates a decision's riskiness (Luedeling & Shepherd, 2016).
                                    Working inter- & trans-disciplinary with, for example, scientists and farmers also give new possibilities for information gathering. 
                                    If repeated with the same group, it could also visualize change over the years after the last decision analysis was conducted."), 
                                    p("For the decision analysis process, many values, so-called \"input estimates\" are needed.
                                    Every calculated step can include several such estimates like 
                                    costs for hiring labour, market prices, or loans. 
                                    Also, the estimation of the decision's riskiness, which is women's unsafety in my model, 
                                    should be done by considering several possible risk factors like child marriage, 
                                    husband's alcohol consumption or upbringing."),
                                    p("After adopting the model to a local situation, the application can be tailored to end-users.
                                    Think of sliders for these different input estimates that a female farmer could use to insert the values 
                                    she knows from her own life, adapting the model to her own individual situation.
                                    But not all values can be taken from her experience - he should find some estimates
                                    already on the website, for example derived from a case study of a local development.
                                    And for this, experts are needed: Other farmers, scientists, consultants, and others, who would like to participate in
                                    a workshop and fill out questionnaires to give these estimates. 
                                    Together with values from literature research, very good estimates 
                                    can be found. And luckily, the results are readily available for all decision-makers, workshop participants,
                                    and stakeholders as websites like this or mobile apps make them easily accessible.
                                    Please consider taking part in the making of a model and visit the other tabs for more information.
                                    Further information regarding the methodology can be found within my master thesis."),
                                    p("Luedeling, E., & Shepherd, K. (2016). Decision-Focused Agricultural Research. Solutions, 7(5), 46-54.")
                                  )),
                                tags$a(href="http://inresgb-lehre.iaas.uni-bonn.de/impressum/", "Impressum", style = "font-size:15px;",style = "color: black;")
                        ),
                        
                        # forth tab content
                        tabItem(tabName = "Model",
                                h3("Model"),
                                fluidRow(box(
                                  title = "", status = "primary",solidHeader = TRUE, collapside = TRUE,
                                  width = 12, align ="center",
                                  column(10, offset = 1,
                                         tags$strong("The Impact Pathway For Empowerment Based On An Economic Assessment."),
                                         tags$p(" To  conduct a model an impact pathway like the following is needed, comparing two options with each other."),
                                         img(src = "impact_pathway1.png",
                                             width="70%"
                                             #"1000"
                                         )
                                         # insert image :) src stands for source, url is ok. 
                                         #menuItem("Loopy1", icon = icon("pencil-alt"),
                                         #         href = "https://bit.ly/34gcgaQ")
                                  )),
                                  
                                  box(
                                    title = "", status = "primary",solidHeader = TRUE, collapside = TRUE,
                                    width = 12,
                                    tags$code("General Overview"),
                                    p("During my thesis, I researched factors influencing rural farm women's empowerment in agricultural development.
                                  I did a literature research and a decision analysis to assess the unequal
                                  resource distribution between male and female farmers and to evaluate how gender can be captured in modeling efforts.
                                  Women contribute not only to farm work, they are also often the primary providers of care for children and elderly family members,
                                  and are also often solely responsible for their family's nutrition and health (Meinzen-Dick et al., 2011). 
                                  Unfortunately, women often lack access to resources and knowledge (Farnworth et al., 2016).
                                  A shift in the distribution of resources between male and female farmers, such as women's access to education, 
                                  has the potential to positively impact this situation (Hallman, 2000; Quisumbing & Maluccio, 2003).
                                  It could site-specifically increase general decision-making power, and with it, productivity, 
                                  food, and nutrition security, as well as educational outcomes for women and children (Farooq et al., 2019; Hallman, 2000; Quisumbing & Maluccio, 2003). 
                                  Often these benefits are connected to other positive effects."),
                                    tags$code("The Model"), 
                                    p("What you see is the small impact pathway, which is also the base for the code.
                                  It shows empowerment from an economic perspective, 
                                  quantifying each part in monetary values. It is a universal model that needs to be adapted to local needs,
                                  actual problems, and factors. 
                                  "),
                                    tags$p("The scenario you see is the following: A farm woman works on the farm and the household.
                                    She might be restricted by factors that limit her empowerment and disproportionally affect women in her region
                                    (e.g., inner-household limitations, limited market access, limited hired labour availability, limited job opportunities). 
                                    She could decide on visiting training/education options to find out more about economic possibilities,
                                    use these options to gather money, buy more agricultural resources 
                                    like chickens and sell products like eggs for investments like health care and nutritious food. 
                                    The \"Resource\" part of the model stands for agricultural resource allocation and value
                                    generation through selling agricultural products.
                                    Instead of selling chickens like in the example above, she might process foods and sell these. 
                                    She might also find a payed job where she does not need to buy and sell agricultural resources
                                    (e.g., an off-farm job, being employed) and can invest in other goods directly. 
                                    For this, the underlying R code needs to undergo minor changes by the scientist.
                                    The \"Workforce\" model part
                                    means better workability through investing in health, namely health care
                                    and enough nutritious food. 
                                    Also, her husband might share responsibility for these investments,
                                    resulting in the input variable \"Husband's Workforce investment\".
                                    This \"Workforce\" part
                                    can also be left out by slightly adapting the R code, so the pathway ends with a farm woman's monetary 
                                    benefit from resource adaptation or direct economic surpluses (e.g., due to credit or job options).
                                    Scientists can run the model for several different options using different input estimates. 
                                    Training programs, including women's groups, focusing on women's market access,
                                    or emphasizing discussing gender relations with males
                                    could have diverse impacts on the empowerment scenarios. Heterogenous groups of females
                                    including minorities like migrants or disabled women could be addressed by tailoring the 
                                    decision to them. Scientists can feed the model with all kinds of input estimates. 
                                    For more severe changes, the underlying code can be easily adjusted to several research interests.
                                    Notice that not only the input parameters but also the code might need to be adapted to local
                                    scenarios. Participatory workshops will clarify if an adaptation is required.
                                    Many examples for local cases are shown within the extended Decision Analysis 
                                    impact pathway you can find through the \"Map\" tab on the left.
                                    The different parts of ecological and economic resource allocation visible through the \"Resource\" tab can be 
                                    useful within the Resources part of the Impact pathway. For further information, please read my master thesis."),
                                    
                                    tags$br(tags$br(p("Farnworth, C. R., Baudron, F., Andersson, J. A., Misiko, M., Badstue, L., & Stirling, C. M. (2016).
                                    Gender and conservation agriculture in East and Southern Africa: towards a research agenda. 
                                    International Journal of Agricultural Sustainability, 14(2), 142-165. https://doi.org/10.1080/14735903.2015.1065602."),
                                                    p("Farooq, M. U., Shah, M. A. R., & Yaseen, M. R. (2019). Mother Schooling and Malnutrition among Children of Rural-Urban Pakistan.
                                    Epidemiology Biostatistics and Public Health, 16(1), 1-10. https://doi.org/10.2427/12978."),
                                                    p("Hallman, K. K. (2000). Mother-father resource control, marriage payments, and girl-boy health in rural Bangladesh (FCND Discussion paper No. 93). 
                                    Washington, DC. International Food Policy Research Institute. https://ageconsearch.umn.edu/record/16422/ https://doi.org/10.22004/ag.econ.16422."),
                                                    p("Meinzen-Dick, R., Quisumbing, A., Behrman, J., Biermayr-Jenzano, P., Wilde, V., Noordeloos, M., Ragasa, C., & Beintema, N. (2011).
                                    Engendering agricultural research, development, and extension. Washington, DC. International Food Policy Research Institute (IFPRI)."),
                                                    p("Quisumbing, A. R., & Maluccio, J. A. (2003). Resources at Marriage and Intrahousehold Allocation: 
                                    Evidence from Bangladesh, Ethiopia, Indonesia, and South Africa Oxford Bulletin of Economics and Statistics,
                                    65(3), 283-327. https://doi.org/10.1111/1468-0084.t01-1-00052.")))
                                    
                                  )),
                                tags$a(href="http://inresgb-lehre.iaas.uni-bonn.de/impressum/", "Impressum", style = "font-size:15px;",style = "color: black;")
                        ),
                        #fifth tbl content
                        tabItem(tabName = "Status",
                                h3("Status"),
                                fluidRow(box(
                                  title = "", status = "primary",solidHeader = TRUE, collapside = TRUE,
                                  width = 12,align ="center",
                                  column(10, offset = 1,
                                         img(src = "impact_pathway-status-quo.png",
                                             width="70%"
                                             #1000"
                                         ))),
                                  box(
                                    title = "Model in Detail: Sticking To Status Quo", status = "primary",solidHeader = TRUE, collapside = TRUE,
                                    width = 12,
                                    #tags$strong("Model in Detail: Sticking To Status Quo"),
                                    p("After coding the model, I further developed this more
                                              detailed figure. This pathway is divided into investments
                                              (green) and payoffs (red). The option of sticking to the status
                                              quo leads down a path of losses. Empowerment can be compared to
                                              a ladder: Depending on the situation the woman is in, the effects
                                              of the lack of Empowerment can be worse or less. But the general
                                              disadvantages stay the same. Women are often not paid for their
                                              farm work as well as their domestic work. Even if they manage 
                                              their own land, they still have to carry the burden of domestic
                                              work and are often expected to neglect their own economic farm 
                                              work for their husband's farm or household needs. In other cases,
                                              females have to give all or parts of their income, derived from
                                              their own agricultural resources, to their husbands. Women might
                                              be allowed to gather a little income by selling small amounts
                                              of farm goods. On the one hand, the economic value might be minimal,
                                              and on the other, women might not be able to spend it on their own
                                              needs but only on their families. Find further information about
                                              these in my master thesis."))),
                                tags$a(href="http://inresgb-lehre.iaas.uni-bonn.de/impressum/", "Impressum", style = "font-size:15px;",style = "color: black;")
                        ),
                        
                        #sixt tbl content
                        tabItem(tabName = "Empowerment",
                                h3("Empowerment"),
                                fluidRow(box(
                                  title = "", status = "primary",solidHeader = TRUE, collapside = TRUE,
                                  width = 12, align ="center",
                                  column(10, offset = 1,
                                         img(src = "impact pathway-emp-pathway.png",
                                             width="70%"
                                         ))),
                                  box(
                                    title = "Model in Detail: Changing Status Quo", status = "primary",solidHeader = TRUE, collapside = TRUE,
                                    width = 12,
                                    #tags$strong("Model in Detail: Changing Status Quo"),
                                    p("Like a ladder, the situation might only be changed stepwise. Therefore,
                                    the Empowerment pathway might contain more or less economically beneficial options 
                                    depending on what is possible for the woman in her situation.
                                    The \"Economy\" part of the model could include economic
                                    gains like finding a paid job or employment off-farm. For a woman, asking the husband
                                    to pay her for her work is also an option. 
                                    Also, see our", tags$a(href="https://rpubs.com/Gendergroup/808624", "Gendergroup pension project"), 
                                    ",where we observed several pathways for german in-married farm wives to achieve money for several pension options.
                                    It shows vividly
                                    that depending on the local context and the decision-maker, different options should be modeled, depending on inner-household
                                    and external social pressure. Find further information about the empowerment decision option in my master thesis."))),
                                tags$a(href="http://inresgb-lehre.iaas.uni-bonn.de/impressum/", "Impressum", style = "font-size:15px;",style = "color: black;")
                        ),
                        #seventh tab content
                        tabItem(tabName = "Risk",
                                h3("Risk"),
                                fluidRow(box(
                                  title = "", status = "primary",solidHeader = TRUE, collapside = TRUE,
                                  width = 12, align ="center",
                                  column(10, offset = 1,
                                         img(src = "riskmap.png",
                                             width="30%"
                                             ))),
                                  box(
                                    title = "The Social Environment", status = "primary",solidHeader = TRUE, collapside = TRUE,
                                    width = 12,
                                    #tags$code("The Social Environment"),
                                    p("A complex social environment in which complex behavior towards women is accepted or praised and victims are blamed,
                                    or strong backlash exists, can also strengthen problematic inner-household dynamics and masculinities. These can lead as well
                                    as the challenging behavior itself to safety issues. Safety issues have a negative effect on mental and physical health.
                                    The decision's riskiness is therefore called Safety risk and affects both status quo and empowerment pathways differently. 
                                      Find further information about these in my master thesis."))),
                                tags$a(href="http://inresgb-lehre.iaas.uni-bonn.de/impressum/", "Impressum", style = "font-size:15px;",style = "color: black;")
                        ),
                        # eight tab content
                        tabItem(tabName = "Map",
                                h3("Map"),
                                fluidRow(box(
                                  title = "", status = "primary",solidHeader = TRUE, collapside = TRUE,
                                  width = 12, 
                                  align ="center",style='width:1600px;overflow-x: scroll;height:750px;overflow-y: scroll;',
                                  column(12, offset = 1, 
                                         tags$strong("It all comes down to the Decision Analysis Impact pathway",style = "font-size:20px;"),
                                         tags$p("which should be adopted with workshop participants to
                                                tailor it to the local situation."),
                                         tags$p("The pathway is the result of literature analysis.
                                                 Minuses and pluses show a positive or negative impact of a factor."),
                                                 tags$p("The numbers stand for sources. 
                                                 Find the excel literature sources table that connects the source numbers to further source information here:", 
                                                (tags$a(href="https://github.com/AlexandraKrause/Thesis/blob/main/methods-excel.xlsx", "https://github.com/AlexandraKrause/Thesis/blob/main/methods-excel.xlsx")),
                                                tags$p("And find statistics regarding the literature review here:",
                                                       tags$a(href="https://github.com/AlexandraKrause/Thesis#readme", "https://github.com/AlexandraKrause/Thesis#readme"))),
                                         
                                         tags$hr(),
                                         tags$br(img(src = "Extended-Decision-Analysis-Impact-Pathway-06.2022.drawio.png", align ="center"))
                                  ))),
                                tags$a(href="http://inresgb-lehre.iaas.uni-bonn.de/impressum/", "Impressum", style = "font-size:15px;",style = "color: black;")
                        ),
                        
                        
                        tabItem(tabName = "Resources",
                                h3("Resources"),
                                fluidRow(box(
                                  title = "", status = "primary",solidHeader = TRUE, collapside = TRUE,
                                  width = 12, align ="center",
                                  column(10, offset = 1,
                                         img(src = "Empowernment_mindmap-resources.png",
                                             width="60%"
                                               #"1000" 
                                             #height="516"
                                         ))),
                                  box(
                                    title = "Resources To Input Into The Model", status = "primary",solidHeader = TRUE, collapside = TRUE,
                                    width = 12,
                                    #tags$code("Resources To Input Into The Model"),
                                    p("This mind map shows, which resources could be usefull for the \"Resources\" part of the model depending on the research
                                       goal and the local condition. Information technologies like mobile phones can also be part of \"Education/Training\" calculations."
                                    ))),
                                tags$a(href="http://inresgb-lehre.iaas.uni-bonn.de/impressum/", "Impressum", style = "font-size:15px;",style = "color: black;")
                        ),
                        tabItem(tabName = "Calculations",
                                h3("Calculations"),
                                fluidRow(box(
                                  title = "", status = "primary",solidHeader = TRUE, collapside = TRUE,
                                  width = 12, align ="left",
                                  column(10, offset = 1,
                                 tags$figure(
                                   class = "centerFigure",
                                   tags$img(src = "PLS.png", align ="left",
                                             width="40%")),
                                 tags$figure(
                                   class = "centerFigure",
                                   tags$img(src = "Cashflow.png", align ="right",
                                            width="40%"
                                   )))),
                                  box(
                                    title = "Calculations", status = "primary",solidHeader = TRUE, collapside = TRUE,
                                    width = 12,
                                    #tags$code("Calculations"),
                                    p("The app provides avrious calculatations, which I will explain in detail. 
                                      The app gerenates a monte carlo simulation, a random sampling method, with 10000  model runs.
                                      Next to the Net present Value, the NPV, reserachers can generate a cashflow,
                                      run a Value of information analysis (VIA) or (VoI).
                                      The cashflow plot shows the time structure of investments and payouts,
                                      visualizing when arural farm woman can expect monetary outcomes.
                                      The VoI includes every variable in the model.
                                      Such value of Information analysis could also show which values a researcher should further
                                      research in detail (Hubbard, 2014; Luedeling & Shepherd, 2016; Whitney et al., 2018).
                                      Then, the output showes the information values for all these variables (Hubbard, 2014).
                                      I set the threshold to 0.8 to differentiate between important and less important values.
                                      This threshold can be adapted within the code structure.
                                      The Variable Importance gives additional information on a variable to receive more certainty
                                      of the decision, which is most profitable. The plot shows the sensitivity of outcomes to variables.
                                      For those variables with a high information value, further measurements could take place to lower uncertainties.
                                      Thresholds show the point at which decision differences might occur (Hubbard, 2014)."
                                    ),
                                    p("Find the code here:", tags$a(href="https://github.com/AlexandraKrause/DA-Code_Femiaculture", "https://github.com/AlexandraKrause/DA-Code_Femiaculture", style ="color:purple;")),
                                    tags$br(p("Hubbard, D. W. (2014). How To Measure Anything: Finding the Value of Intangibles in Business: Bd. Second Edition (2. Aufl.). John Wiley & Sons."),
                                    p("Luedeling, E., & Shepherd, K. (2016). Decision-Focused Agricultural Research. Solutions, 7(5), 46–54. https://www.thesolutionsjournal.com/article/decision-focused-agricultural-research/"),
                                    p("Whitney, C., Luedeling, E., & Shepherd, K. (2018). Decision Analysis Methods Guide; Agricultural Policy for Nutrition. World Agroforestry (ICRAF), Working Paper series(275), 40.")),
                                    tags$a(href="http://inresgb-lehre.iaas.uni-bonn.de/impressum/", "Impressum", style = "font-size:15px;",style = "color: black;")
                        ))),
                        # size 700 and 451
                        #ninth tab content
                        tabItem(tabName = "Impressum",
                                h3("Impressum"),
                                fluidRow(box(
                                  title = "", status = "primary",solidHeader = TRUE, collapside = TRUE,
                                  width = 12,
                                  column(10, offset = 1,
                                         tags$br(tags$strong("Impressum")),
                                         icon = icon("star",class =NULL, lib="font-awesome"),
                                         p("The Institute of Plant Sciences and Resource Conservation (INRES) of the University of 
                                         Bonn is represented by the Executive Director Prof. Dr. Claudia Knief"),
                                         tags$strong("Secretariat:"),
                                         p("Karlrobert-Kreiten-Strasse 13"),
                                         p("D-53115 Bonn"),
                                         p("Phone.: ++49 (0)228 732851"),
                                         p("Fax: ++49 (0)228 732489"),
                                         p("Email: inres@uni-bonn.de"),
                                         p("The Horticultural Sciences Department of the University of Bonn is represented by Prof. Dr. Eike Luedeling."),
                                         tags$div(tags$a(href="http://inresgb-lehre.iaas.uni-bonn.de/impressum/", "http://inresgb-lehre.iaas.uni-bonn.de/impressum/")),
                                         tags$br(tags$strong("Further information")),
                                         p("Feel free to visit me at github.com/AlexandraKrause, Find me on twitter: @Al__Krause
                                         or contact me via s7alkrau@uni-bonn.de"),
                                         tags$a(href="https://github.com/AlexandraKrause", "click for github",style = "font-size:20px;"),
                                         tags$br(img(src = "uni_logo.png",width="60%"))))
                                )))
                      
                      
                    ))



