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
                                    farm-women's empowerment.  
                                    To understand the underlying empowerment system, I conducted
                                    a literature research, asked experts and visited conferences.
                                    Furthermore, I developed a model based on the literature reserach, which visualizes the system (see the  \"Map\"-section). 
                                    Later, I applied the Decision Analysis methodology (see the  \"Methodology\"-section) to
                                    conduct a generalized statistical model, suggesting it as a 
                                    measurement technique of the researched factors. See",
                                    tags$a(href="https://github.com/AlexandraKrause/DA-Code_Femiaculture", "https://github.com/AlexandraKrause/DA-Code_Femiaculture"),
                                    "for further information."),
                                    p("
                                      So, by using the sliders in the upper part of this page, researchers can use the model for their own case studies.
                                      Decision Analysis also bears the possibility for actual change in the form of information for farm women.
                                      By using Shiny apps like these, online information systems can be developed also for them.
                                      Therefore, researchers need to adapt the model to local situations by using the sliders
                                      and sometimes change the website's code to 
                                      rural women's user needs. Find more information in my master thesis."),
                                    p("Please try it out yourself: You see the initial graph left on the page - if you changed the values on the slider, 
                                      you see the chart changing (if not, please reload the page).
                                      So, what should a farm women decide to do? 
                                      Should she empower herself or choose not to change her status quo situation?
                                      Please use the slider and see how the graph changes: Focus on the x-axis, which shows the monetary range
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
                                    tags$br(sliderInput("slider1", "Education investment (Dollar/Month):", 1, 1000, c(10,50),step=1)),
                                    p("Afterward, an investment in the form of a credit,
                                      a (new) paid job, or additional income is estimated.
                                      This investment includes every monthly expanse that leads to an economic payoff in the future, 
                                      like transport or child care costs. 
                                      Education about this investment possibility took place within the first step"),
                                    tags$br(sliderInput("slider2", "Economy investment (Dollar/Month):", 1, 1000, c(1,20),step=1)),
                                    p("Here the range of the monetary gain from the prior investment is calculated."),
                                    tags$br(sliderInput("slider3", "Economy payout (Dollar/Month):", 1, 1000, c(50,200),step=1)),
                                    p("Here, the agricultural resource investment range per month is calculated
                                      for the status quo option - a life living from farm resources without
                                      empowerment intervention."),
                                    tags$br(sliderInput("slider4", "Status Quo Resources investment (Dollar/Month):", 1, 1000, c(30,100),step=1)),
                                    p("Following is the monthly payout of the resource investment a farm woman can expect while 
                                      not changing the status quo."),
                                    tags$br(sliderInput("slider5", "Status Quo Resources payout (Dollar/Month):", 1, 1000, c(20,90),step=1)),
                                    p("This could be chicken, seeds, processing material, technology, etc."),
                                    tags$br(sliderInput("slider6", "Empowerment Resources investment (Dollar/Month):", 1, 1000, c(30,100),step=1)),
                                    p("Following is the monthly payout of the resource investment a farm woman can expect while 
                                      changing her situation. This could be monetary gains from eggs, processed food and other."),
                                    tags$br(sliderInput("slider7", "Empowerment Resources payout (Dollar/Month):", 1, 1000, c(200,300),step=1)),
                                    p("Following are the monthly investments a farm woman could make on nutritious food and health care
                                    investments like contraceptives or savings for doctor's visits while in a status quo scenario."),
                                    tags$br(sliderInput("slider8", "Status Quo Workforce investment (Dollar/Month):", 1, 1000, c(50,100),step=1)),
                                    p("Investments in nutritious food and health care can lead to higher workforce ability. 
                                      A farm woman's daily work hours are calculated and put in monetary values. 
                                      Here the value ranges for the status quo pathway are calculated."),
                                    tags$br(sliderInput("slider9", "Status Quo Workforce payout (Dollar/Month):", 1, 1000, c(30,100),step=1)),
                                    p("The monthly investments a farm woman could make on nutritious food and health care
                                    investments like contraception or savings for doctor's visits while being in an empowerment scenaro."),
                                    tags$br(sliderInput("slider10", "Empowerment Workforce investment (Dollar/Month):", 1, 1000, c(50,100),step=1)),
                                    p("Investments into nutritious food and health care can lead to higher workforce ability. 
                                      A farm woman's daily work hours are calculated here and put in monetary values. 
                                      Here the value ranges for the empowerment pathway are calculated."),
                                    tags$br(sliderInput("slider11", "Empowerment Workforce payout (Dollar/Month):", 1, 1000, c(300,1000),step=1)),
                                    p("A husband might share some money with this wife, the farm woman, for health care or food investments.
                                    This money is calculated here for the status quo pathway."),
                                    tags$br(sliderInput("slider12", "SQ Husband's Workforce investment (Dollar/Month):", 1, 1000, c(50,100),step=1)),
                                    p("The Husband's workforce investment for the empowerment pathway is calculated here."),
                                    tags$br(sliderInput("slider13", "Husband's Workforce investment (Dollar/Month):", 1, 1000, c(10,50),step=1)),
                                  ###
                                    tags$br(tags$strong("The following five inputs are constant. Please set the slider
                                    to one number instead of a range for the application to work properly.", style = "color:purple;")),
                                    tags$br(tags$strong("Please wait a second for 
                                    the error warning to disappear after changing the following inputs.", style = "color:purple;")),
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
                                    tags$br(sliderInput("slider18", "Risk Safety [%]:", 0.1, 1, c(0.5,0.5),step=0.1))
                                  )),
                                
                                fluidRow(
                                  box(
                                    title ="Self Adjusting Table", status = "primary", background = "navy", solidHeader = TRUE, collapside = TRUE, width = 12,
                                    tableOutput("table1"))),
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
                                             width="90%"
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
                                  Often these benefits are connected to other positive effects. For example, increased nutrition security can reduce difficult births (Hallman, 2000)."),
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
                                    like chickens and sell them for investments like health care and food/ nutrition. 
                                    The \"Resource\" part of the model stands for agricultural resource allocation and value
                                    generation through selling agricultural products.
                                    Instead of selling chickens like in the example above, she might process foods and sell these. 
                                    She might also find a payed job where she does not need to buy and sell agricultural resources
                                    (e.g., an off-farm job, being employed) and can invest in other goods directly. 
                                    For this, the underlying R code needs to undergo minor changes by the scientist.
                                    Different researched outputs like investments in children's education or clothes
                                    could also be included by adapting the actual model.
                                    The \"Workforce\" model part
                                    means extended working hours through investing in health, namely health care
                                    and enough nutritious food. This can include different kinds of farm or domestic work.
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
                                    useful within the Resources part of the Impact pathway."),
                                    
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
                                             width="90%"
                                             #1000"
                                         ))),
                                  box(
                                    title = "", status = "primary",solidHeader = TRUE, collapside = TRUE,
                                    width = 12,
                                    tags$code("Sticking To Status Quo"),
                                    tags$br(p("The Option of sticking to the status quo leads down a path of losses. Empowerment 
                                    can be compared to a ladder: Depending on the situation the woman is in, the effects of the lack of Empowerment
                                    can be worse or less. But the general disadvantages stay the same. Women are often not paid for
                                    their farm work as well as their domestic work. Even if they manage their own land, they still have to carry the burden
                                    of domestic work and are often expected to neglect their own economic farm work for their husband's farm or household needs.
                                    In other cases, females have to give all or parts of their income, derived from their own agricultural resources, to their husbands.
                                    Women might be allowed to gather few incomes by selling small amounts of farm goods. 
                                    But on the one hand, the economic value might be minimal
                                    and on the other women might not be able to spend it on their own needs, but only on their families.
                                    So, the first part,\"Resources\", is about gaining agricultural resources and benefitting from them monetarily.
                                    But it can also include other resources that are investigated within the local project (see tthe Section of changing status quo
                                    for further information."),
                                            tags$strong("Food And Health Care"),
                                            p("Also, in various contexts, men who earn the family's money share less food with their wives, 
                                    especially when having one or more other wives. This can also be true for the children. It means that
                                    there is less money for these investments than there is within the Empowerment pathway."),
                                            p("Another problem is when men who are already restricting their wives from earning money 
                                    are not paying for their wives' health care.
                                    So, these women have less or no money for these investments. In the end, the outcome first means less money for women's
                                    quite basic needs like food and health care. 
                                    ")),
                                    (tags$strong("Missing Workforce - The Extra, Possible Variable")),
                                    p(" As visible in the graph, the main obstacles of being not empowered are life-threatening. 
                                    But these obstacles are also very much dependent on the local context and the chosen local scenario. The level
                                    of empowerment can be very different, and so is the position of females in families, their decision-making power,
                                    and their right to physical and psychological integrity. Not being empowered in the worst cases means the lack of important rights,
                                    and cuts of these rights can be seen in various local contexts and ladders of empowerment. Unfortunately, if the woman herself 
                                    has no money for health care and her husband is not paying for it, her health situation is in danger. 
                                    This obviously also depends on local laws and health care provisions.
                                    Her husband can also deny access to nutritious food, e.g., by not allowing his wife to grow this food in his fields since
                                    it lowers his economic success. He could also spend money on other assets meant for himself instead of his family."),
                                    
                                    p(" \"Workforce\" is calculated by investments into health care and nutritious food, possibly resulting
                                    in more or less monthly work hours doing farm and domestic tasks. Several studies show that work ability
                                    decreases for people suffreing from illnesses.
                                    A women's life expectancy is also dependent on these investments 
                                    and could be calculated in monthly workforce hours to compare an empowered situation with health
                                    care and food supply against a situation without these assets.
                                    Health risks can also include children when these risks are, for example, connected with missing obstetrics. 
                                    Depending on local and individual cases, a husband might also be willing to spend a certain amount of money
                                    directly on these investments.
                                    This possibility is calculated as \"Husband's Workforce investment\"."),
                                    p("Within this model part only resource allocation
                                    , selling on farm and workforce investments and payouts are included. If a farm woman has other earnings, e.g.
                                    from a job away from farm, need to be compared to the decision option to change the status quo.")
                                  )),
                                tags$a(href="http://inresgb-lehre.iaas.uni-bonn.de/impressum/", "Impressum", style = "font-size:15px;",style = "color: black;")
                        ),
                        #oder 700 und 320
                        
                        #sixt tbl content
                        tabItem(tabName = "Empowerment",
                                h3("Empowerment"),
                                fluidRow(box(
                                  title = "", status = "primary",solidHeader = TRUE, collapside = TRUE,
                                  width = 12, align ="center",
                                  column(10, offset = 1,
                                         img(src = "impact pathway-emp-pathway.png",
                                             width="90%"
                                               #"1000"
                                         ))),
                                  box(
                                    title = "", status = "primary",solidHeader = TRUE, collapside = TRUE,
                                    width = 12,
                                    tags$code("Changing Status Quo"),
                                    p("The Empowerment pathway shows the costs that need to be paid for education, 
                                    which leads to better economic standing. This might, for example, exist because
                                    the decision-maker decides to be employed and be paid for her work or to use a credit. 
                                    Then resources can be acquired. This includes more money for food and health care that 
                                    needs to be calculated if these parts are also incorporated into the option of sticking 
                                    to the status quo. Like a ladder, the situation might only be changed stepwise. Therefore,
                                    the Empowerment pathway might contain several more or less economically beneficial options 
                                    depending on what is possible for the woman in her situation. Depending on the actual decision,
                                    some steps might be left out or varied."),
                                    tags$code("The First Step Is \"Education\", Which Is A Cost Factor."),
                                    p("Information about negotiations might require investments in education. In these cases, this step is essential.
                                    An example for this first step would be: A woman needs to change her time use as an initial step to get further information on how to change her status quo.
                                    Education means Information gathering. An essential step to empowerment is the awareness of the current social situation of females, especially within the community,
                                    and possibilities to better the case. Understanding what empowerment is or could be is necessary for achieving a more empowered situation for oneself. Then other themes
                                    like nutrition, health, climate change, and agricultural practices can be understood from a female perspective. The amount of money paid for a seminar itself, transportation,
                                    internet, electricity, or other factors is calculated here. A significant cost factor for all parts of the model is time: To number the effort, the amount of money she would need
                                    to pay for someone else doing her work (e.g., Child care, fieldwork) could be calculated. Many women in agriculture are not paid for their work and are expected to do long work hours
                                    when they are sick. It is hard to find someone who takes their place. This model assumes that there is a monetary value to this. Even if the work could not be done by someone else, 
                                    but the woman later will have to work more, this number replicates the cost for a time-use change. A beneficial factor might be a social environment beneficial for the female farmer. 
                                    Other women from women's groups with who she could share transportation would be an excellent example in this case. But for all steps of the model,
                                    help from a positive environment could lower the cost for each step."),
                                    tags$code("The Next Step Is \"Economy\":"),
                                    p("The \"Economy\" part of the model also means costs like time use or child or elderly care payments.
                                    But it also includes economic gains like finding a paid job or employment off-farm. Asking the husband
                                    to pay her for her work is also an option. Also, see our", tags$a(href="https://rpubs.com/Gendergroup/808624", "Gendergroup pension project"), 
                                      "where we observed several pathways for german in-married farm wives to achieve money for several pension options. It shows vividly
                                    that depending on the local context, many different options should be modeled, depending on inner-household and external social pressure.
                                    We also included the case of high inner-household pressure as an option to choose the less monetarily valuable but peace-keeping option 
                                    of still working on her husband's farm but either being paid for the same work or having her own farm branch, from which she can keep the money.
                                    Processing food might be an example for her own farm brach.
                                    This shows the importance of leaving the decision to the decision-maker and not only observing 
                                    the economically best outcome. The farm wife decides what she would like to do and what options would be doable for her in her own individual
                                    situation. If the study is done with a group of women, shiny app with sliders like the ones you saw in the first tab (Decision analysis basics) 
                                    might be helpful for a single woman to adapt the model for her
                                    individual situation."),
                                    tags$code("The Next Step Is the Allocation Of Resources "),
                                    p("With the money that was gathered within the former step, 
                                    an allocation of several resources should be possible. First agricultural resources like trees, livestock, crops 
                                    water, breeding achievements can be allocated. Information technologies, possibly exceeding agricultural knowledge,
                                    like mobile phones can also belong to the model part\"Education/Training\". 
                                    It needs to be mentioned that resources are often seperated into male and female resources,
                                    depending on their prestige, limiting women's access to benefit from their monetary value. High economic benefit might also be 
                                    associated with male income generation, so that only smaller amounts might be gathered without facing inner-household violence."),
                                    tags$strong("The Seperation Into Workforce And Agricultural Resources"),
                                    p("With agricultural resource allocation, farm women can invest in other goods like health care services. 
                                    A husband might also be willing to spend money  directly on these investements depending on local and individual cases.
                                    If the option of sticking to the status quo includes the resources food and health care,
                                    these also need to be included in the empowerment pathway to compare the monetary values to each other. Within the model,
                                    these resources are portrayed seperately, since food and health care are included in the next step:\"Workforce\".
                                    If a scientists wants to end the pathways earlier, he can include these variables within the \"Resources\" section.
                                    A scientist can stop the model by only calculating money availabilty for resources (\"Economy\" part), 
                                    or end the model one step further by calculating the impact these resources have on further monetary gain (\"Resources\" part)."), 
                                    tags$strong("Different Resources"),
                                    p("There are resources that are not directly sold to make profit with like chickens, but still benefit the monetary gain over a
                                    period of time. To portay these, two options are available: 
                                    The whole model can focus on education and economic gain for one specially researched resource allocation.
                                    Or the monetary gain these resource allocations produce can be estimated.
                                    A women might spend money on different resources, that theoretically further influence the empowerment pathway like a spiral: Working
                                    equipment and training or technological advantages like mobile phones can benfit her future farm work, her options and her informed 
                                    decision making. Technological resources like mobile phones or radios can be a crucial advantage for further information gathering. 
                                    The money could also be used for children's education, clothing or hiring farm labour. 
                                    Many of these resource allocations open the way for new strategies against climate change. It must be said that 
                                    resource allocations as well as economic allocations depending on the local and individual context can also benefit the whole family and
                                    might therefore in some contexts be welcomed by male family members as well. This topic will be further discussed in the", tags$em(" risk section"),
                                      "below.
                                    "))),
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
                                             width="40%"
                                             #width="400", 
                                             #height="304"
                                             ))),
                                  box(
                                    title = "", status = "primary",solidHeader = TRUE, collapside = TRUE,
                                    width = 12,
                                    tags$code("The Social Environmnet"),
                                    p("It is affecting all steps of the model as a cost reducer. In its negative form, a complex social environment 
                                    can be incorporated as several options to choose from to limit danger (see the Economic section) or as a risk."),
                                    tags$br(tags$code("The Stop Point")),
                                    p(" A problematic social environment in which complex behavior towards women is accepted or praised and victims are blamed,
                                    or strong backlash exists, can also strengthen problematic inner-household dynamics and masculinities. These can lead as well
                                    as the challenging behavior itself to safety issues. Safety issues have a negative effect on mental and physical health, possibly
                                    even causing death. This decision's riskiness is therefore called ",
                                      tags$strong("Safety"),
                                      "It should not be a moral question for a scientist to advise a woman on bettering her economic status 
                                    if this could harm her. But the scientist should not decide on this so-called stop for the decision-maker. 
                                    Every woman should have the possibility to determine whether she wants to change her situation. So, whether this risk occurs, 
                                    the model puts out a binominal distribution of 0/1. The model, so to say, stops when the decision-maker regards the process as too dangerous.
                                    Various studies show that economic strength can lead to backlash, and inner-household peace can be disturbed. If mental and physical safety
                                    can still be guaranteed in a form the decision-maker conforms with, options of less economic solid benefit and resource allocations can be 
                                    decided on, as the \"Economy\"
                                    part of the text shows. It is the decision-maker herself who should decide whether another option would be possible or not. 
                                    What should be marked in this context is that also the option to stick to a status quo could lead to health risks since they are a major issue here." 
                                      
                                    ))),
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
                                         img(src = "Empowernment_mindmap-only-agriculture.png",
                                             width="70%"
                                               #"1000" 
                                             #height="516"
                                         ))),
                                  box(
                                    title = "", status = "primary",solidHeader = TRUE, collapside = TRUE,
                                    width = 12,
                                    tags$code("Resources To Input Into The Model"),
                                    p("This mind map shows, which resources could be usefull for the \"Resources\" part of the model depending on the research
                                       goal and the local condition. Information technologies like mobile phones can also be part of \"Education/Training\" calculations."
                                    ))),
                                tags$a(href="http://inresgb-lehre.iaas.uni-bonn.de/impressum/", "Impressum", style = "font-size:15px;",style = "color: black;")
                        ),
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



