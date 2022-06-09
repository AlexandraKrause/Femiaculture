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
                                    plotOutput("plot1"), 
                                    
                                  ),
                                  box(
                                    title = "Femiaculture", status = "primary",solidHeader = TRUE, collapside = TRUE,
                                    width = 6, 
                                    tags$strong("Welcome to Femiaculture", style = "font-size:20px;"),
                                    p(" This website was build during a master thesis focusing on factors influencing rural farm-women's empowerment.
                                      A literature research was conducted, experts were asked and conferences visited to understand the underlying system
                                      of empowerment. A model was conducted, which visualised the system (see the 'Map'-section.). 
                                      Later, the Decision Analysis methodology was applied to conduct a generalized statistical model, 
                                      suggesting it as a measurement technique of the researched factors. 
                                      But Decision Analysis also bears the possibility for actual change in form of an online information system for farm women.
                                      Therefore, researchers need to adapt the model to local situations and change the webiste's code to 
                                      rural women's user needs. Find more information in my master thesis."),
                                    p("Please try it out yourself: You see the initial graph left on the page - if you changed the values on the slider, 
                                      you could reload the page.
                                      Beneath the sliders and the text, a table also shows the current values.
                                      So, what should a farm women decide on doing? Empower herself or choose not to change her status quo situation?
                                      Please use the slider and see how the graph changes: Depending on the amount of money the farm woman could earn,
                                      she should decide on eather the one or the other option. 
                                      Also, the decision is influenced by the costs for the influencing factors like education, the outcome benefits like
                                      health and food/ nutrition and the risk of being unsafe.
                                      As you see, input estimates are needed: Well estimated numbers to insert into such models."),
                                    p("To find out more, please klick on 'The Project' and read the following texts on this website."),
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
                                    sliderInput("slider1", "Education investment:", 1, 1000, c(10,50),step=1),
                                    sliderInput("slider2", "Economy investment:", 1, 1000, c(1,20),step=1),
                                    sliderInput("slider3", "Economy payout:", 1, 1000, c(50,200),step=1),
                                    sliderInput("slider4", "Status Quo Resources investment:", 1, 1000, c(30,100),step=1),
                                    sliderInput("slider5", "Status Quo Resources payout:", 1, 1000, c(20,90),step=1),
                                    sliderInput("slider6", "Empowerment Resources investment:", 1, 1000, c(30,100),step=1),
                                    sliderInput("slider7", "Empowerment Resources payout:", 1, 1000, c(200,300),step=1),
                                    sliderInput("slider8", "Status Quo Workforce investment:", 1, 1000, c(50,100),step=1),
                                    sliderInput("slider9", "Status Quo Workforce payout:", 1, 1000, c(30,100),step=1),
                                    sliderInput("slider10", "Empowerment Workforce investment:", 1, 1000, c(50,100),step=1),
                                    sliderInput("slider11", "Empowerment Workforce payout:", 1, 1000, c(300,1000),step=1),
                                    sliderInput("slider12", "SQ Husband's Workforce investment:", 1, 1000, c(50,100),step=1),
                                    sliderInput("slider13", "Husband's Workforce investment:", 1, 1000, c(10,50),step=1),
                                    sliderInput("slider14", "Coefficient of variation:", 1, 1, c(1,1),step=0.1),
                                    sliderInput("slider15", "Discout rate:", 1, 5, c(1,1),step=0.1),
                                    sliderInput("slider16", "Months of receiving money:", 1, 36, c(9,9),step=1),
                                    sliderInput("slider17", "Months of paying into empowerment efforts:", 1, 36, c(3,3),step=1),
                                    sliderInput("slider18", "Risk Safety:", 0.1, 1, c(0.5,0.5),step=0.1)
                                    
                                  )),
                                
                                fluidRow(
                                  box(
                                    title ="Self Adjusting Table", status = "primary", background = "navy", solidHeader = TRUE, collapside = TRUE, width = 12,
                                    tableOutput("table1")
                                  ))
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
                                    p(" Notice that not only the input parameters, but also the code might need to be adapted to local scenarios. 
                                    Partcipatory workshops will clarify if an adaptation is needed. 
                                    The scenario you see is the following: A farm woman is working on the farm and the household. She might be ristricted by factors
                                    that limit her empowerment (e.g., inner-household limitations, limited market-access, ...). She could for example decide on
                                    visiting training/eduaction options to find out more about economic options, use these options to gather money, buy more agricultural resources 
                                    like chickens and sell them to gather money for health care and food/ nutrition. Also, her husband might share responsibility for these investemnts.
                                    Other researched outputs like education would need to be further included into the core code. 
                                    More money for health care and food/nutrition might lead to more effective working hours
                                    per month doing differnt kinds of farm or domestic work."),
                                    p("For the decision analysis process, many values, so called 'input estimates' are needed.
                                    Every calculated step can include several of such estimates like 
                                    costs for hiring labor, market prices or loans. Also the estimation of the risk of unsafety should also be done by considering several
                                    possible risk factors like child marriage, husband's alcohol consumption or upbringing."),
                                    p("After adopting the model to a local situation, the application can be tailored to end-users.
                                    Think of sliders for these different input estimates that a female farmer could use to insert the values 
                                    she knows from her own live, adapting the model to her own individual situation.
                                    But not all values can be taken from her experience - he should find some estimates
                                    already on the website, for example from a local development program or other farmers.
                                    And for this, experts are needed: farmers, scientists, consultants, and others, who would like to participate in
                                    a workshop and fill out questionnaires to give these estimates. 
                                    Together with values from literature research, very good estimates 
                                    can be found. And luckily, the results are readily available for all decision-makers, workshop participants, and stakeholders
                                    as websites like this or handy apps make them easily accessible.
                                    Please consider taking part in the making of a model and visit the other tabs for more information."),
                                    p("Luedeling, E., & Shepherd, K. (2016). Decision-Focused Agricultural Research. Solutions, 7(5), 46-54.")
                                  ))
                        ),
                        
                        # forth tab content
                        tabItem(tabName = "Model",
                                h3("Model"),
                                fluidRow(box(
                                  title = "", status = "primary",solidHeader = TRUE, collapside = TRUE,
                                  width = 12, align ="center",
                                  column(10, offset = 1,
                                         tags$strong("The Impact pathway for Empowerment based on an economic assessment."),
                                         tags$p(" To  conduct a model an impact pathway like the following is needed, comparing two options with each other."),
                                         img(src = "impact_pathway1.png", 
                                             width="600", 
                                             height="339")# insert image :) src stands for source, url is ok. 
                                         #menuItem("Loopy1", icon = icon("pencil-alt"),
                                         #         href = "https://bit.ly/34gcgaQ")
                                  )),
                                  
                                  box(
                                    title = "", status = "primary",solidHeader = TRUE, collapside = TRUE,
                                    width = 12,
                                    tags$code("General Overview"),
                                    p(" During my thesis, I will research factors influencing rural farm women's empowerment in agricultural development.
                                  Within this master thesis, I will apply literature research and a decision analysis to assess the unequal
                                  resource distribution between male and female farmers and evaluate how gender can be captured in modeling efforts.
                                  Women contribute not only to farm work, they are also often the primary providers of care for children and elderly family members
                                  and are also often solely responsible for their family's nutrition and health (Meinzen-Dick et al., 2011). 
                                  Unfortunately, women often lack access to resources and knowledge (Farnworth et al., 2016).
                                  A shift in the distribution of resources between male and female farmers, such as women's access to education, 
                                  has the potential to positively impact this situation (Hallman, 2000; Quisumbing & Maluccio, 2003).
                                  It could site-specifically increase general decision-making power, and with it, productivity, 
                                  food, and nutrition security, as well as educational outcomes for women and children (Farooq et al., 2019; Hallman, 2000; Quisumbing & Maluccio, 2003). 
                                  Often these benefits are connected to other positive effects. For example, increased nutrition security can reduce difficult births (Hallman, 2000)."),
                                    
                                    p("What you see is my first draft of an impact pathway.
                                  It shows empowerment from an economic perspective, 
                                  quantifying each part in monetary values. It is a universal model that needs to be adapted to local needs, actual problems, and factors. 
                                  Many examples for local cases are shown within the Decision Analysis impact pathway you can find through the Map tab.
                                  The different parts of ecological and economic resource allocation visible through the Resource tab can be 
                                  inserted into the Resources part of the Impact pathway."),
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
                                    
                                  ))),
                        #fifth tbl content
                        tabItem(tabName = "Status",
                                h3("Status"),
                                fluidRow(box(
                                  title = "", status = "primary",solidHeader = TRUE, collapside = TRUE,
                                  width = 12,align ="center",
                                  column(10, offset = 1,
                                         img(src = "impact_pathway-status-quo.png",
                                             width="600", 
                                             height="274"))),
                                  box(
                                    title = "", status = "primary",solidHeader = TRUE, collapside = TRUE,
                                    width = 12,
                                    tags$code("Sticking to status Quo"),
                                    tags$br(p("The Option of sticking to the status quo leads down a path of losses. Empowerment 
                                    can be compared to a ladder: Depending on the situation the woman is in, the effects of the lack of Empowerment
                                    can be worse or less. But the general disadvantages stay the same. Women are often not paid for
                                    their farm work as well as their domestic work. Also,in various contexts men share less food with their 
                                    wives, especially when having one or more other wives after them. This can also be true for the children. It means that
                                    there is less money for these investments than there is within the Empowerment pathway."),
                                            p(" Another problem is when men are not paying for their wives' health care.
                                          So, these women have less or no money for these investments. In the end, the outcome first means less money for women's
                                    basic human needs like food and health care. But it can also include other resources 
                                    that are investigated within the local project.  Also, her earnings, e.g. from a job away from farm, 
                                    if there are any, need to be compared to the decision option to change the status quo.
                                    ")),
                                    (tags$strong("Missing workforce - the extra, possible variable")),
                                    p(" As visible in the graph, the main obstacles of being not empowered are life-threatening. 
                                    But they are also very much dependent on the local context and the chosen local scenario. The level
                                    of empowerment can be very different, and so is the position of females in families, their decision-making power,
                                    and their right to physical and psychological integrity. Not being empowered in the worst cases means the lack of human rights,
                                    and cuts of these rights can be seen in various local contexts and ladders of empowerment. Unfortunately, if the woman herself 
                                    has no money for health care and her husband is not paying for it, her health situation is in danger.
                                    This obviously also depends on local law and health care provision, but the concept stays the same. Not being paid for work might lead
                                    the way to problems with not having employment protection. Working equipment is usually not produced or explicitly bought for women,
                                    multiplying the risk of injuries. Health risks can also include children when they are for example, connected with lack in obstetrics. 
                                    Access to nutritious food can also be denied by the husband, as studies how. This can have a direct beneficial effect on a woman's 
                                  health and workforce.")
                                  ))),
                        #oder 700 und 320
                        
                        #sixt tbl content
                        tabItem(tabName = "Empowerment",
                                h3("Empowerment"),
                                fluidRow(box(
                                  title = "", status = "primary",solidHeader = TRUE, collapside = TRUE,
                                  width = 12, align ="center",
                                  column(10, offset = 1,
                                         img(src = "impact pathway-emp-pathway.png",
                                             width="800", 
                                             height="101"))),
                                  box(
                                    title = "", status = "primary",solidHeader = TRUE, collapside = TRUE,
                                    width = 12,
                                    tags$code("Changing status Quo"),
                                    p("The Empowerment pathway shows the costs that need to be paid for education, 
                                    which leads to better economic standing. This might, for example, exists because
                                    the decision-maker decides to be employed and be paid for her work. Then resources
                                    can be acquired. This includes more money for food and health care that 
                                    needs to be calculated if these parts are also incorporated into the option of sticking 
                                    to the status quo. Like a ladder, the situation might only be changed stepwise. Therefore,
                                    the Empowerment pathway might contain several more or less economically beneficial options 
                                    depending on what is possible for the woman in her situation. Depending on the actual decision,
                                    some steps might be left out or varied."),
                                    tags$strong("The first step would be Education, which is a cost factor."),
                                    p("Information about negotiations might require investments in education. In these cases, this step is essential.
                                    An example for this first step would be: A woman needs to change her time use as an initial step to get further information on how to change her status quo.
                                    Education means Information gathering. An essential step to empowerment is the awareness of the current social situation of females, especially within the community,
                                    and possibilities to better the case. Understanding what empowerment is or could be is necessary for achieving a more empowered situation for oneself. Then other themes
                                    like nutrition, health, climate change, and agricultural practices can be understood from the women's perspective. The amount of money paid for a seminar itself, transportation,
                                    internet, electricity, or other factors is calculated here. A significant cost factor for all parts of the model is time: To number the effort, the amount of money she would need
                                    to pay for someone else doing her work (e.g., Child care, fieldwork) could be calculated. Many women in agriculture are not paid for their work and are expected to do long work hours
                                    when they are sick, it is hard to find someone who takes their place. This model assumes that there is a monetary value to this. Even if the work could not be done by someone else, 
                                    but the woman later will have to work more, this number replicates the cost for a time-use change. A beneficial factor might be a social environment beneficial for the female farmer. 
                                    Other women from women's groups with who she could share transportation would be an excellent example in this case. But for all steps of the model,
                                    help from a positive environment could lower the cost for each step."),
                                    tags$strong("The next step would be Economy:"),
                                    p("The economy part of the model also means costs like time use or child or elderly care payments.
                                    But it also includes economic gains like finding a paid job or employment off farm. Asking the husband
                                    to pay her for her work is also an option. Also, see our", tags$a(href="https://rpubs.com/Gendergroup/808624", "Gendergroup pension project"), 
                                      "where we observed several pathways for in-married farm wives to achieve money for several pension options. It shows vividly
                                    that depending on the local context, many different options should be modeled, depending on inner-household and external social pressure.
                                    We also included the case of high inner-household pressure as an option to choose the less monetarily valuable but peace-keeping option 
                                    of having an own branch on her husband's farm. This shows the importance of leaving the decision to the decision-maker and not only observing 
                                    the economically best outcome. The farm wife decides what she would like to do and what options would be doable for her in her own individual
                                    situation. If the study is done with a group of women, shiny app with sliders like the ones you saw in the first tab (Decision analysis basics) 
                                    might be helpful for a single woman to adapt the model for her
                                    individual situation."),
                                    tags$strong("The next step would be Resource allocation: "),
                                    p("With the money that was gathered within the former step, 
                                    an allocation of several resources should be possible. Trees, livestock, crops 
                                    (since there can be typical so-called male and female crops), water, breeding achievements 
                                    (seeds/ livestock), ... . If the option of sticking to the status quo includes the parts named food and health care,
                                    these also need to be included in here to compare the monetary values to each other. This point could include insurance payments or retirement savings. 
                                    These also could influence health and old-age poverty. Many different resources like saving working
                                    equipment better nutrition/cooking options could also benefit health. Technological resources like mobile phones or radios are also
                                    crucial for further information gathering. The money could also be used for children's education. Many of these resource allocations
                                    open the way for new strategies against climate change and biodiversity options, which are not seldom welcomed by women. If food and 
                                    health care are part of the option of sticking to the status quo, they need to be included here as well. It must be said that many 
                                    resource allocations as well as economic allocations depending on the local and individual context can also benefit the whole family and
                                    might therefore be welcomed by male family members as well. This topic will be further discussed in the", tags$em(" risk section"),
                                      "below.
                                    ")))
                        ),
                        #seventh tab content
                        tabItem(tabName = "Risk",
                                h3("Risk"),
                                fluidRow(box(
                                  title = "", status = "primary",solidHeader = TRUE, collapside = TRUE,
                                  width = 12, align ="center",
                                  column(10, offset = 1,
                                         img(src = "riskmap.png",
                                             width="400", 
                                             height="304"))),
                                  box(
                                    title = "", status = "primary",solidHeader = TRUE, collapside = TRUE,
                                    width = 12,
                                    tags$code("The social Environmnet"),
                                    p("It is affecting all steps of the model as cost reducers. In its negative form, a complex social environment 
                                    is incorporated as several options to choose from (see the Economic section) or as a risk."),
                                    tags$br(tags$code("The Stop Point")),
                                    p(" A problematic social environment in which complex behavior towards women is accepted or praised and victims are blamed,
                                    or strong backlash exists, can also strengthen problematic inner-household dynamics and masculinities. These can lead as well
                                    as the challenging behavior itself to safety issues. Safety issues have a negative effect on mental and physical health, possibly
                                    even causing death. This decision's riskiness is therefore called ",
                                      tags$strong("safety"),
                                      "It should not be a moral question for a scientist to advise a woman on bettering her economic status 
                                    if this could harm her. But the scientist should not decide on this so-called stop for the decision-maker. 
                                    Every woman should have the possibility to determine whether she wants to change her situation. So, whether this risk occurs, 
                                    the model puts out a binominal distribution of 0/1. The model, so to say, stops when the decision-maker sees the process as too dangerous.
                                    Various studies show that economic strength can lead to backlash, and inner-household peace can be disturbed. If mental and physical safety
                                    can still be guaranteed in a form the decision-maker conforms with, options of less economic solid benefit and resource allocations can be 
                                    decided on, as the ",
                                      tags$em("Economy"), 
                                      "part of the text shows. It is the decision-maker herself who should decide whether another option would be possible or not. 
                                    What should be marked in this context is that also the option to stick to a status quo also could lead to health risks since they are a major issue here." 
                                      
                                    )))
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
                                         tags$p("The pathway is the result of a literature analysis and the numbers stand for sources. Find the excel literature sources table here:", 
                                                (tags$a(href="https://github.com/AlexandraKrause/Thesis/blob/main/methods-excel.xlsx", "https://github.com/AlexandraKrause/Thesis/blob/main/methods-excel.xlsx")),
                                                tags$p("And find statistics regarding the literature review here:",
                                                       tags$a(href="https://github.com/AlexandraKrause/Thesis#readme", "https://github.com/AlexandraKrause/Thesis#readme"))),
                                         
                                         tags$hr(),
                                         tags$br(img(src = "Extended-Decision-Analysis-Impact-Pathway-06.2022.drawio.png", align ="center"))
                                  )))),
                        
                        
                        tabItem(tabName = "Resources",
                                h3("Resources"),
                                fluidRow(box(
                                  title = "", status = "primary",solidHeader = TRUE, collapside = TRUE,
                                  width = 12, align ="center",
                                  column(10, offset = 1,
                                         img(src = "Empowernment_mindmap-only-agriculture.png",
                                             width="800", 
                                             height="516"))),
                                  box(
                                    title = "", status = "primary",solidHeader = TRUE, collapside = TRUE,
                                    width = 12,
                                    tags$code("Resources to input into the model"),
                                    p("This mind map shows, which resources could be inserted into the Resource part of the model depending on the research
                                       goal and the local condition.")))),
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
                                         or Contact me via s7alkrau@uni-bonn.de"),
                                         tags$a(href="https://github.com/AlexandraKrause", "click for github"),
                                         img(src = "uni_logo.png")))
                                )))
                      
                      
                    ))



