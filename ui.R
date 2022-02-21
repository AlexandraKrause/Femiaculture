library(shiny)
library(shinydashboard)
library(tidyverse)
library(readr)
library(decisionSupport)
library (DiagrammeR)
library(tidyverse)
library(ggplot2)
library(plyr)
library(dplyr)
#not necessary to download!!!:
#install.packages("easystats", repos = "https://easystats.r-universe.dev")
#library(easystats)



ui <- dashboardPage(skin =  "purple",
                    dashboardHeader(title = "Femiaculture",
                                    dropdownMenuOutput("messageMenu")),
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem("Decision analysis basics", tabName = "dashboard", icon = icon("brain",class =NULL, lib="font-awesome" )),
                        menuItem("The project", tabName = "The project", 
                                 menuSubItem("Model", tabName = "Model", icon = icon("pencil-alt",class =NULL, lib="font-awesome")),
                                 menuSubItem("Status", tabName = "Status", icon = icon("map-marker-alt",class =NULL, lib="font-awesome" )),
                                 menuSubItem("Empowerment", tabName = "Empowerment", icon = icon("venus",class =NULL, lib="font-awesome" )),
                                 menuSubItem("Risk", tabName = "Risk", icon = icon("exclamation-triangle",class =NULL, lib="font-awesome" )),
                                 menuSubItem("Map", tabName = "Map", icon = icon("paper-plane",class =NULL, lib="font-awesome" )),
                                 menuSubItem("Resources", tabName = "Resources", icon = icon("seedling",class =NULL, lib="font-awesome" )),
                                 menuSubItem("Impressum", tabName = "Impressum", icon = icon("star",class =NULL, lib="font-awesome" ))
                                 
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
                                    plotOutput("plot1")
                                    
                                  ),
                                  box(
                                    title = "Controls", status = "primary", solidHeader = TRUE, collapside = TRUE,
                                    width = 4,
                                    sliderInput("slider", "Market price for small farms:", 0.1, 1.9, c(0.1,1.9)),
                                    sliderInput("substrate", "Substrate cost for compost:", 0.1, 1.0, 0.34),
                                    sliderInput("peat", "Substrate cost for peat:", 0.1, 3.0, 1.8)
                                  )),
                                fluidRow(
                                  box(
                                    title = "Decision Analysis", status = "primary",solidHeader = TRUE, collapside = TRUE,
                                    width = 12,
                                    p("Decision analysis implies actual help for decision-makers and is a powerful tool for system change. 
                                    It provides a quantitative assessment of interviews and workshops leading to the conduction of a stochastic model.
                                    The result is a holistic model of a complex system that also incorporates a decision's riskiness (Luedeling & Shepherd, 2016).
                                    Working inter- & trans-disciplinary with for example scientists and farmers also gives new possibilities for information gathering. 
                                    If repeated with the same group, it could also visualize change over the years after the last decision analysis was conducted."), 
                                    p(" This is only an easy example of decision analysis with estimated values. Please see it as just an example of the possibilities of this methodology.
                                    The scenario you see is the following: A small farmer near big cities is growing and selling salad in small pots. 
                                    He has access to local markets and is currently using peat as a substrate. 
                                    Ultimately, he would like to know if using food-based compost would be a good decision market-wise. 
                                    Unfortunately, the compost costs more than peat. But he might see higher incomes due to better
                                    marketing of salads produced with the compost due to its higher environmental value: Customers might pay more for this salad.
                                    The model calculates the scenario for a five-year duration, using values of 0.34 for peat cost, 1.8 for substrate cost
                                    And estimating farmers' earnings as between 0.1 and 1.9. You see the initial graph on the page - if 
                                    you changed the values on the slider, you could reload the page.
                                    Beneath this text, a table also shows the current values.
                                    So what should the farmer decide on doing? 
                                    Please use the slider and see how the graph changes: Depending on the amount of money the farmer could earn,
                                    he should decide on using compost or peat. Also, the decision changes with the costs for the two substrates.
                                    As you see, input estimates are needed: Numbers to insert into such models that are well estimated.
                                    But I cannot do this alone. 
                                    And for this, experts are needed: farmers, scientists, consultants and others, who would like to participate in a workshop or fill out questionnaires
                                    to give these estimates. Together with values from literature research, very good estimates can be found. 
                                    And luckily, the results are readily available for all decision-makers, workshop participants, and stakeholders
                                    as websites like this or handy apps make them easily accessible.
                                    Please consider taking part in the making of a model and visit the other tabs for more information."),
                                    
                                    p("Luedeling, E., & Shepherd, K. (2016). Decision-Focused Agricultural Research. Solutions, 7(5), 46-54.")
                                  )),
                                fluidRow(
                                  box(
                                    title ="Self Adjusting Table", status = "primary", background = "navy", solidHeader = TRUE, collapside = TRUE, width = 8,
                                    tableOutput("table1")
                                  ))
                        ),
                        # Second tab content
                        tabItem(tabName = "The Project",
                                h2("Widgets tab content"),
                                menuItem("Source code", 
                                         href = "https://github.com/rstudio/shinydashboard/")
                                
                                
                        ),
                        # third tab content
                        tabItem(tabName = "Model",
                                h3("Model"),
                                fluidRow(box(
                                  title = "", status = "primary",solidHeader = TRUE, collapside = TRUE,
                                  width = 12, align ="center",
                                  column(10, offset = 1,
                                         tags$strong("The Impact pathway for Empowerment based on an economic assessment."),
                                         tags$p(" To  conduct a model an impact pathway like the following is needed, comparing two options with each other."),
                                         img(src = "impact_pathway.png", 
                                             width="600", 
                                             height="339"),# insert image :) src stands for source, url is ok. 
                                         menuItem("Loopy1", icon = icon("pencil-alt"),
                                                  href = "https://bit.ly/34gcgaQ"))),
                                
                                box(
                                  title = "", status = "primary",solidHeader = TRUE, collapside = TRUE,
                                  width = 12,
                                  tags$code("General Overview"),
                                  p(" During my thesis I will research factors influencing rural farm women's empowerment in agricultural development.
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
                                  This is a general impact pathway that is applicable for several local situations. Many examples for local cases are shown within the mind map you can see in the Map section below, 
                                  even though it has its boundaries. The different parts of ecological and economic resource allocation visible in the mind map can be 
                                  inserted into the Resources part."),
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
                        #forth tbl content
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
                                          p(" Another problem is when men are not paying for their wives' retirements and health care.
                                          So, these women have less or no money for these investments. In the end, the outcome first means less money for women's
                                    basic human needs like food, health care, and retirement. But it can also include other resources 
                                    that are investigated within the local project.  Also, her earnings, e.g.from a job away from farm, 
                                    if there are any, need to be compared to the decision option to change the status quo.
                                    ")),
                                  (tags$strong("Missing workforce - the extra, possible variable")),
                                  p(" As visible in the graph, the main obstacles of being not empowered are life-threatening. 
                                    But they are also very much dependent on the local context and the chosen local scenario. The level
                                    of empowerment can be very different, and so is the position of females in families, their decision-making power,
                                    and their right to physical and psychological integrity. Not being empowered in the worst cases means the lack of human rights,
                                    and cuts of these rights can be seen in various local contexts and ladders of empowerment. Unfortunately, if the woman herself 
                                    has no money for a pension, insurance, or health care itself and her husband is not paying for it, her health situation is in danger.
                                    This obviously also depends on local law and health care provision, but the concept stays the same. Not being paid for work might lead
                                    the way to problems with not having employment protection. Working equipment is usually not produced or explicitly bought for women,
                                    multiplying the risk of injuries. Health risks can also include children when they are for example, connected with lack in obstetrics. 
                                    Access to nutritious food can also be denied by the husband, as studies how. This can have a direct beneficial effect on a woman's 
                                  health and workforce.")
                                ))),
                        #oder 700 und 320
                        #fifth tbl content
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
                                    can be acquired. This includes more money for food and health care/retirement that 
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
                                    (seeds/ livestock), ... . If the option of sticking to the status quo includes the parts named food and health care/retirement,
                                    these also need to be included in here to compare the monetary values to each other. This point could include insurance payments and gains as well 
                                    as retirement plans. These also could influence health and old-age poverty. Many different resources like saving working
                                    equipment better nutrition/cooking options could also benefit health. Technological resources like mobile phones or radios are also
                                    crucial for further information gathering. The money could also be used for children's education. Many of these resource allocations
                                    open the way for new strategies against climate change and biodiversity options, which are not seldom welcomed by women. If food and 
                                    health care are part of the option of sticking to the status quo, they need to be included here as well. It must be said that many 
                                    resource allocations as well as economic allocations depending on the local and individual context can also benefit the whole family and
                                    might therefore be welcomed by male family members as well. This topic will be further discussed in the", tags$em(" risk section"),
                                    "below.
                                    ")))
                        ),
                        #sixt tab content
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
                        # seventh tab content
                        tabItem(tabName = "Map",
                                h3("Map"),
                                fluidRow(box(
                                  title = "", status = "primary",solidHeader = TRUE, collapside = TRUE,
                                  width = 12, align ="center",
                                  column(10, offset = 1,
                                         tags$strong("The mind map, on which the impact pathway is based.",
                                         tags$a(href="Empowernment_mindmap-big-letters.html", "Please Click here to see the whole interactive HTML5-Map in high resulution.")),
                                         img(src = "Empowerment_Mindmap.png", align ="right",
                                             width="1100", 
                                             height="582")
                                         )))),
                        
                        # eight tab content
                        tabItem(tabName = "Resources",
                                h3("Resources"),
                                fluidRow(box(
                                  title = "", status = "primary",solidHeader = TRUE, collapside = TRUE,
                                  width = 12, align ="center",
                                  column(10, offset = 1,
                                         img(src = "Empowernment_mindmap-only-agriculture.png", align ="right",
                                             width="800", 
                                             height="516"))),
                                box(
                                  title = "", status = "primary",solidHeader = TRUE, collapside = TRUE,
                                  width = 12,
                                  tags$code("Resources to input into the model"),
                                  p("This part of the mind map shows, which resources could be inserted into the model depending on the reserch
                                    question and the local condition.")))),
                        # size 700 and 451
                        #ninth tab content
                        tabItem(tabName = "Impressum",
                                h3("Impressum"),
                                box(
                                  title = "", status = "primary",solidHeader = TRUE, collapside = TRUE,
                                  width = 12,
                                  column(10, offset = 1,
                                         tags$strong("Impressum"),
                                         p("...."), 
                                         icon = icon("star",class =NULL, lib="font-awesome"),
                                         p("Someones adress (university i guess) and logos by university bonn and cgiar and more? have to be in here. does anyone know?
                                           and the whole page has to be visible directly when using the link, not by a sidebar menu"),
                                         p("Visit me at github.com/AlexandraKrause, Find me on twitter: @Al__Krause
                                         or Contact me via s7alkrau@uni-bonn.de"),
                                         tags$a(href="https://github.com/AlexandraKrause", "click for github")
                                  )))
                        
                        
                      ))
)


