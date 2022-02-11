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
                                 menuSubItem("Visuals", tabName = "Visuals", icon = icon("seedling",class =NULL, lib="font-awesome" ))
                                 
                        ))
                    ),
                    dashboardBody(
                      tags$head(
                        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
                        tags$link(rel = "stylesheet", type = "text/css", href = "custom2.css")),
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
                                    
                                    p(" This is only an easy, fast-made example of decision analysis with estimated values. Please see it as just an example of the possibilities of this methodology.
                                    The scenario you see is the following: A small farmer near big cities is growing and selling salad in small pots. He has access to local markets and is currently using peat as a substrate. 
                                    Ultimately, he would like to know if using food-based compost would be a good decision market-wise. 
                                    Unfortunately, the compost costs more than peat. But he might see higher incomes due to better
                                    marketing of salads produced with the compost due to its higher environmental value.
                                    The model calculates the scenario for a five-year duration, using values of 0.34 for peat cost, 1.8 for substrate cost
                                    And estimating farmers' earnings as between 0.1 and 1.9. You see the initial graph on the page - if you changed the values on the slider, you could reload the page.
                                    Beneath this text, a table also shows the current values.
                                    So what should the farmer decide on doing? 
                                    Please use the slider and see how the graph changes: Depending on the amount of money the farmer could earn,
                                    he should decide on compost or peat. Also, the decision changes with the costs for the two substrates.
                                    As you see, input estimates are needed: Numbers to insert into such models that are well estimated.
                                    But I cannot do this alone. 
                                    And for this, experts are needed: farmers, scientists, consultants and others, who would like to participate in a workshop
                                    and give these estimates. Together with values from literature research, very good estimates can be found. 
                                    And luckily, the results are readily available for all decision-makers, workshop participants, and stakeholders as websites like this or handy apps make them easily accessible.
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
                                box(
                                  title = "", status = "primary",solidHeader = TRUE, collapside = TRUE,
                                  width = 12,
                                  column(10, offset = 1,
                                         tags$strong("The Impact pathway for Empowerment based on an economic assessment."),
                                         tags$p(" To  conduct a model an impact pathway like the following is needed, comparing two options with each other."),
                                         img(src = "impact_pathway.png", 
                                             width="500", 
                                             height="300"),# insert image :) src stands for source, url is ok. 
                                         menuItem("Loopy1", icon = icon("pencil-alt"),
                                                  href = "https://bit.ly/34gcgaQ"))),
                                
                                box(
                                  title = "", status = "primary",solidHeader = TRUE, collapside = TRUE,
                                  width = 12,
                                  p(" What you see is my first, yet incomplete draft of an impact pathway.  It shows empowernment from an
                                economic perspective, quantifying each parts in monetary values. It is an universal model that needs to be adapted to 
                                local needs, actual problems and factors."),  
                                  tags$code("General Overview"),
                                  p("This is a general impact pathway that is applicable for several local situations. 
                                    Many examples for  local situations are shown within the mind map, even though it has its boundaries. 
                                    Depending on the actual decision, some steps might be left out or be varied. "),
                                  tags$code("Sticking to status Quo"),
                                  tags$br(tags$br(tags$strong("Missing workforce - the extra, possible variable"))),
                                  p("As visible in the graph, the main obstacles of being not empowered are live threatening. But they are also very much dependent on the local context and the chosen local scenario. 
                                    The level of empowerment can be very different and so is the position of females in families, their decision-making power and their right to physical and psychological integrity. 
                                    Not being empowered in the worst cases means the lack of human rights and cuts of these rights can be seen in various local contexts and ladders of empowerment, as also can be seen in my previous group work, 
                                    the ",  tags$a(href = "https://rpubs.com/Gendergroup/808624", "Gendergroup pension project."),
                                    "Unfortunately, assuming that the woman herself has no money for a pension, insurances or health care itself and her husband is not paying for it,
                                    her health situation is in danger. This obviously also depends on local law and health care provision, but the concept stays the same. Not being payed for work might 
                                    lead the way to problems with not having employment protection. Working equipment is usually not produced or bought specifically for women, multiplying the risk of injuries. 
                                    Health risks can also include children when they are for example connected with lacks in obstetrics. Nutritious problems can affect the health of the whole family. 
                                    The access to nutritious food can also be denied by the husband, as studies how. This can have a direct beneficial effect on a woman's health and workforce.
                                    But how to calculate this? The women's' work needs to be put in economic values: Her lifetime carrying for children, cooking, cleaning and doing agricultural labour.
                                    Bad health is directly affecting her ability to work. The amount in which it is affecting her ability to work differs in local contexts and depends on the status-quo situation.
                                    Calculations could include the number of accidents during work versus the payments and local usefulness of a health insurance. It could also include in local contexts often occurring
                                    diseases related to nutritious deficits. Local people and especially doctors could be able to give numbers of disease occurrence and incapability for work. 
                                    These values can then be used together with an estimation of the worth of female workforce. Obviously, under other local conditions and questions, other factors can be included
                                    as well. An example: as himmelweit() stated, less economic strength can lead to women having to use less save options like public transportation more often and more attacks 
                                    against them take place. 
                                    These attacks also can lower not only a woman's physical health as well. Depending on the local situation, this might be hard to estimate, though."),
                                  p("This part of the model therefore can also be a negotiation point with the husband or the male family members since the workforce will be lowered. 
                                  In these cases where money for obstetrics is missing, not only the woman's health and so her workforce, but also that of further children might be falling away. 
                                  Also, her earnings (e.g., job away from farm),if there are any, need to be compared the decision-option to change the status-quo."),
                                  p("This part of the model therefore can also be a negotiation point with the husband or the male family members, since the workforce will be lowered.
                                In these cases where money for obstetrics is missing, not only the woman's health and so her workforce, but also that of further children might be falling away.
                                Also her earnings, if there are any, need to be compared the decision-option to change the status-quo."),
                                  p("The Option of sticking to status quo, which is the lower one in the picture, leads down a path of losses. Empowerment as () found can be compared to a ladder: 
                                  Depending on the situation the woman is in, the situation can be worse or less. But the general disadvantages stay the same. 
                                  Many studies() state, that women are not payed for their farm work as well as their domestic work (himmelweit). Several studies state, 
                                  that men share less food with their wives, especially when having one or more other wives after her. 
                                  This can also be true for the children. It means, that there is less money for these investments than for the Empowerment pathway.
                                  Another problem is, that men are not paying for their wives retirements and health cares. 
                                  So, these women have less or no money for these investments. In the end the outcome 
                                  is ether less money for women's basic needs like food, health care and retirement, other
                                  resources that are investigated within the local project or more specifically less workforce.
                                  The last outcome highly depends on the specific question the project would like to answer 
                                  in its local context. Pro-WEAI measures women's time use by asking them what they do during 
                                  a day after waking up. Many women find it complicated to see how much work they do during the day.
                                  But it is possible to measure it and the costs that would need to be payed if someone else would do this work. 
                                  To estimate the years these costs would need to be payed or how many years 
                                    the women would be incapable to work I would suggest to either rely on doctor's opinions within local context or use a shiny app slider.
                                    "),
                                  tags$code("Changing status Quo"),
                                  tags$br(tags$br(tags$strong("The first step would be Education, which is a cost factor."))), 
                                  p("Information about negotiations might require investments into education. In these cases, this step is important. An example for this first step would be following:
                                  A woman needs to change her time use as an initial step to get further information on how to change her status quo. Education means Information gathering. An important 
                                  step to empowerment is the awareness of the current social situation of females, especially within the community and possibilities to better the situation. An understanding 
                                  of what empowerment is or could be is necessary for achieving a more empowered situation for oneself. Then other themes like nutrition, health, climate change and agricultural
                                  practices can be understood from the women's' perspective. The amount of money payed for a seminar itself, transportation, internet, electricity or other factors is calculated here.
                                  A very important cost factor for all parts of the model is time: To number the effort, the amount of money she would need to pay for someone else doing her work
                                  (e.g. Child care, field work) could be calculated. Many women in agriculture are not payed for their work and expected to do work long hours. When they are sick, 
                                  it is hard to find someone who takes their place. This model assumes that there is a monetary value to this. Even if the work could not be done by someone else, but the woman 
                                  later will have to work more, this number replicates the cost for a time-use change. A beneficial factor might be a social environment that is beneficial for the female farmer.
                                  Other women from women's groups who she could share
                                    transportation with would be a good example in this case. But for all steps of the model, help from a positive environment could lower the cost for each step. "),
                                  tags$strong("The next step would be Economy:"),
                                  p("Economy also means costs like time use or payments for child or elderly care. But it also includes economic gains like finding a payed job or an employment on a farm. 
                                  Asking the husband to pay her for her work is also an option.
                                  This point could include insurance payments and gains as well as retirement plans. These also could influence health and old-age poverty. 
                                  Also see our", tags$a(href = "https://rpubs.com/Gendergroup/808624", "Gendergroup pension project"),
                                    "where we observed several pathways for in-married farm wives to achieve money for several pension options. It shows vividly, that depending on the local
                                    context many different options should be modelled, depending on inner-household and outer social pressure. We also included the case of high inner-household 
                                    pressure as an option to choose the less monetarily valuable, but peace-keeping option of having an own branch on her husband's farm. This shows the importance
                                    of leaving the decision to the decision-maker and not only observing the economically best outcome. It is the farm wife who decides what she would like to do and
                                    what options would be doable for her in her own individual situation. 
                                    In case the study is done with a group of women a shiny app with sliders might be helpful for a single woman to adapt the model for her own situation."),
                                  tags$strong("The next step would be Resource allocation:"),
                                  p("With the money that was gathered within the former step, an allocation of several resources should be possible. Trees, livestock, crops (since there can be typical
                                  so-called male and female crops), water, breeding achievements (seeds/ livestock), ... . If the option of sticking to status quo includes 
                                  the parts named food and health care/retirement, these also need to be included here to compare the monetary values to each other. Many different resources
                                  like save working equipment, better nutrition/cooking options could also benefit health. Technological resources like mobile phones or radios are also important 
                                  for further information gathering. The money could also be used for children's education. Many of these resource allocations open the way for new strategies against 
                                  climate change and biodiversity options, which are not seldom welcomed by women. It must be said that many resource allocations as well as economic allocations depending 
                                  on the local and individual context can also benefit the whole family and might therefore be welcomed by male family members as well. 
                                    This topic will be further discussed in the", tags$em(" risk section"),
                                    "below.
                                    "),
                                  tags$code("The social Environmnet"),
                                  p("The Social environment is affecting all steps of the model as cost reducers.
                                  Its negative form, a problematic social environment is incorporated as several options to choose from (see the Economic section down below) or as a risk."),
                                  tags$code("The decision's riskiness"),
                                  tags$br(tags$br(tags$strong("The Stop Point"))),
                                  p(" A problematic social environment, in which problematic behaviour towards women is accepted or praised and victims are blamed,
                                  or strong backlash exists, can also strengthen problematic inner-household dynamics and masculinities. 
                                  These can lead as well as the problematic behaviour itself to safety issues. Safety issue have a negative effect on
                                  mental and physical health, possibly even causing death. This decision's riskiness is therefore called",
                                    tags$strong("safety"),
                                    "It should not be a moral question for a scientist to advice a woman on bettering her economic status if this could harm her.
                                  But the scientist should not decide on this so-called stop for the decision maker. Every woman should have the possibility to
                                  decide whether she wants to change her situation. So, whether this risk occurs, the model puts out a binominal distribution of 0/1.
                                  The model so to say stops when the decision maker sees the process as too dangerous. Various studies show that economic strength 
                                  can lead to backlash and inner-household peace can be disturbed. If mental and physical safety can still be guaranteed in a form the
                                  decision maker conforms with, options of less strong economic benefit and resource allocations can be decided on as the",
                                    tags$em("Economy"), 
                                    "of the text shows. It is the decision maker herself who should decide whether another option would be possible or not. What should be 
                                  marked in this context is, that also the option to stick to a status quo also could lead to health risks, since they are a major issue here." 
                                    ,
                                    
                                  ))),
                        # forth tab content
                        tabItem(tabName = "Visuals",
                                h3("Visuals"),
                                box(
                                  title = "", status = "primary",solidHeader = TRUE, collapside = TRUE,
                                  width = 12,
                                  column(10, offset = 1,
                                         tags$strong("The mind map, on which the impact pathway is based"),
                                         img(src = "Empowerment_Mindmap.png", align ="right",
                                             width="1100", 
                                             height="600"))),# insert image :) src stands for source, url is ok. 
                                box(
                                  title = "", status = "primary",solidHeader = TRUE, collapside = TRUE,
                                  width = 12,
                                  column(10, offset = 1,
                                         
                                         tags$br(tags$strong("The Mind map for finding the risk called safety")),
                                         img(src = "riskmap.png",
                                             width="400", 
                                             height="300"),# insert image :) src stands for source, url is ok. 
                                  ))
                                
                        ))
                    ))


