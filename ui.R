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
#install.packages("easystats", repos = "https://easystats.r-universe.dev")
library(easystats)



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
                                             width="400", 
                                             height="300"),# insert image :) src stands for source, url is ok. 
                                         menuItem("Loopy1", icon = icon("pencil-alt"),
                                                  href = "https://bit.ly/34gcgaQ"))),
                                
                                box(
                                  title = "", status = "primary",solidHeader = TRUE, collapside = TRUE,
                                  width = 12,
                                  p(" What you see is my first, yet incomplete draft of an impact pathway.  It shows empowernment from an
                                economic perspective, quantifying each parts in monetary values. It is an universal model that needs to be adapted to 
                                local needs, actual problems and factors."),  
                                  tags$code("Changing status Quo"),
                                  tags$br(tags$br(tags$strong("The first step would be Education, which is a cost factor."))), 
                                  p("An example for this firstv step would be following: A woman needs to change her time use as an initial step
                                to get furtehr information on how to change her status quo. Education means Information gathering. An important step to empowerment is the awareness of the current social 
                                situation of females, especially  within the community and possibilities to better the situation. 
                                An understanding of what empowernment is or could be is necessary for achieving a more empowered situation for oneself.
                                Then other themes like nutrition, health, climate change and agricultural practices can be understood from the womens' perspective.
                                The amount of money payed for
                                a seminar itself, transportation, internet, electricity
                                or other factors is calculated here.A very important cost factor for all parts of the model is time: To number the effort, the amount of money she would need to pay for
                                someone else doing her work (e.g. Child care, field work) is calculated. Many women in agriculture are not payed for their work
                                and expected to do work long hours. When they are sick, it is hard to find someone who takes their place. This model assumes that
                                there is a monetary value to this. Even if the work could not be done by somewone else, but the woman later will have to work more,
                                this number replicates the cost for a time-use change. A beneficial factor might be a social environment that is beneficial wor the female farmer.
                                Other women from womens' groups who she could share transportation with would be a good example in this case. But throuout the model, help from a
                                positive enviroment can lower the cost for each step."),
                                  tags$strong("The next step would be Economy:"),
                                  p("Economy also means cost like time use or payments for child or eldery care. But it also includes economic gains
                                  like finding a payed job or an employment on a farm. Asking the husband for payment for her work is also an option. Inforamtion about negotiation otions
                                  might require investments into education. It could include insurance payments and gains as well as retirement plans. These also could have an effect on 
                                  health and old-age poverty.
                                  Also see our", tags$a(href = "https://rpubs.com/Gendergroup/808624", "Gendergroup pension project"),
                                    "where we observed several pathways for in-married farm wives to achieve money for several pension options.
                                    It shows vividly, that depending on the local context many different options can be choosen, also depending on 
                                    inner-household and outer social pressure. It is the farm wive who decides what she would like to do and what options
                                    would be doable for her in her own individual situation. We also included the case of high inner-household pressure as an option
                                    to choose the less monetarily valuable, but peace-keeping option of having an own branch on her husband's farm.
                                    This shows the importance of leaving the decison to the decision-maker and not only observing the economically best outcome."),
                                  tags$strong("The next step would be Resource allocation:"),
                                  p("With the money that was gatheredwithin the former step, an allocation of several ressources shouold be possible.
                                    Trees, livestock, crops (since there can be typical so-called male and female crops), water, breeding achievements (seeds/ 
                                    livestock),... . Many different ressources like Save working equipment, better nutrition/cooking options
                                    could also benefit health. Technological resources like handys or radios are also important for further information gathering.
                                    The money could also be used for children's education.
                                    Many of these ressource allocations open the way for new startegies against climate change and biodiversity options, which are not seldom
                                    welcomed by women.It has to be said that many ressource allocations as well as economic allocations depending on the local and individual context can also
                                    benefit the whole family and might therefore be welcomed by male family members as well. This topic will be further discussed in the", tags$em(" risk section"),
                                    "below.
                                    "),
                                  tags$code("Sticking to status Quo"),
                                  tags$br(tags$br(tags$strong("Missing workforce."))),
                                  p("As visible in the graph, the main obstacles of being not empowered are live threatening.But they are also very much dependent on the local context
                                  and the choosen local scenario. The level of empowernment can be very different and so is the position of females in families,
                                  their decision-making power and their right to physical and psychological integrity. Not being empowered in the worst cases
                                  means the lack of human rights and cuts of these rights can bes seen in various local contexts and ladders of empowernment, as also can be seen in my
                                  previous group work, the",  tags$a(href = "https://rpubs.com/Gendergroup/808624", "Gendergroup pension project."),
                                    "Unfortunately, assuming that the woman herself has no money for a pension, insurances or health care itself and her husband is not paying for it,
                                  her health situation is in danger. This obviously also depends on local law and health care provision, but the concept
                                stays the same.Not being payed for work might lead the way to problems with not having employment protection. Working equipment is
                                usually not produced or bought specifically for women, multipying the risk of injuries. Health risks can also include chuildren
                                when they are for example connected with lacks in obsterics. Nutritious problems can affect the health of the whole family. 
                                Studies show that, depending on the local context, women tend to pay more money for their childrens' education than males.But this can be the
                                opposite case as well in some localities. Still without economic empowernment, especially girls' education is at risk. Higher education
                                seems to benefit monetary income. Children not seldom pay for their parents' retirement. With less educated chikdren, the retirement payings might go down 
                                which might affect health. The access to nutritious food can also be denied by the husabnd, as studies how. This 
                                can have a direct beneficial effect on a woman's health and workforce. But how to calculate this? The womens' work need to be put in economic values:
                                Her life-time carying for children, cooking, cleaning, doing agricultural labour. Bad health is directly affecting her ability to work. The amount in which
                                it is affecting her ability to work differs in local contects and depends on the staus-quo situation. Calculations could include
                                the amount of accidents during work versus the payments and local usefulness of a health insurance. It could also include
                                in local contexts often accuring deseases realted to nutritious deficits. Some health insurances keep track about the probabilities of 
                                incapacity for work. (????google???) Osteoporosis, associated with bone breaking for example hinders the lifting of heavy things (???). Local people
                                could be able to give numbers of desease occurance and percentages of incapability for work. These values can then be used together with an estimation of the 
                                worth of female workforce.Obviously under other local conditions and questions, other factors can be included as well. An example: as himmelweit stated,
                                less economic strenght can lead to women having to use public transportation mor eoften and more attacks against them take place there. These attacks also acn lower
                                a woman's health."),
                                  p("This part of the model therefore can also be a negotiation point with the husband or the male family members, since the workforce will be lowered.
                                In these cases where money for obstetrics is missing, not only the woman's health and so her workforce, but also that of further children might be falling away.
                                Also her earnings, if there are any, need to be compared the decision-option to change the status-quo."),
                                  tags$code("The decision's riskiness"),
                                  tags$br(tags$br(tags$strong("The Stop Point"))),
                                  p("A problematic social environment, in which problematic behaviour towards women is accepted or praised and victims are blaimed or
                                            strong backlash exists can also strenghen problematic inner-household dynamics and masculinities.
                                            These can lead as well as the to problematic behaviour itself to safety issues. Safety issue have a negative effect on menatl and physical health,
                                            possibly even causing death. This decision's riskiness is therefore called", tags$strong("safety."),
                                    "It should not be a moral question for a scientist to advice a woman on bettering her economic status if this could harm her. But the scientist should not
                                    decide on this so called stop for the decision maker. Every woman should have the possibility to decide wether or not she wants to change her situation.
                                    So wether or not this risk occurs, the model puts out a binominal distribution of 0/1. The model so to say stops when the decision maker sees the process as too dangerous.
                                    Various studies show that economic strengh can lead to backlash and inner-household peace can be disturbed. If mental and physical safety can still be guarnteed in a dform
                                    the decision maker conforms with, options of less strong economic benefit and ressource allocations can be decided on as the", tags$em("Economy"), "part of
                                    the text shows. It is the decision maker herself who should decide wether another option would be possible or not.
                                    What should be marked in this context is, that also the option to stick to a status quo also could lead to health risks, since they are a major issue here.")
                                )),
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
                    

