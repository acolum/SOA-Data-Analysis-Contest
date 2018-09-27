sidebar <- dashboardSidebar(
  hr(),
  sidebarMenu(id="tabs",
              menuItem("Home", tabName="home", icon=icon("home"), selected=TRUE),
              menuItem("Summary", tabName="summary", icon=icon("line-chart")#,
              ),
              menuItem("Comparison", tabName = "comparison", icon=icon("bar-chart")#,
              ),
              menuItem("App Metrics", tabName = "app-metrics", icon = icon("file-text-o")),
              menuItem("Help", tabName = "help", icon = icon("question"))
  ),
  hr(),
  conditionalPanel("input.tabs=='summary'",
                   sliderInput(inputId = "summary_issueyear", 
                               label = "Issue Year",
                               min = min(salesdata$`Issue Year`), max = max(salesdata$`Issue Year`),
                               value = c(min(salesdata$`Issue Year`), max(salesdata$`Issue Year`)), step = 1),
                   sliderInput(inputId = "summary_obsyear", 
                               label = "Observation Year",
                               min = min(salesdata$`Observation Year`), max = max(salesdata$`Observation Year`),
                               value = c(min(salesdata$`Observation Year`),max(salesdata$`Observation Year`)), step = 1),
                   # value = c(2011,max(salesdata$`Observation Year`)), step = 1),
                   sliderInput(inputId = "summary_issueage", 
                               label = "Issue Age",
                               min = min(salesdata$`Issue Age`), max = max(salesdata$`Issue Age`),
                               value = c(min(salesdata$`Issue Age`),max(salesdata$`Issue Age`))),
                   # value = c(52,max(salesdata$`Issue Age`))),
                   sliderInput(inputId = "summary_attainedage", 
                               label = "Attained Age",
                               min = min(salesdata$`Attained Age`), max = max(salesdata$`Attained Age`),
                               value = c(min(salesdata$`Attained Age`),max(salesdata$`Attained Age`))),
                   sliderInput(inputId = "summary_duration", 
                               label = "Duration",
                               min = min(salesdata$Duration), max = max(salesdata$Duration),
                               value = c(min(salesdata$Duration),max(salesdata$Duration))),
                   checkboxGroupInput(inputId = "summary_gender",
                                      label = "Gender",
                                      choices = unique(salesdata$Gender),
                                      # selected = unique(salesdata$Gender),
                                      selected = unique(salesdata$Gender),
                                      inline = TRUE),
                   checkboxGroupInput(inputId = "summary_smoker",
                                      label = "Smoker Status",
                                      choices = unique(salesdata$`Smoker Status`),
                                      # selected = unique(salesdata$`Smoker Status`),
                                      selected = unique(salesdata$`Smoker Status`),
                                      inline = TRUE),
                   checkboxGroupInput(inputId = "summary_selectult",
                                      label = "Select/Ultimate Indicator",
                                      choices = unique(salesdata$Select_Ultimate_Indicator),
                                      # selected = unique(salesdata$Select_Ultimate_Indicator),
                                      selected = unique(salesdata$Select_Ultimate_Indicator),
                                      inline = TRUE)
  ),
  conditionalPanel("input.tabs=='comparison'",
                   sliderInput(inputId = "comparison_issueyear", 
                               label = "Issue Year",
                               min = min(salesdata$`Issue Year`), max = max(salesdata$`Issue Year`),
                               value = c(min(salesdata$`Issue Year`), max(salesdata$`Issue Year`)), step = 1),
                   sliderInput(inputId = "comparison_obsyear", 
                               label = "Observation Year",
                               min = min(salesdata$`Observation Year`), max = max(salesdata$`Observation Year`),
                               value = c(min(salesdata$`Observation Year`),max(salesdata$`Observation Year`)), step = 1),
                   # value = c(2011,max(salesdata$`Observation Year`)), step = 1),
                   box(
                     title = "Comparison Option 1",
                     status = "warning", solidHeader = TRUE,
                     width = 12,
                     collapsible = TRUE,
                     collapsed = TRUE,
                     sliderInput(inputId = "comparison_issueage_1", 
                                 label = "Issue Age",
                                 min = min(salesdata$`Issue Age`), max = max(salesdata$`Issue Age`),
                                 value = c(min(salesdata$`Issue Age`),max(salesdata$`Issue Age`))),
                     # value = c(52,max(salesdata$`Issue Age`))),
                     sliderInput(inputId = "comparison_attainedage_1", 
                                 label = "Attained Age",
                                 min = min(salesdata$`Attained Age`), max = max(salesdata$`Attained Age`),
                                 value = c(min(salesdata$`Attained Age`),max(salesdata$`Attained Age`))),
                     sliderInput(inputId = "comparison_duration_1", 
                                 label = "Duration",
                                 min = min(salesdata$Duration), max = max(salesdata$Duration),
                                 value = c(min(salesdata$Duration),max(salesdata$Duration))),
                     checkboxGroupInput(inputId = "comparison_gender_1",
                                        label = "Gender",
                                        choices = unique(salesdata$Gender),
                                        # selected = unique(salesdata$Gender),
                                        selected = unique(salesdata$Gender),
                                        inline = TRUE),
                     checkboxGroupInput(inputId = "comparison_smoker_1",
                                        label = "Smoker Status",
                                        choices = unique(salesdata$`Smoker Status`),
                                        # selected = unique(salesdata$`Smoker Status`),
                                        selected = "Smoker",
                                        inline = TRUE),
                     checkboxGroupInput(inputId = "comparison_selectult_1",
                                        label = "Select/Ultimate Indicator",
                                        choices = unique(salesdata$Select_Ultimate_Indicator),
                                        # selected = unique(salesdata$Select_Ultimate_Indicator),
                                        selected = unique(salesdata$Select_Ultimate_Indicator),
                                        inline = TRUE)
                   ),
                   box(
                     title = "Comparison Option 2",
                     status = "warning", solidHeader = TRUE,
                     width = 12,
                     collapsible = TRUE,
                     collapsed = TRUE,
                     sliderInput(inputId = "comparison_issueage_2", 
                                 label = "Issue Age",
                                 min = min(salesdata$`Issue Age`), max = max(salesdata$`Issue Age`),
                                 value = c(min(salesdata$`Issue Age`),max(salesdata$`Issue Age`))),
                     # value = c(52,max(salesdata$`Issue Age`))),
                     sliderInput(inputId = "comparison_attainedage_2", 
                                 label = "Attained Age",
                                 min = min(salesdata$`Attained Age`), max = max(salesdata$`Attained Age`),
                                 value = c(min(salesdata$`Attained Age`),max(salesdata$`Attained Age`))),
                     sliderInput(inputId = "comparison_duration_2", 
                                 label = "Duration",
                                 min = min(salesdata$Duration), max = max(salesdata$Duration),
                                 value = c(min(salesdata$Duration),max(salesdata$Duration))),
                     checkboxGroupInput(inputId = "comparison_gender_2",
                                        label = "Gender",
                                        choices = unique(salesdata$Gender),
                                        # selected = unique(salesdata$Gender),
                                        selected = unique(salesdata$Gender),
                                        inline = TRUE),
                     checkboxGroupInput(inputId = "comparison_smoker_2",
                                        label = "Smoker Status",
                                        choices = unique(salesdata$`Smoker Status`),
                                        # selected = unique(salesdata$`Smoker Status`),
                                        selected = "NonSmoker",
                                        inline = TRUE),
                     checkboxGroupInput(inputId = "comparison_selectult_2",
                                        label = "Select/Ultimate Indicator",
                                        choices = unique(salesdata$Select_Ultimate_Indicator),
                                        # selected = unique(salesdata$Select_Ultimate_Indicator),
                                        selected = unique(salesdata$Select_Ultimate_Indicator),
                                        inline = TRUE)
                   )
  )
)
body <- dashboardBody(
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"
  ),
  tags$head(tags$style("#comparison_issueage_1{color: black;
                                 # font-size: 20px;
                                 # font-style: italic;
                       }
                       #comparison_attainedage_1{color: black; }
                       #comparison_duration_1{color: black; }
                       #comparison_gender_1{color: black; }
                       #comparison_smoker_1{color: black; }
                       #comparison_selectult_1{color: black; }
                       
                       #comparison_issueage_2{color: black;
                       # font-size: 20px;
                       # font-style: italic;
                       }
                       #comparison_attainedage_2{color: black; }
                       #comparison_duration_2{color: black; }
                       #comparison_gender_2{color: black; }
                       #comparison_smoker_2{color: black; }
                       #comparison_selectult_2{color: black; }
                       "
  )
  ),
  tags$head(tags$style(HTML('
                            /* logo */
                            .skin-blue .main-header .logo { 
                            background-color: #0077C8;
                            }
                            
                            /* logo on hover */
                            .skin-blue .main-header .logo:hover { 
                            background-color: #0077C8;
                            }
                            
                            /* rest of the header */
                            .skin-blue .main-header .navbar { 
                            background-color: #0077C8;
                            }
                            
                            /* main sidebar */
                            .skin-blue .main-sidebar {
                            background-color: #002D72;
                            }
                            
                            /* backgrounds of all tabs */
                            .content-wrapper {
                            background-color: #ffffff !important;
                            }       

                            /* toggle when hovered */
                            .skin-blue .main-header .navbar .sidebar-toggle:hover{
                            background-color: #002D72; 
                            }
                            .checkbox-inline {
                                        margin-left: 0px;
                                        margin-right: 10px;
                              }
                            .checkbox-inline+.checkbox-inline {
                                        margin-left: 0px;
                                        margin-right: 10px;
                              }
                            '))),
  
  tabItems(
    ### Home Tab
    tabItem(tabName = "home",
            img(src="soa_logo.png", style="display: block; margin-left: auto; margin-right: auto;", width = "75%"),#, align = "right", width = "50%"), 
            h3("The Society of Actuaries Data Analysis Contest Dashboard is a one-stop shop for all comparative analyses of the provided SOA Contest dataset. Information is presented using dynamic and interactive graphs."),
            tags$hr(style="border-color: blue;"),
            h4("Groupings of analyses are shown on the left collapsible toolbar. All graphs can be downloaded as .png files."),
            h4(strong("PLEASE NOTE:"),"This app may time-out if left idle too long, which will cause the screen to grey-out. To use the app again, refresh the page. This will reset all previously-selected input options.")
    ),
    ### Summary Tab
    tabItem(tabName = "summary",
            fluidRow(
              box(
                title = "Exposed Policies and Amounts by Attained Age", 
                status = "primary", solidHeader = TRUE,
                width = 12,
                collapsible = TRUE,
                plotlyOutput("age1_plot"),
                radioButtons("pType_age1", "", list("Exposed Policies", "Exposed Amounts"))
              )
            ),
            fluidRow(
              box(
                title = "Exposed Policies and Amounts by Policy Duration", 
                status = "primary", solidHeader = TRUE,
                width = 12,
                collapsible = TRUE,
                plotlyOutput("duration1_plot"),
                radioButtons("pType_duration1", "", list("Exposed Policies", "Exposed Amounts"))             
              )
            ),
            fluidRow(
              box(
                title = "Actual/Expected Death Policies and Amounts by Attained Age", 
                status = "primary", solidHeader = TRUE,
                width = 12,
                collapsible = TRUE,
                plotlyOutput("age2_plot"),
                radioButtons("pType_age2", "", list("A/E Policies", "A/E Exposures"))
              )
            ),
            fluidRow(
              box(
                title = "Actual/Expected Death Policies and Amounts by Policy Duration", 
                status = "primary", solidHeader = TRUE,
                width = 12,
                collapsible = TRUE,
                plotlyOutput("duration2_plot"),
                radioButtons("pType_duration2", "", list("A/E Policies", "A/E Exposures"))
              )
            ),
            fluidRow(
              box(title = "A/E by Age and Mortality Table",
                  status = "primary", solidHeader = TRUE,
                  width = 12,
                  collapsible = TRUE,
                  tabBox(
                    title = "",
                    id = "tabset1", 
                    # height = "250px",
                    width = 12,
                    tabPanel("QX7580E", plotlyOutput("QX7580E_ageplot")),
                    tabPanel("QX2001VBT", plotlyOutput("QX2001VBT_ageplot")),
                    tabPanel("QX2008VBT", plotlyOutput("QX2008VBT_ageplot")),
                    tabPanel("QX2008VBTLU", plotlyOutput("QX2008VBTLU_ageplot")),
                    tabPanel("QX2015VBT", plotlyOutput("QX2015VBT_ageplot"))
                  )
              )
            ),
            fluidRow(
              box(title = "A/E by Duration and Mortality Table",
                  status = "primary", solidHeader = TRUE,
                  width = 12,
                  collapsible = TRUE,
                  tabBox(
                    title = "",
                    id = "tabset2", 
                    # height = "250px",
                    width = 12,
                    tabPanel("QX7580E", plotlyOutput("QX7580E_durplot")),
                    tabPanel("QX2001VBT", plotlyOutput("QX2001VBT_durplot")),
                    tabPanel("QX2008VBT", plotlyOutput("QX2008VBT_durplot")),
                    tabPanel("QX2008VBTLU", plotlyOutput("QX2008VBTLU_durplot")),
                    tabPanel("QX2015VBT", plotlyOutput("QX2015VBT_durplot"))
                  )
              )
            ),
            fluidRow(
              box(
                title = "Additional Factor Selections",
                status = "warning", solidHeader = TRUE,
                width = 12,
                collapsible = TRUE,
                collapsed = TRUE,
                # selectizeInput(inputId = "summary_insuranceplan",
                #                label = "Insurance Plan",
                #                choices = sort(unique(salesdata$`Insurance Plan`)),
                #                selected = sort(unique(salesdata$`Insurance Plan`)),
                #                multiple = TRUE),
                selectizeInput(inputId = "summary_faceamtband",
                               label = "Face Amount Band",
                               choices = sort(unique(salesdata$`Face Amount Band`)),
                               selected = sort(unique(salesdata$`Face Amount Band`)),
                               multiple = TRUE),
                selectizeInput(inputId = "summary_soaanticipated",
                               label = "SOA Anticipated Level Term Period",
                               choices = sort(unique(salesdata$`SOA Anticipated Level Term Period`)),
                               selected = sort(unique(salesdata$`SOA Anticipated Level Term Period`)),
                               multiple = TRUE),
                selectizeInput(inputId = "summary_soaguaranteed",
                               label = "SOA Guaranteed Level Term Period",
                               choices = sort(unique(salesdata$`SOA Guaranteed Level Term Period`)),
                               selected = sort(unique(salesdata$`SOA Guaranteed Level Term Period`)),
                               multiple = TRUE),
                selectizeInput(inputId = "summary_soapostlevel",
                               label = "SOA Post Level Term Indicator",
                               choices = sort(unique(salesdata$`SOA Post level term indicator`)),
                               selected = sort(unique(salesdata$`SOA Post level term indicator`)),
                               multiple = TRUE)
              )
            ),
            fluidRow(
              box(
                title = "Additional Numeric Selections",
                status = "warning", solidHeader = TRUE,
                width = 12,
                collapsible = TRUE,
                collapsed = TRUE,
                sliderInput(inputId = "summary_agebasis",
                            label = "Age Basis",
                            min = min(salesdata$`Age Basis`), max = max(salesdata$`Age Basis`),
                            value = c(min(salesdata$`Age Basis`),max(salesdata$`Age Basis`))),
                # sliderInput(inputId = "summary_numprefer",
                #             label = "Number of Preferred Classes",
                #             min = min(salesdata$`Number of Preferred Classes`), max = max(salesdata$`Number of Preferred Classes`),
                #             value = c(min(salesdata$`Number of Preferred Classes`),max(salesdata$`Number of Preferred Classes`))),
                # sliderInput(inputId = "summary_prefer",
                #             label = "Preferred Class",
                #             min = min(salesdata$`Preferred Class`), max = max(salesdata$`Preferred Class`),
                # value = c(min(salesdata$`Preferred Class`),max(salesdata$`Preferred Class`))),
                sliderInput(inputId = "summary_deaths", 
                            label = "Number of Deaths",
                            min = min(salesdata$`Number of Deaths`), max = max(salesdata$`Number of Deaths`),
                            value = c(min(salesdata$`Number of Deaths`),max(salesdata$`Number of Deaths`))),
                # value = c(264,max(salesdata$`Number of Deaths`))),
                sliderInput(inputId = "summary_deathclaim", 
                            label = "Death Claim Amount",
                            min = min(salesdata$`Death Claim Amount`), max = max(salesdata$`Death Claim Amount`),
                            value = c(min(salesdata$`Death Claim Amount`),max(salesdata$`Death Claim Amount`))),
                # value = c(100000,max(salesdata$`Death Claim Amount`))),
                sliderInput(inputId = "summary_policies", 
                            label = "Policies Exposed",
                            min = min(salesdata$`Policies Exposed`), max = max(salesdata$`Policies Exposed`),
                            value = c(min(salesdata$`Policies Exposed`),max(salesdata$`Policies Exposed`))),
                # value = c(10000,max(salesdata$`Policies Exposed`))),
                sliderInput(inputId = "summary_amount", 
                            label = "Amount Exposed",
                            min = min(salesdata$`Amount Exposed`), max = max(salesdata$`Amount Exposed`),
                            # value = c(min(salesdata$`Amount Exposed`),max(salesdata$`Amount Exposed`))),
                            value = c(1750000,max(salesdata$`Amount Exposed`))),
                sliderInput(inputId = "summary_eda1", 
                            label = "Expected Death QX7580E by Amount",
                            min = min(salesdata$`Expected Death QX7580E by Amount`), max = max(salesdata$`Expected Death QX7580E by Amount`),
                            # value = c(min(salesdata$`Expected Death QX7580E by Amount`),max(salesdata$`Expected Death QX7580E by Amount`))),
                            value = c(3000,max(salesdata$`Expected Death QX7580E by Amount`))),
                sliderInput(inputId = "summary_eda2", 
                            label = "Expected Death QX2001VBT by Amount",
                            min = min(salesdata$`Expected Death QX2001VBT by Amount`), max = max(salesdata$`Expected Death QX2001VBT by Amount`),
                            # value = c(min(salesdata$`Expected Death QX2001VBT by Amount`),max(salesdata$`Expected Death QX2001VBT by Amount`))),
                            value = c(3000,max(salesdata$`Expected Death QX2001VBT by Amount`))),
                sliderInput(inputId = "summary_eda3", 
                            label = "Expected Death QX2008VBT by Amount",
                            min = min(salesdata$`Expected Death QX2008VBT by Amount`), max = max(salesdata$`Expected Death QX2008VBT by Amount`),
                            # value = c(min(salesdata$`Expected Death QX2008VBT by Amount`),max(salesdata$`Expected Death QX2008VBT by Amount`))),
                            value = c(3000,max(salesdata$`Expected Death QX2008VBT by Amount`))),
                sliderInput(inputId = "summary_eda4", 
                            label = "Expected Death QX2008VBTLU by Amount",
                            min = min(salesdata$`Expected Death QX2008VBTLU by Amount`), max = max(salesdata$`Expected Death QX2008VBTLU by Amount`),
                            # value = c(min(salesdata$`Expected Death QX2008VBTLU by Amount`),max(salesdata$`Expected Death QX2008VBTLU by Amount`))),
                            value = c(3000,max(salesdata$`Expected Death QX2008VBTLU by Amount`))),
                sliderInput(inputId = "summary_eda5", 
                            label = "Expected Death QX2015VBT by Amount",
                            min = min(salesdata$`Expected Death QX2015VBT by Amount`), max = max(salesdata$`Expected Death QX2015VBT by Amount`),
                            # value = c(min(salesdata$`Expected Death QX2015VBT by Amount`),max(salesdata$`Expected Death QX2015VBT by Amount`))),
                            value = c(3000,max(salesdata$`Expected Death QX2015VBT by Amount`))),
                sliderInput(inputId = "summary_edp1", 
                            label = "Expected Death QX7580E by Policy",
                            min = min(salesdata$`Expected Death QX7580E by Policy`), max = max(salesdata$`Expected Death QX7580E by Policy`),
                            value = c(min(salesdata$`Expected Death QX7580E by Policy`),max(salesdata$`Expected Death QX7580E by Policy`))),
                # value = c(250,max(salesdata$`Expected Death QX7580E by Policy`))),
                sliderInput(inputId = "summary_edp2", 
                            label = "Expected Death QX2001VBT by Policy",
                            min = min(salesdata$`Expected Death QX2001VBT by Policy`), max = max(salesdata$`Expected Death QX2001VBT by Policy`),
                            value = c(min(salesdata$`Expected Death QX2001VBT by Policy`),max(salesdata$`Expected Death QX2001VBT by Policy`))),
                # value = c(250,max(salesdata$`Expected Death QX2001VBT by Policy`))),
                sliderInput(inputId = "summary_edp3", 
                            label = "Expected Death QX2008VBT by Policy",
                            min = min(salesdata$`Expected Death QX2008VBT by Policy`), max = max(salesdata$`Expected Death QX2008VBT by Policy`),
                            value = c(min(salesdata$`Expected Death QX2008VBT by Policy`),max(salesdata$`Expected Death QX2008VBT by Policy`))),
                # value = c(250,max(salesdata$`Expected Death QX2008VBT by Policy`))),
                sliderInput(inputId = "summary_edp4", 
                            label = "Expected Death QX2008VBTLU by Policy",
                            min = min(salesdata$`Expected Death QX2008VBTLU by Policy`), max = max(salesdata$`Expected Death QX2008VBTLU by Policy`),
                            value = c(min(salesdata$`Expected Death QX2008VBTLU by Policy`),max(salesdata$`Expected Death QX2008VBTLU by Policy`))),
                # value = c(250,max(salesdata$`Expected Death QX2008VBTLU by Policy`))),
                sliderInput(inputId = "summary_edp5", 
                            label = "Expected Death QX2015VBT by Policy",
                            min = min(salesdata$`Expected Death QX2015VBT by Policy`), max = max(salesdata$`Expected Death QX2015VBT by Policy`),
                            value = c(min(salesdata$`Expected Death QX2015VBT by Policy`),max(salesdata$`Expected Death QX2015VBT by Policy`)))
                # value = c(250,max(salesdata$`Expected Death QX2015VBT by Policy`)))
                
              )
            )
    ),
    ### Comparison Tab
    tabItem(tabName = "comparison",
            fluidRow(
              box(title = "Credibility-Weighted A/E by Age and Mortality Table",
                  status = "primary", solidHeader = TRUE,
                  width = 12,
                  collapsible = TRUE,
                  tabBox(
                    title = "",
                    id = "tabset11", 
                    # height = "250px",
                    width = 12,
                    tabPanel("QX7580E", plotlyOutput("comparison_QX7580E_ageplot")),
                    tabPanel("QX2001VBT", plotlyOutput("comparison_QX2001VBT_ageplot")),
                    tabPanel("QX2008VBT", plotlyOutput("comparison_QX2008VBT_ageplot")),
                    tabPanel("QX2008VBTLU", plotlyOutput("comparison_QX2008VBTLU_ageplot")),
                    tabPanel("QX2015VBT", plotlyOutput("comparison_QX2015VBT_ageplot"))
                  )
              )
            ),
            fluidRow(
              box(title = "Credibility-Weighted A/E by Duration and Mortality Table",
                  status = "primary", solidHeader = TRUE,
                  width = 12,
                  collapsible = TRUE,
                  tabBox(
                    title = "",
                    id = "tabset21", 
                    # height = "250px",
                    width = 12,
                    tabPanel("QX7580E", plotlyOutput("comparison_QX7580E_durplot")),
                    tabPanel("QX2001VBT", plotlyOutput("comparison_QX2001VBT_durplot")),
                    tabPanel("QX2008VBT", plotlyOutput("comparison_QX2008VBT_durplot")),
                    tabPanel("QX2008VBTLU", plotlyOutput("comparison_QX2008VBTLU_durplot")),
                    tabPanel("QX2015VBT", plotlyOutput("comparison_QX2015VBT_durplot"))
                  )
              )
            ),
            #### Comparison Option 1
            fluidRow(
              box(
                title = "Comparison Option 1: Additional Factor Selections",
                status = "warning", solidHeader = TRUE,
                width = 12,
                collapsible = TRUE,
                collapsed = TRUE,
                # selectizeInput(inputId = "summary_insuranceplan",
                #                label = "Insurance Plan",
                #                choices = sort(unique(salesdata$`Insurance Plan`)),
                #                selected = sort(unique(salesdata$`Insurance Plan`)),
                #                multiple = TRUE),
                selectizeInput(inputId = "comparison_faceamtband_1",
                               label = "Face Amount Band",
                               choices = sort(unique(salesdata$`Face Amount Band`)),
                               selected = sort(unique(salesdata$`Face Amount Band`)),
                               multiple = TRUE),
                selectizeInput(inputId = "comparison_soaanticipated_1",
                               label = "SOA Anticipated Level Term Period",
                               choices = sort(unique(salesdata$`SOA Anticipated Level Term Period`)),
                               selected = sort(unique(salesdata$`SOA Anticipated Level Term Period`)),
                               multiple = TRUE),
                selectizeInput(inputId = "comparison_soaguaranteed_1",
                               label = "SOA Guaranteed Level Term Period",
                               choices = sort(unique(salesdata$`SOA Guaranteed Level Term Period`)),
                               selected = sort(unique(salesdata$`SOA Guaranteed Level Term Period`)),
                               multiple = TRUE),
                selectizeInput(inputId = "comparison_soapostlevel_1",
                               label = "SOA Post Level Term Indicator",
                               choices = sort(unique(salesdata$`SOA Post level term indicator`)),
                               selected = sort(unique(salesdata$`SOA Post level term indicator`)),
                               multiple = TRUE)
              )
            ),
            fluidRow(
              box(
                title = "Comparison Option 1: Additional Numeric Selections",
                status = "warning", solidHeader = TRUE,
                width = 12,
                collapsible = TRUE,
                collapsed = TRUE,
                sliderInput(inputId = "comparison_agebasis_1",
                            label = "Age Basis",
                            min = min(salesdata$`Age Basis`), max = max(salesdata$`Age Basis`),
                            value = c(min(salesdata$`Age Basis`),max(salesdata$`Age Basis`))),
                # sliderInput(inputId = "summary_numprefer",
                #             label = "Number of Preferred Classes",
                #             min = min(salesdata$`Number of Preferred Classes`), max = max(salesdata$`Number of Preferred Classes`),
                #             value = c(min(salesdata$`Number of Preferred Classes`),max(salesdata$`Number of Preferred Classes`))),
                # sliderInput(inputId = "summary_prefer",
                #             label = "Preferred Class",
                #             min = min(salesdata$`Preferred Class`), max = max(salesdata$`Preferred Class`),
                # value = c(min(salesdata$`Preferred Class`),max(salesdata$`Preferred Class`))),
                sliderInput(inputId = "comparison_deaths_1", 
                            label = "Number of Deaths",
                            min = min(salesdata$`Number of Deaths`), max = max(salesdata$`Number of Deaths`),
                            value = c(min(salesdata$`Number of Deaths`),max(salesdata$`Number of Deaths`))),
                # value = c(264,max(salesdata$`Number of Deaths`))),
                sliderInput(inputId = "comparison_deathclaim_1", 
                            label = "Death Claim Amount",
                            min = min(salesdata$`Death Claim Amount`), max = max(salesdata$`Death Claim Amount`),
                            value = c(min(salesdata$`Death Claim Amount`),max(salesdata$`Death Claim Amount`))),
                # value = c(100000,max(salesdata$`Death Claim Amount`))),
                sliderInput(inputId = "comparison_policies_1", 
                            label = "Policies Exposed",
                            min = min(salesdata$`Policies Exposed`), max = max(salesdata$`Policies Exposed`),
                            value = c(min(salesdata$`Policies Exposed`),max(salesdata$`Policies Exposed`))),
                # value = c(10000,max(salesdata$`Policies Exposed`))),
                sliderInput(inputId = "comparison_amount_1", 
                            label = "Amount Exposed",
                            min = min(salesdata$`Amount Exposed`), max = max(salesdata$`Amount Exposed`),
                            # value = c(min(salesdata$`Amount Exposed`),max(salesdata$`Amount Exposed`))),
                            value = c(1750000,max(salesdata$`Amount Exposed`))),
                sliderInput(inputId = "comparison_eda1_1", 
                            label = "Expected Death QX7580E by Amount",
                            min = min(salesdata$`Expected Death QX7580E by Amount`), max = max(salesdata$`Expected Death QX7580E by Amount`),
                            # value = c(min(salesdata$`Expected Death QX7580E by Amount`),max(salesdata$`Expected Death QX7580E by Amount`))),
                            value = c(3000,max(salesdata$`Expected Death QX7580E by Amount`))),
                sliderInput(inputId = "comparison_eda2_1", 
                            label = "Expected Death QX2001VBT by Amount",
                            min = min(salesdata$`Expected Death QX2001VBT by Amount`), max = max(salesdata$`Expected Death QX2001VBT by Amount`),
                            # value = c(min(salesdata$`Expected Death QX2001VBT by Amount`),max(salesdata$`Expected Death QX2001VBT by Amount`))),
                            value = c(3000,max(salesdata$`Expected Death QX2001VBT by Amount`))),
                sliderInput(inputId = "comparison_eda3_1", 
                            label = "Expected Death QX2008VBT by Amount",
                            min = min(salesdata$`Expected Death QX2008VBT by Amount`), max = max(salesdata$`Expected Death QX2008VBT by Amount`),
                            # value = c(min(salesdata$`Expected Death QX2008VBT by Amount`),max(salesdata$`Expected Death QX2008VBT by Amount`))),
                            value = c(3000,max(salesdata$`Expected Death QX2008VBT by Amount`))),
                sliderInput(inputId = "comparison_eda4_1", 
                            label = "Expected Death QX2008VBTLU by Amount",
                            min = min(salesdata$`Expected Death QX2008VBTLU by Amount`), max = max(salesdata$`Expected Death QX2008VBTLU by Amount`),
                            # value = c(min(salesdata$`Expected Death QX2008VBTLU by Amount`),max(salesdata$`Expected Death QX2008VBTLU by Amount`))),
                            value = c(3000,max(salesdata$`Expected Death QX2008VBTLU by Amount`))),
                sliderInput(inputId = "comparison_eda5_1", 
                            label = "Expected Death QX2015VBT by Amount",
                            min = min(salesdata$`Expected Death QX2015VBT by Amount`), max = max(salesdata$`Expected Death QX2015VBT by Amount`),
                            # value = c(min(salesdata$`Expected Death QX2015VBT by Amount`),max(salesdata$`Expected Death QX2015VBT by Amount`))),
                            value = c(3000,max(salesdata$`Expected Death QX2015VBT by Amount`))),
                sliderInput(inputId = "comparison_edp1_1", 
                            label = "Expected Death QX7580E by Policy",
                            min = min(salesdata$`Expected Death QX7580E by Policy`), max = max(salesdata$`Expected Death QX7580E by Policy`),
                            value = c(min(salesdata$`Expected Death QX7580E by Policy`),max(salesdata$`Expected Death QX7580E by Policy`))),
                # value = c(250,max(salesdata$`Expected Death QX7580E by Policy`))),
                sliderInput(inputId = "comparison_edp2_1", 
                            label = "Expected Death QX2001VBT by Policy",
                            min = min(salesdata$`Expected Death QX2001VBT by Policy`), max = max(salesdata$`Expected Death QX2001VBT by Policy`),
                            value = c(min(salesdata$`Expected Death QX2001VBT by Policy`),max(salesdata$`Expected Death QX2001VBT by Policy`))),
                # value = c(250,max(salesdata$`Expected Death QX2001VBT by Policy`))),
                sliderInput(inputId = "comparison_edp3_1", 
                            label = "Expected Death QX2008VBT by Policy",
                            min = min(salesdata$`Expected Death QX2008VBT by Policy`), max = max(salesdata$`Expected Death QX2008VBT by Policy`),
                            value = c(min(salesdata$`Expected Death QX2008VBT by Policy`),max(salesdata$`Expected Death QX2008VBT by Policy`))),
                # value = c(250,max(salesdata$`Expected Death QX2008VBT by Policy`))),
                sliderInput(inputId = "comparison_edp4_1", 
                            label = "Expected Death QX2008VBTLU by Policy",
                            min = min(salesdata$`Expected Death QX2008VBTLU by Policy`), max = max(salesdata$`Expected Death QX2008VBTLU by Policy`),
                            value = c(min(salesdata$`Expected Death QX2008VBTLU by Policy`),max(salesdata$`Expected Death QX2008VBTLU by Policy`))),
                # value = c(250,max(salesdata$`Expected Death QX2008VBTLU by Policy`))),
                sliderInput(inputId = "comparison_edp5_1", 
                            label = "Expected Death QX2015VBT by Policy",
                            min = min(salesdata$`Expected Death QX2015VBT by Policy`), max = max(salesdata$`Expected Death QX2015VBT by Policy`),
                            value = c(min(salesdata$`Expected Death QX2015VBT by Policy`),max(salesdata$`Expected Death QX2015VBT by Policy`)))
                # value = c(250,max(salesdata$`Expected Death QX2015VBT by Policy`)))
                
              )
            ),
            #### Comparison Option 2
            fluidRow(
              box(
                title = "Comparison Option 2: Additional Factor Selections",
                status = "warning", solidHeader = TRUE,
                width = 12,
                collapsible = TRUE,
                collapsed = TRUE,
                # selectizeInput(inputId = "summary_insuranceplan",
                #                label = "Insurance Plan",
                #                choices = sort(unique(salesdata$`Insurance Plan`)),
                #                selected = sort(unique(salesdata$`Insurance Plan`)),
                #                multiple = TRUE),
                selectizeInput(inputId = "comparison_faceamtband_2",
                               label = "Face Amount Band",
                               choices = sort(unique(salesdata$`Face Amount Band`)),
                               selected = sort(unique(salesdata$`Face Amount Band`)),
                               multiple = TRUE),
                selectizeInput(inputId = "comparison_soaanticipated_2",
                               label = "SOA Anticipated Level Term Period",
                               choices = sort(unique(salesdata$`SOA Anticipated Level Term Period`)),
                               selected = sort(unique(salesdata$`SOA Anticipated Level Term Period`)),
                               multiple = TRUE),
                selectizeInput(inputId = "comparison_soaguaranteed_2",
                               label = "SOA Guaranteed Level Term Period",
                               choices = sort(unique(salesdata$`SOA Guaranteed Level Term Period`)),
                               selected = sort(unique(salesdata$`SOA Guaranteed Level Term Period`)),
                               multiple = TRUE),
                selectizeInput(inputId = "comparison_soapostlevel_2",
                               label = "SOA Post Level Term Indicator",
                               choices = sort(unique(salesdata$`SOA Post level term indicator`)),
                               selected = sort(unique(salesdata$`SOA Post level term indicator`)),
                               multiple = TRUE)
              )
            ),
            fluidRow(
              box(
                title = "Comparison Option 2: Additional Numeric Selections",
                status = "warning", solidHeader = TRUE,
                width = 12,
                collapsible = TRUE,
                collapsed = TRUE,
                sliderInput(inputId = "comparison_agebasis_2",
                            label = "Age Basis",
                            min = min(salesdata$`Age Basis`), max = max(salesdata$`Age Basis`),
                            value = c(min(salesdata$`Age Basis`),max(salesdata$`Age Basis`))),
                # sliderInput(inputId = "summary_numprefer",
                #             label = "Number of Preferred Classes",
                #             min = min(salesdata$`Number of Preferred Classes`), max = max(salesdata$`Number of Preferred Classes`),
                #             value = c(min(salesdata$`Number of Preferred Classes`),max(salesdata$`Number of Preferred Classes`))),
                # sliderInput(inputId = "summary_prefer",
                #             label = "Preferred Class",
                #             min = min(salesdata$`Preferred Class`), max = max(salesdata$`Preferred Class`),
                # value = c(min(salesdata$`Preferred Class`),max(salesdata$`Preferred Class`))),
                sliderInput(inputId = "comparison_deaths_2", 
                            label = "Number of Deaths",
                            min = min(salesdata$`Number of Deaths`), max = max(salesdata$`Number of Deaths`),
                            value = c(min(salesdata$`Number of Deaths`),max(salesdata$`Number of Deaths`))),
                # value = c(264,max(salesdata$`Number of Deaths`))),
                sliderInput(inputId = "comparison_deathclaim_2", 
                            label = "Death Claim Amount",
                            min = min(salesdata$`Death Claim Amount`), max = max(salesdata$`Death Claim Amount`),
                            value = c(min(salesdata$`Death Claim Amount`),max(salesdata$`Death Claim Amount`))),
                # value = c(100000,max(salesdata$`Death Claim Amount`))),
                sliderInput(inputId = "comparison_policies_2", 
                            label = "Policies Exposed",
                            min = min(salesdata$`Policies Exposed`), max = max(salesdata$`Policies Exposed`),
                            value = c(min(salesdata$`Policies Exposed`),max(salesdata$`Policies Exposed`))),
                # value = c(10000,max(salesdata$`Policies Exposed`))),
                sliderInput(inputId = "comparison_amount_2", 
                            label = "Amount Exposed",
                            min = min(salesdata$`Amount Exposed`), max = max(salesdata$`Amount Exposed`),
                            # value = c(min(salesdata$`Amount Exposed`),max(salesdata$`Amount Exposed`))),
                            value = c(1750000,max(salesdata$`Amount Exposed`))),
                sliderInput(inputId = "comparison_eda1_2", 
                            label = "Expected Death QX7580E by Amount",
                            min = min(salesdata$`Expected Death QX7580E by Amount`), max = max(salesdata$`Expected Death QX7580E by Amount`),
                            # value = c(min(salesdata$`Expected Death QX7580E by Amount`),max(salesdata$`Expected Death QX7580E by Amount`))),
                            value = c(3000,max(salesdata$`Expected Death QX7580E by Amount`))),
                sliderInput(inputId = "comparison_eda2_2", 
                            label = "Expected Death QX2001VBT by Amount",
                            min = min(salesdata$`Expected Death QX2001VBT by Amount`), max = max(salesdata$`Expected Death QX2001VBT by Amount`),
                            # value = c(min(salesdata$`Expected Death QX2001VBT by Amount`),max(salesdata$`Expected Death QX2001VBT by Amount`))),
                            value = c(3000,max(salesdata$`Expected Death QX2001VBT by Amount`))),
                sliderInput(inputId = "comparison_eda3_2", 
                            label = "Expected Death QX2008VBT by Amount",
                            min = min(salesdata$`Expected Death QX2008VBT by Amount`), max = max(salesdata$`Expected Death QX2008VBT by Amount`),
                            # value = c(min(salesdata$`Expected Death QX2008VBT by Amount`),max(salesdata$`Expected Death QX2008VBT by Amount`))),
                            value = c(3000,max(salesdata$`Expected Death QX2008VBT by Amount`))),
                sliderInput(inputId = "comparison_eda4_2", 
                            label = "Expected Death QX2008VBTLU by Amount",
                            min = min(salesdata$`Expected Death QX2008VBTLU by Amount`), max = max(salesdata$`Expected Death QX2008VBTLU by Amount`),
                            # value = c(min(salesdata$`Expected Death QX2008VBTLU by Amount`),max(salesdata$`Expected Death QX2008VBTLU by Amount`))),
                            value = c(3000,max(salesdata$`Expected Death QX2008VBTLU by Amount`))),
                sliderInput(inputId = "comparison_eda5_2", 
                            label = "Expected Death QX2015VBT by Amount",
                            min = min(salesdata$`Expected Death QX2015VBT by Amount`), max = max(salesdata$`Expected Death QX2015VBT by Amount`),
                            # value = c(min(salesdata$`Expected Death QX2015VBT by Amount`),max(salesdata$`Expected Death QX2015VBT by Amount`))),
                            value = c(3000,max(salesdata$`Expected Death QX2015VBT by Amount`))),
                sliderInput(inputId = "comparison_edp1_2", 
                            label = "Expected Death QX7580E by Policy",
                            min = min(salesdata$`Expected Death QX7580E by Policy`), max = max(salesdata$`Expected Death QX7580E by Policy`),
                            value = c(min(salesdata$`Expected Death QX7580E by Policy`),max(salesdata$`Expected Death QX7580E by Policy`))),
                # value = c(250,max(salesdata$`Expected Death QX7580E by Policy`))),
                sliderInput(inputId = "comparison_edp2_2", 
                            label = "Expected Death QX2001VBT by Policy",
                            min = min(salesdata$`Expected Death QX2001VBT by Policy`), max = max(salesdata$`Expected Death QX2001VBT by Policy`),
                            value = c(min(salesdata$`Expected Death QX2001VBT by Policy`),max(salesdata$`Expected Death QX2001VBT by Policy`))),
                # value = c(250,max(salesdata$`Expected Death QX2001VBT by Policy`))),
                sliderInput(inputId = "comparison_edp3_2", 
                            label = "Expected Death QX2008VBT by Policy",
                            min = min(salesdata$`Expected Death QX2008VBT by Policy`), max = max(salesdata$`Expected Death QX2008VBT by Policy`),
                            value = c(min(salesdata$`Expected Death QX2008VBT by Policy`),max(salesdata$`Expected Death QX2008VBT by Policy`))),
                # value = c(250,max(salesdata$`Expected Death QX2008VBT by Policy`))),
                sliderInput(inputId = "comparison_edp4_2", 
                            label = "Expected Death QX2008VBTLU by Policy",
                            min = min(salesdata$`Expected Death QX2008VBTLU by Policy`), max = max(salesdata$`Expected Death QX2008VBTLU by Policy`),
                            value = c(min(salesdata$`Expected Death QX2008VBTLU by Policy`),max(salesdata$`Expected Death QX2008VBTLU by Policy`))),
                # value = c(250,max(salesdata$`Expected Death QX2008VBTLU by Policy`))),
                sliderInput(inputId = "comparison_edp5_2", 
                            label = "Expected Death QX2015VBT by Policy",
                            min = min(salesdata$`Expected Death QX2015VBT by Policy`), max = max(salesdata$`Expected Death QX2015VBT by Policy`),
                            value = c(min(salesdata$`Expected Death QX2015VBT by Policy`),max(salesdata$`Expected Death QX2015VBT by Policy`)))
                # value = c(250,max(salesdata$`Expected Death QX2015VBT by Policy`)))
                
              )
            )
    ),
    ### App Metrics Tab
    tabItem(tabName = "app-metrics",
            h5(strong("Session Information:")),
            verbatimTextOutput("sessionInfo")
    ),
    ### Help Tab
    tabItem(tabName = "help",
            h3("Have any feedback?"),
            h4("Please send any feedback on the SOA Data Analysis Contest Dashboard to:"),
            h5(tags$ul(tags$li(a("Alyssa Columbus", href = "mailto:alyssa.columbus@pacificlife.com", target = "_blank")))),
            h5(tags$ul(tags$li(a("Tommy Steed", href = "mailto:THsteed@pacificlife.com", target = "_blank")))),
            
            h4("Please mention the following in the subject:"),
            h5(tags$ul(tags$li("The name of the app (SOA Data Analysis Contest Dashboard)"))),
            h5(tags$ul(tags$li("Which tab/sub-tab you are referring (if applicable)"))),
            h5(tags$ul(tags$li("e.g. SOA Data Analysis Contest Dashboard: Summary"))),
            h5("Please include details of feedback in the body of the email."),
            img(src="RSD.png", width = "100%")#, align = "right", width = "50%")  
    )
  )
)
dashboardPage(title = "Pacific Life SOA Data Analysis Contest Dashboard",
              dashboardHeader(title = span(img(src="Pacific_Life.png", width = 190, height = 90))),
              sidebar,
              body
)