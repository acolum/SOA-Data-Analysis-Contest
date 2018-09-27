shinyServer(function(input, output, session) {
  #### Reactive Dataset: Summary ####
  salesdata_reactive <- reactive({
    salesdata %>%
      filter(`Issue Year` >= input$summary_issueyear[1] &
               `Issue Year` <= input$summary_issueyear[2]) %>%
      filter(`Observation Year` >= input$summary_obsyear[1] &
               `Observation Year` <= input$summary_obsyear[2]) %>% 
      filter(`Issue Age` >= input$summary_issueage[1] &
               `Issue Age` <= input$summary_issueage[2]) %>% 
      filter(`Attained Age` >= input$summary_attainedage[1] &
               `Attained Age` <= input$summary_attainedage[2]) %>% 
      filter(Duration >= input$summary_duration[1] &
               Duration <= input$summary_duration[2]) %>% 
      subset(Gender %in% input$summary_gender) %>% # CORRECT
      subset(`Smoker Status` %in% input$summary_smoker) %>%
      subset(Select_Ultimate_Indicator %in% input$summary_selectult) %>% 
      # subset(`Insurance Plan` %in% input$summary_insuranceplan) %>% 
      subset(`Face Amount Band` %in% input$summary_faceamtband) %>% 
      subset(`SOA Anticipated Level Term Period` %in% input$summary_soaanticipated) %>% 
      subset(`SOA Guaranteed Level Term Period` %in% input$summary_soaguaranteed) %>% 
      subset(`SOA Post level term indicator` %in% input$summary_soapostlevel) %>% 
      filter(`Age Basis` >= input$summary_agebasis[1] &
               `Age Basis` <= input$summary_agebasis[2]) %>%
      # filter(`Number of Preferred Classes` >= input$summary_numprefer[1] &
      #          `Number of Preferred Classes` <= input$summary_numprefer[2]) %>%
      # filter(`Preferred Class` >= input$summary_prefer[1] &
      #          `Preferred Class` <= input$summary_prefer[2]) %>%
      filter(`Number of Deaths` >= input$summary_deaths[1] &
               `Number of Deaths` <= input$summary_deaths[2]) %>% 
      filter(`Death Claim Amount` >= input$summary_deathclaim[1] &
               `Death Claim Amount` <= input$summary_deathclaim[2]) %>% 
      filter(`Policies Exposed` >= input$summary_policies[1] &
               `Policies Exposed` <= input$summary_policies[2]) %>% 
      filter(`Amount Exposed` >= input$summary_amount[1] &
               `Amount Exposed` <= input$summary_amount[2]) %>% 
      filter(`Expected Death QX7580E by Amount` >= input$summary_eda1[1] &
               `Expected Death QX7580E by Amount` <= input$summary_eda1[2]) %>% 
      filter(`Expected Death QX2001VBT by Amount` >= input$summary_eda2[1] &
               `Expected Death QX2001VBT by Amount` <= input$summary_eda2[2]) %>% 
      filter(`Expected Death QX2008VBT by Amount` >= input$summary_eda3[1] &
               `Expected Death QX2008VBT by Amount` <= input$summary_eda3[2]) %>% 
      filter(`Expected Death QX2008VBTLU by Amount` >= input$summary_eda4[1] &
               `Expected Death QX2008VBTLU by Amount` <= input$summary_eda4[2]) %>% 
      filter(`Expected Death QX2015VBT by Amount` >= input$summary_eda5[1] &
               `Expected Death QX2015VBT by Amount` <= input$summary_eda5[2]) %>% 
      filter(`Expected Death QX7580E by Policy` >= input$summary_edp1[1] &
               `Expected Death QX7580E by Policy` <= input$summary_edp1[2]) %>% 
      filter(`Expected Death QX2001VBT by Policy` >= input$summary_edp2[1] &
               `Expected Death QX2001VBT by Policy` <= input$summary_edp2[2]) %>% 
      filter(`Expected Death QX2008VBT by Policy` >= input$summary_edp3[1] &
               `Expected Death QX2008VBT by Policy` <= input$summary_edp3[2]) %>% 
      filter(`Expected Death QX2008VBTLU by Policy` >= input$summary_edp4[1] &
               `Expected Death QX2008VBTLU by Policy` <= input$summary_edp4[2]) %>% 
      filter(`Expected Death QX2015VBT by Policy` >= input$summary_edp5[1] &
               `Expected Death QX2015VBT by Policy` <= input$summary_edp5[2])
  })
  
  df_duration <- reactive({
    salesdata_reactive() %>% 
      group_by(Duration) %>% 
      summarise(Policies = sum(`Policies Exposed`),
                Exposure = sum(`Amount Exposed`),
                Deaths_Num = sum(`Number of Deaths`),
                Deaths_Amt = sum(`Death Claim Amount`),
                EDeaths_Num_QX2001VBT = sum(`Expected Death QX2001VBT by Policy`),
                EDeaths_Amt_QX2001VBT = sum(`Expected Death QX2001VBT by Amount`),
                EDeaths_Num_QX2008VBT = sum(`Expected Death QX2008VBT by Policy`),
                EDeaths_Amt_QX2008VBT = sum(`Expected Death QX2008VBT by Amount`),
                EDeaths_Num_QX2008VBTLU = sum(`Expected Death QX2008VBTLU by Policy`),
                EDeaths_Amt_QX2008VBTLU = sum(`Expected Death QX2008VBTLU by Amount`),
                EDeaths_Num_QX2015VBT = sum(`Expected Death QX2015VBT by Policy`),
                EDeaths_Amt_QX2015VBT = sum(`Expected Death QX2015VBT by Amount`),
                EDeaths_Num_QX7580E = sum(`Expected Death QX7580E by Policy`),
                EDeaths_Amt_QX7580E = sum(`Expected Death QX7580E by Amount`)) %>% 
      mutate(Deaths_Num_Cap = replace(Deaths_Num, Deaths_Num > 3007, 3007)) %>%
      mutate(AE_Num_QX2001VBT = Deaths_Num/EDeaths_Num_QX2001VBT,
             AE_Amt_QX2001VBT = Deaths_Amt/EDeaths_Amt_QX2001VBT,
             AE_Num_QX2008VBT = Deaths_Num/EDeaths_Num_QX2008VBT,
             AE_Amt_QX2008VBT = Deaths_Amt/EDeaths_Amt_QX2008VBT,
             AE_Num_QX2008VBTLU = Deaths_Num/EDeaths_Num_QX2008VBTLU,
             AE_Amt_QX2008VBTLU = Deaths_Amt/EDeaths_Amt_QX2008VBTLU,
             AE_Num_QX2015VBT = Deaths_Num/EDeaths_Num_QX2015VBT,
             AE_Amt_QX2015VBT = Deaths_Amt/EDeaths_Amt_QX2015VBT,
             AE_Num_QX7580E = Deaths_Num/EDeaths_Num_QX7580E,
             AE_Amt_QX7580E = Deaths_Amt/EDeaths_Amt_QX7580E) %>% 
      mutate(Cred_AE_Num_QX2001VBT = (sqrt(Deaths_Num_Cap/3007)*AE_Num_QX2001VBT) + (1-sqrt(Deaths_Num_Cap/3007)),
             Cred_AE_Amt_QX2001VBT = (sqrt(Deaths_Num_Cap/3007)*AE_Amt_QX2001VBT) + (1-sqrt(Deaths_Num_Cap/3007)),
             Cred_AE_Num_QX2008VBT = (sqrt(Deaths_Num_Cap/3007)*AE_Num_QX2008VBT) + (1-sqrt(Deaths_Num_Cap/3007)),
             Cred_AE_Amt_QX2008VBT = (sqrt(Deaths_Num_Cap/3007)*AE_Amt_QX2008VBT) + (1-sqrt(Deaths_Num_Cap/3007)),
             Cred_AE_Num_QX2008VBTLU = (sqrt(Deaths_Num_Cap/3007)*AE_Num_QX2008VBTLU) + (1-sqrt(Deaths_Num_Cap/3007)),
             Cred_AE_Amt_QX2008VBTLU = (sqrt(Deaths_Num_Cap/3007)*AE_Amt_QX2008VBTLU) + (1-sqrt(Deaths_Num_Cap/3007)),
             Cred_AE_Num_QX2015VBT = (sqrt(Deaths_Num_Cap/3007)*AE_Num_QX2015VBT) + (1-sqrt(Deaths_Num_Cap/3007)),
             Cred_AE_Amt_QX2015VBT = (sqrt(Deaths_Num_Cap/3007)*AE_Amt_QX2015VBT) + (1-sqrt(Deaths_Num_Cap/3007)),
             Cred_AE_Num_QX7580E = (sqrt(Deaths_Num_Cap/3007)*AE_Num_QX7580E) + (1-sqrt(Deaths_Num_Cap/3007)),
             Cred_AE_Amt_QX7580E = (sqrt(Deaths_Num_Cap/3007)*AE_Amt_QX7580E) + (1-sqrt(Deaths_Num_Cap/3007)))
  })
  
  
  df_age <- reactive({
    salesdata_reactive() %>% 
      group_by(`Attained Age`) %>% 
      summarise(Policies = sum(`Policies Exposed`),
                Exposure = sum(`Amount Exposed`),
                Deaths_Num = sum(`Number of Deaths`),
                Deaths_Amt = sum(`Death Claim Amount`),
                EDeaths_Num_QX2001VBT = sum(`Expected Death QX2001VBT by Policy`),
                EDeaths_Amt_QX2001VBT = sum(`Expected Death QX2001VBT by Amount`),
                EDeaths_Num_QX2008VBT = sum(`Expected Death QX2008VBT by Policy`),
                EDeaths_Amt_QX2008VBT = sum(`Expected Death QX2008VBT by Amount`),
                EDeaths_Num_QX2008VBTLU = sum(`Expected Death QX2008VBTLU by Policy`),
                EDeaths_Amt_QX2008VBTLU = sum(`Expected Death QX2008VBTLU by Amount`),
                EDeaths_Num_QX2015VBT = sum(`Expected Death QX2015VBT by Policy`),
                EDeaths_Amt_QX2015VBT = sum(`Expected Death QX2015VBT by Amount`),
                EDeaths_Num_QX7580E = sum(`Expected Death QX7580E by Policy`),
                EDeaths_Amt_QX7580E = sum(`Expected Death QX7580E by Amount`)) %>%
      mutate(Deaths_Num_Cap = replace(Deaths_Num, Deaths_Num > 3007, 3007)) %>%
      mutate(AE_Num_QX2001VBT = Deaths_Num/EDeaths_Num_QX2001VBT,
             AE_Amt_QX2001VBT = Deaths_Amt/EDeaths_Amt_QX2001VBT,
             AE_Num_QX2008VBT = Deaths_Num/EDeaths_Num_QX2008VBT,
             AE_Amt_QX2008VBT = Deaths_Amt/EDeaths_Amt_QX2008VBT,
             AE_Num_QX2008VBTLU = Deaths_Num/EDeaths_Num_QX2008VBTLU,
             AE_Amt_QX2008VBTLU = Deaths_Amt/EDeaths_Amt_QX2008VBTLU,
             AE_Num_QX2015VBT = Deaths_Num/EDeaths_Num_QX2015VBT,
             AE_Amt_QX2015VBT = Deaths_Amt/EDeaths_Amt_QX2015VBT,
             AE_Num_QX7580E = Deaths_Num/EDeaths_Num_QX7580E,
             AE_Amt_QX7580E = Deaths_Amt/EDeaths_Amt_QX7580E) %>% 
      mutate(Cred_AE_Num_QX2001VBT = (sqrt(Deaths_Num_Cap/3007)*AE_Num_QX2001VBT) + (1-sqrt(Deaths_Num_Cap/3007)),
             Cred_AE_Amt_QX2001VBT = (sqrt(Deaths_Num_Cap/3007)*AE_Amt_QX2001VBT) + (1-sqrt(Deaths_Num_Cap/3007)),
             Cred_AE_Num_QX2008VBT = (sqrt(Deaths_Num_Cap/3007)*AE_Num_QX2008VBT) + (1-sqrt(Deaths_Num_Cap/3007)),
             Cred_AE_Amt_QX2008VBT = (sqrt(Deaths_Num_Cap/3007)*AE_Amt_QX2008VBT) + (1-sqrt(Deaths_Num_Cap/3007)),
             Cred_AE_Num_QX2008VBTLU = (sqrt(Deaths_Num_Cap/3007)*AE_Num_QX2008VBTLU) + (1-sqrt(Deaths_Num_Cap/3007)),
             Cred_AE_Amt_QX2008VBTLU = (sqrt(Deaths_Num_Cap/3007)*AE_Amt_QX2008VBTLU) + (1-sqrt(Deaths_Num_Cap/3007)),
             Cred_AE_Num_QX2015VBT = (sqrt(Deaths_Num_Cap/3007)*AE_Num_QX2015VBT) + (1-sqrt(Deaths_Num_Cap/3007)),
             Cred_AE_Amt_QX2015VBT = (sqrt(Deaths_Num_Cap/3007)*AE_Amt_QX2015VBT) + (1-sqrt(Deaths_Num_Cap/3007)),
             Cred_AE_Num_QX7580E = (sqrt(Deaths_Num_Cap/3007)*AE_Num_QX7580E) + (1-sqrt(Deaths_Num_Cap/3007)),
             Cred_AE_Amt_QX7580E = (sqrt(Deaths_Num_Cap/3007)*AE_Amt_QX7580E) + (1-sqrt(Deaths_Num_Cap/3007))) 
  })
  
  ############################################################ Age 1 ########################################
  plotType_age1 <- function(x, type) {
    switch(type,
           "Exposed Policies" = ggplot(x, aes(x = `Attained Age`, y = Policies/1000000)) +
             geom_bar(stat = "identity") +
             xlab("Attained Age") +
             ylab("Exposed Policies (Millions)") +
             labs(title = "Exposed Policies vs. Attained Age of Policy Holders") +
             theme(legend.position = "none"),
           "Exposed Amounts" = ggplot(x, aes(x = `Attained Age`, y = Exposure/1000000000)) +
             geom_bar(stat = "identity") +
             xlab("Attained Age") +
             ylab("Exposed Amounts (Billions)") +
             labs(title = "Exposed Amounts vs. Attained Age of Policy Holders") +
             theme(legend.position = "none")
    )
  }
  
  output$age1_plot <- renderPlotly({
    ggplot_age1 <- plotType_age1(df_age(), input$pType_age1)
    ggplotly(ggplot_age1)
  })
  
  ############################################################ Duration 1 ########################################
  plotType_duration1 <- function(x, type) {
    switch(type,
           "Exposed Policies" = ggplot(x, aes(x = Duration, y = Policies/1000000)) +
             geom_bar(stat = "identity") +
             xlab("Duration") +
             ylab("Exposed Policies (Millions)") +
             labs(title = "Exposed Policies vs. Duration of Policies") +
             theme(legend.position = "none"),
           "Exposed Amounts" = ggplot(x, aes(x = Duration, y = Exposure/1000000000)) +
             geom_bar(stat = "identity") +
             xlab("Duration") +
             ylab("Exposed Amounts (Billions)") +
             labs(title = "Exposed Amounts vs. Duration of Policies") +
             theme(legend.position = "none")
    )
  }
  
  output$duration1_plot <- renderPlotly({
    ggplot_duration1 <- plotType_duration1(df_duration(), input$pType_duration1)
    ggplotly(ggplot_duration1)
  })
  
  ############################################################ Age 2 ########################################
  plotType_age2 <- function(x, type) {
    switch(type,
           "A/E Policies" = ggplot(x, aes(x = `Attained Age`)) +
             geom_line(aes(y = AE_Num_QX2001VBT, color = "AE_Num_QX2001VBT")) +
             geom_line(aes(y = AE_Num_QX2008VBT, color = "AE_Num_QX2008VBT")) +
             geom_line(aes(y = AE_Num_QX2008VBTLU, color = "AE_Num_QX2008VBTLU")) +
             geom_line(aes(y = AE_Num_QX2015VBT, color = "AE_Num_QX2015VBT")) +
             geom_line(aes(y = AE_Num_QX7580E, color = "AE_Num_QX7580E")) +
             geom_line(aes(y = Cred_AE_Num_QX2001VBT, color = "Cred_AE_Num_QX2001VBT")) +
             geom_line(aes(y = Cred_AE_Num_QX2008VBT, color = "Cred_AE_Num_QX2008VBT")) +
             geom_line(aes(y = Cred_AE_Num_QX2008VBTLU, color = "Cred_AE_Num_QX2008VBTLU")) +
             geom_line(aes(y = Cred_AE_Num_QX2015VBT, color = "Cred_AE_Num_QX2015VBT")) +
             geom_line(aes(y = Cred_AE_Num_QX7580E, color = "Cred_AE_Num_QX7580E")) +
             geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
             scale_color_manual(values = c("#20E500",
                                           "#22CE13",
                                           "#24B726",
                                           "#27A039",
                                           "#29894C",
                                           "#E54300",
                                           "#D33C13",
                                           "#C23526",
                                           "#B12E39",
                                           "#9F284C")) +
             xlab("Attained Age") +
             ylab("Actual/Expected Deaths") +
             labs(title = "A/E Deaths vs. Attained Age of Policy Holders") +
             theme(legend.position = "none"),
           "A/E Exposures" = ggplot(x, aes(x = `Attained Age`)) +
             geom_line(aes(y = AE_Amt_QX2001VBT, color = "AE_Amt_QX2001VBT")) +
             geom_line(aes(y = AE_Amt_QX2008VBT, color = "AE_Amt_QX2008VBT")) +
             geom_line(aes(y = AE_Amt_QX2008VBTLU, color = "AE_Amt_QX2008VBTLU")) +
             geom_line(aes(y = AE_Amt_QX2015VBT, color = "AE_Amt_QX2015VBT")) +
             geom_line(aes(y = AE_Amt_QX7580E, color = "AE_Amt_QX7580E")) +
             geom_line(aes(y = Cred_AE_Amt_QX2001VBT, color = "Cred_AE_Amt_QX2001VBT")) +
             geom_line(aes(y = Cred_AE_Amt_QX2008VBT, color = "Cred_AE_Amt_QX2008VBT")) +
             geom_line(aes(y = Cred_AE_Amt_QX2008VBTLU, color = "Cred_AE_Amt_QX2008VBTLU")) +
             geom_line(aes(y = Cred_AE_Amt_QX2015VBT, color = "Cred_AE_Amt_QX2015VBT")) +
             geom_line(aes(y = Cred_AE_Amt_QX7580E, color = "Cred_AE_Amt_QX7580E")) +
             geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
             scale_color_manual(values = c("#20E500",
                                           "#22CE13",
                                           "#24B726",
                                           "#27A039",
                                           "#29894C",
                                           "#E54300",
                                           "#D33C13",
                                           "#C23526",
                                           "#B12E39",
                                           "#9F284C")) +
             xlab("Attained Age") +
             ylab("Actual/Expected Death Amounts") +
             labs(title = "A/E Death Amounts vs. Attained Age of Policy Holders") +
             theme(legend.position = "none")
    )
  }
  
  output$age2_plot <- renderPlotly({
    ggplot_age2 <- plotType_age2(df_age(), input$pType_age2)
    ggplotly(ggplot_age2)
  })
  
  ############################################################ Duration 2 ########################################
  plotType_duration2 <- function(x, type) {
    switch(type,
           "A/E Policies" = ggplot(x, aes(x = Duration)) +
             geom_line(aes(y = AE_Num_QX2001VBT, color = "AE_Num_QX2001VBT")) +
             geom_line(aes(y = AE_Num_QX2008VBT, color = "AE_Num_QX2008VBT")) +
             geom_line(aes(y = AE_Num_QX2008VBTLU, color = "AE_Num_QX2008VBTLU")) +
             geom_line(aes(y = AE_Num_QX2015VBT, color = "AE_Num_QX2015VBT")) +
             geom_line(aes(y = AE_Num_QX7580E, color = "AE_Num_QX7580E")) +
             geom_line(aes(y = Cred_AE_Num_QX2001VBT, color = "Cred_AE_Num_QX2001VBT")) +
             geom_line(aes(y = Cred_AE_Num_QX2008VBT, color = "Cred_AE_Num_QX2008VBT")) +
             geom_line(aes(y = Cred_AE_Num_QX2008VBTLU, color = "Cred_AE_Num_QX2008VBTLU")) +
             geom_line(aes(y = Cred_AE_Num_QX2015VBT, color = "Cred_AE_Num_QX2015VBT")) +
             geom_line(aes(y = Cred_AE_Num_QX7580E, color = "Cred_AE_Num_QX7580E")) +
             geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
             scale_color_manual(values = c("#20E500",
                                           "#22CE13",
                                           "#24B726",
                                           "#27A039",
                                           "#29894C",
                                           "#E54300",
                                           "#D33C13",
                                           "#C23526",
                                           "#B12E39",
                                           "#9F284C")) +
             xlab("Duration") +
             ylab("Actual/Expected Deaths") +
             labs(title = "A/E Deaths vs. Duration of Policies") +
             theme(legend.position = "none"),
           "A/E Exposures" = ggplot(x, aes(x = Duration)) +
             geom_line(aes(y = AE_Amt_QX2001VBT, color = "AE_Amt_QX2001VBT")) +
             geom_line(aes(y = AE_Amt_QX2008VBT, color = "AE_Amt_QX2008VBT")) +
             geom_line(aes(y = AE_Amt_QX2008VBTLU, color = "AE_Amt_QX2008VBTLU")) +
             geom_line(aes(y = AE_Amt_QX2015VBT, color = "AE_Amt_QX2015VBT")) +
             geom_line(aes(y = AE_Amt_QX7580E, color = "AE_Amt_QX7580E")) +
             geom_line(aes(y = Cred_AE_Amt_QX2001VBT, color = "Cred_AE_Amt_QX2001VBT")) +
             geom_line(aes(y = Cred_AE_Amt_QX2008VBT, color = "Cred_AE_Amt_QX2008VBT")) +
             geom_line(aes(y = Cred_AE_Amt_QX2008VBTLU, color = "Cred_AE_Amt_QX2008VBTLU")) +
             geom_line(aes(y = Cred_AE_Amt_QX2015VBT, color = "Cred_AE_Amt_QX2015VBT")) +
             geom_line(aes(y = Cred_AE_Amt_QX7580E, color = "Cred_AE_Amt_QX7580E")) +
             geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
             scale_color_manual(values = c("#20E500",
                                           "#22CE13",
                                           "#24B726",
                                           "#27A039",
                                           "#29894C",
                                           "#E54300",
                                           "#D33C13",
                                           "#C23526",
                                           "#B12E39",
                                           "#9F284C")) +
             xlab("Duration") +
             ylab("Actual/Expected Death Amounts") +
             labs(title = "A/E Death Amounts vs. Duration of Policies") +
             theme(legend.position = "none")
    )
  }
  
  output$duration2_plot <- renderPlotly({
    ggplot_duration2 <- plotType_duration2(df_duration(), input$pType_duration2)
    ggplotly(ggplot_duration2)
  })
  ################################### Counts and Amounts by Policy Type ###########################################
  
  output$QX7580E_ageplot <- renderPlotly({
    QX7580E_ageplot <- ggplot(df_age(), aes(x = `Attained Age`)) +
      geom_line(aes(y = AE_Num_QX7580E, color = "AE_Num_QX7580E")) +
      geom_line(aes(y = Cred_AE_Num_QX7580E, color = "Cred_AE_Num_QX7580E")) +
      geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
      scale_color_manual(values = c("#29894C",
                                    "#9F284C")) +
      xlab("Attained Age") +
      ylab("Actual/Expected Deaths") +
      labs(title = "A/E Deaths vs. Attained Age of Policy Holders") +
      theme(legend.position = "none")
    
    ggplotly(QX7580E_ageplot)
  })
  
  output$QX2001VBT_ageplot <- renderPlotly({
    QX2001VBT_ageplot <- ggplot(df_age(), aes(x = `Attained Age`)) +
      geom_line(aes(y = AE_Amt_QX2001VBT, color = "AE_Amt_QX2001VBT")) +
      geom_line(aes(y = Cred_AE_Amt_QX2001VBT, color = "Cred_AE_Amt_QX2001VBT")) +
      geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
      scale_color_manual(values = c("#20E500",
                                    "#E54300")) +
      xlab("Attained Age") +
      ylab("Actual/Expected Death Amounts") +
      labs(title = "A/E Death Amounts vs. Attained Age of Policies") +
      theme(legend.position = "none")
    
    ggplotly(QX2001VBT_ageplot)
  })
  
  output$QX2008VBT_ageplot <- renderPlotly({
    QX2008VBT_ageplot <- ggplot(df_age(), aes(x = `Attained Age`)) +
      geom_line(aes(y = AE_Amt_QX2008VBT, color = "AE_Amt_QX2008VBT")) +
      geom_line(aes(y = Cred_AE_Amt_QX2008VBT, color = "Cred_AE_Amt_QX2008VBT")) +
      geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
      scale_color_manual(values = c("#22CE13",
                                    "#D33C13")) +
      xlab("Attained Age") +
      ylab("Actual/Expected Death Amounts") +
      labs(title = "A/E Death Amounts vs. Attained Age of Policies") +
      theme(legend.position = "none")
    
    ggplotly(QX2008VBT_ageplot)
  })
  
  output$QX2008VBTLU_ageplot <- renderPlotly({
    QX2008VBTLU_ageplot <- ggplot(df_age(), aes(x = `Attained Age`)) +
      geom_line(aes(y = AE_Amt_QX2008VBTLU, color = "AE_Amt_QX2008VBTLU")) +
      geom_line(aes(y = Cred_AE_Amt_QX2008VBTLU, color = "Cred_AE_Amt_QX2008VBTLU")) +
      geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
      scale_color_manual(values = c("#24B726",
                                    "#C23526")) +
      xlab("Attained Age") +
      ylab("Actual/Expected Death Amounts") +
      labs(title = "A/E Death Amounts vs. Attained Age of Policies") +
      theme(legend.position = "none")
    
    ggplotly(QX2008VBTLU_ageplot)
  })
  
  output$QX2015VBT_ageplot <- renderPlotly({
    QX2015VBT_ageplot <- ggplot(df_age(), aes(x = `Attained Age`)) +
      geom_line(aes(y = AE_Amt_QX2015VBT, color = "AE_Amt_QX2015VBT")) +
      geom_line(aes(y = Cred_AE_Amt_QX2015VBT, color = "Cred_AE_Amt_QX2015VBT")) +
      geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
      scale_color_manual(values = c("#27A039",
                                    "#B12E39")) +
      xlab("Attained Age") +
      ylab("Actual/Expected Death Amounts") +
      labs(title = "A/E Death Amounts vs. Attained Age of Policies") +
      theme(legend.position = "none")
    
    ggplotly(QX2015VBT_ageplot)
  })
  
  output$QX7580E_durplot <- renderPlotly({
    QX7580E_durplot <- ggplot(df_duration(), aes(x = Duration)) +
      geom_line(aes(y = AE_Num_QX7580E, color = "AE_Num_QX7580E")) +
      geom_line(aes(y = Cred_AE_Num_QX7580E, color = "Cred_AE_Num_QX7580E")) +
      geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
      scale_color_manual(values = c("#29894C",
                                    "#9F284C")) +
      xlab("Duration") +
      ylab("Actual/Expected Deaths") +
      labs(title = "A/E Deaths vs. Duration of Policy Holders") +
      theme(legend.position = "none")
    
    ggplotly(QX7580E_durplot)
  })
  
  output$QX2001VBT_durplot <- renderPlotly({
    QX2001VBT_durplot <- ggplot(df_duration(), aes(x = Duration)) +
      geom_line(aes(y = AE_Amt_QX2001VBT, color = "AE_Amt_QX2001VBT")) +
      geom_line(aes(y = Cred_AE_Amt_QX2001VBT, color = "Cred_AE_Amt_QX2001VBT")) +
      geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
      scale_color_manual(values = c("#20E500",
                                    "#E54300")) +
      xlab("Duration") +
      ylab("Actual/Expected Death Amounts") +
      labs(title = "A/E Death Amounts vs. Duration of Policies") +
      theme(legend.position = "none")
    
    ggplotly(QX2001VBT_durplot)
  })
  
  output$QX2008VBT_durplot <- renderPlotly({
    QX2008VBT_durplot <- ggplot(df_duration(), aes(x = Duration)) +
      geom_line(aes(y = AE_Amt_QX2008VBT, color = "AE_Amt_QX2008VBT")) +
      geom_line(aes(y = Cred_AE_Amt_QX2008VBT, color = "Cred_AE_Amt_QX2008VBT")) +
      geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
      scale_color_manual(values = c("#22CE13",
                                    "#D33C13")) +
      xlab("Duration") +
      ylab("Actual/Expected Death Amounts") +
      labs(title = "A/E Death Amounts vs. Duration of Policies") +
      theme(legend.position = "none")
    
    ggplotly(QX2008VBT_durplot)
  })
  
  output$QX2008VBTLU_durplot <- renderPlotly({
    QX2008VBTLU_durplot <- ggplot(df_duration(), aes(x = Duration)) +
      geom_line(aes(y = AE_Amt_QX2008VBTLU, color = "AE_Amt_QX2008VBTLU")) +
      geom_line(aes(y = Cred_AE_Amt_QX2008VBTLU, color = "Cred_AE_Amt_QX2008VBTLU")) +
      geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
      scale_color_manual(values = c("#24B726",
                                    "#C23526")) +
      xlab("Duration") +
      ylab("Actual/Expected Death Amounts") +
      labs(title = "A/E Death Amounts vs. Duration of Policies") +
      theme(legend.position = "none")
    
    ggplotly(QX2008VBTLU_durplot)
  })
  
  output$QX2015VBT_durplot <- renderPlotly({
    QX2015VBT_durplot <- ggplot(df_duration(), aes(x = Duration)) +
      geom_line(aes(y = AE_Amt_QX2015VBT, color = "AE_Amt_QX2015VBT")) +
      geom_line(aes(y = Cred_AE_Amt_QX2015VBT, color = "Cred_AE_Amt_QX2015VBT")) +
      geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
      scale_color_manual(values = c("#27A039",
                                    "#B12E39")) +
      xlab("Duration") +
      ylab("Actual/Expected Death Amounts") +
      labs(title = "A/E Death Amounts vs. Duration of Policies") +
      theme(legend.position = "none")
    
    ggplotly(QX2015VBT_durplot)
  })
  
  ############################### Comparison Tab ###########################################
  comparison_reactive <- reactive({
    salesdata %>%
      filter(`Issue Year` >= input$comparison_issueyear[1] &
               `Issue Year` <= input$comparison_issueyear[2]) %>%
      filter(`Observation Year` >= input$comparison_obsyear[1] &
               `Observation Year` <= input$comparison_obsyear[2])
  })
  
  salesdata_reactive_comparison1 <- reactive({
    comparison_reactive() %>% 
      filter(`Issue Age` >= input$comparison_issueage_1[1] &
               `Issue Age` <= input$comparison_issueage_1[2]) %>% 
      filter(`Attained Age` >= input$comparison_attainedage_1[1] &
               `Attained Age` <= input$comparison_attainedage_1[2]) %>% 
      filter(Duration >= input$comparison_duration_1[1] &
               Duration <= input$comparison_duration_1[2]) %>% 
      subset(Gender %in% input$comparison_gender_1) %>% # CORRECT
      subset(`Smoker Status` %in% input$comparison_smoker_1) %>%
      subset(Select_Ultimate_Indicator %in% input$comparison_selectult_1) %>% 
      # subset(`Insurance Plan` %in% input$comparison_insuranceplan_1) %>% 
      subset(`Face Amount Band` %in% input$comparison_faceamtband_1) %>% 
      subset(`SOA Anticipated Level Term Period` %in% input$comparison_soaanticipated_1) %>% 
      subset(`SOA Guaranteed Level Term Period` %in% input$comparison_soaguaranteed_1) %>% 
      subset(`SOA Post level term indicator` %in% input$comparison_soapostlevel_1) %>% 
      filter(`Age Basis` >= input$comparison_agebasis_1[1] &
               `Age Basis` <= input$comparison_agebasis_1[2]) %>%
      # filter(`Number of Preferred Classes` >= input$comparison_numprefer_1[1] &
      #          `Number of Preferred Classes` <= input$comparison_numprefer_1[2]) %>%
      # filter(`Preferred Class` >= input$comparison_prefer_1[1] &
      #          `Preferred Class` <= input$comparison_prefer_1[2]) %>%
      filter(`Number of Deaths` >= input$comparison_deaths_1[1] &
               `Number of Deaths` <= input$comparison_deaths_1[2]) %>% 
      filter(`Death Claim Amount` >= input$comparison_deathclaim_1[1] &
               `Death Claim Amount` <= input$comparison_deathclaim_1[2]) %>% 
      filter(`Policies Exposed` >= input$comparison_policies_1[1] &
               `Policies Exposed` <= input$comparison_policies_1[2]) %>% 
      filter(`Amount Exposed` >= input$comparison_amount_1[1] &
               `Amount Exposed` <= input$comparison_amount_1[2]) %>% 
      filter(`Expected Death QX7580E by Amount` >= input$comparison_eda1_1[1] &
               `Expected Death QX7580E by Amount` <= input$comparison_eda1_1[2]) %>% 
      filter(`Expected Death QX2001VBT by Amount` >= input$comparison_eda2_1[1] &
               `Expected Death QX2001VBT by Amount` <= input$comparison_eda2_1[2]) %>% 
      filter(`Expected Death QX2008VBT by Amount` >= input$comparison_eda3_1[1] &
               `Expected Death QX2008VBT by Amount` <= input$comparison_eda3_1[2]) %>% 
      filter(`Expected Death QX2008VBTLU by Amount` >= input$comparison_eda4_1[1] &
               `Expected Death QX2008VBTLU by Amount` <= input$comparison_eda4_1[2]) %>% 
      filter(`Expected Death QX2015VBT by Amount` >= input$comparison_eda5_1[1] &
               `Expected Death QX2015VBT by Amount` <= input$comparison_eda5_1[2]) %>% 
      filter(`Expected Death QX7580E by Policy` >= input$comparison_edp1_1[1] &
               `Expected Death QX7580E by Policy` <= input$comparison_edp1_1[2]) %>% 
      filter(`Expected Death QX2001VBT by Policy` >= input$comparison_edp2_1[1] &
               `Expected Death QX2001VBT by Policy` <= input$comparison_edp2_1[2]) %>% 
      filter(`Expected Death QX2008VBT by Policy` >= input$comparison_edp3_1[1] &
               `Expected Death QX2008VBT by Policy` <= input$comparison_edp3_1[2]) %>% 
      filter(`Expected Death QX2008VBTLU by Policy` >= input$comparison_edp4_1[1] &
               `Expected Death QX2008VBTLU by Policy` <= input$comparison_edp4_1[2]) %>% 
      filter(`Expected Death QX2015VBT by Policy` >= input$comparison_edp5_1[1] &
               `Expected Death QX2015VBT by Policy` <= input$comparison_edp5_1[2]) %>% 
      mutate(Series = "Series 1")
  })
  
  salesdata_reactive_comparison2 <- reactive({
    comparison_reactive() %>% 
      filter(`Issue Age` >= input$comparison_issueage_2[1] &
               `Issue Age` <= input$comparison_issueage_2[2]) %>% 
      filter(`Attained Age` >= input$comparison_attainedage_2[1] &
               `Attained Age` <= input$comparison_attainedage_2[2]) %>% 
      filter(Duration >= input$comparison_duration_2[1] &
               Duration <= input$comparison_duration_2[2]) %>% 
      subset(Gender %in% input$comparison_gender_2) %>% # CORRECT
      subset(`Smoker Status` %in% input$comparison_smoker_2) %>%
      subset(Select_Ultimate_Indicator %in% input$comparison_selectult_2) %>% 
      # subset(`Insurance Plan` %in% input$comparison_insuranceplan_2) %>% 
      subset(`Face Amount Band` %in% input$comparison_faceamtband_2) %>% 
      subset(`SOA Anticipated Level Term Period` %in% input$comparison_soaanticipated_2) %>% 
      subset(`SOA Guaranteed Level Term Period` %in% input$comparison_soaguaranteed_2) %>% 
      subset(`SOA Post level term indicator` %in% input$comparison_soapostlevel_2) %>% 
      filter(`Age Basis` >= input$comparison_agebasis_2[1] &
               `Age Basis` <= input$comparison_agebasis_2[2]) %>%
      # filter(`Number of Preferred Classes` >= input$comparison_numprefer_2[1] &
      #          `Number of Preferred Classes` <= input$comparison_numprefer_2[2]) %>%
      # filter(`Preferred Class` >= input$comparison_prefer_2[1] &
      #          `Preferred Class` <= input$comparison_prefer_2[2]) %>%
      filter(`Number of Deaths` >= input$comparison_deaths_2[1] &
               `Number of Deaths` <= input$comparison_deaths_2[2]) %>% 
      filter(`Death Claim Amount` >= input$comparison_deathclaim_2[1] &
               `Death Claim Amount` <= input$comparison_deathclaim_2[2]) %>% 
      filter(`Policies Exposed` >= input$comparison_policies_2[1] &
               `Policies Exposed` <= input$comparison_policies_2[2]) %>% 
      filter(`Amount Exposed` >= input$comparison_amount_2[1] &
               `Amount Exposed` <= input$comparison_amount_2[2]) %>% 
      filter(`Expected Death QX7580E by Amount` >= input$comparison_eda1_2[1] &
               `Expected Death QX7580E by Amount` <= input$comparison_eda1_2[2]) %>% 
      filter(`Expected Death QX2001VBT by Amount` >= input$comparison_eda2_2[1] &
               `Expected Death QX2001VBT by Amount` <= input$comparison_eda2_2[2]) %>% 
      filter(`Expected Death QX2008VBT by Amount` >= input$comparison_eda3_2[1] &
               `Expected Death QX2008VBT by Amount` <= input$comparison_eda3_2[2]) %>% 
      filter(`Expected Death QX2008VBTLU by Amount` >= input$comparison_eda4_2[1] &
               `Expected Death QX2008VBTLU by Amount` <= input$comparison_eda4_2[2]) %>% 
      filter(`Expected Death QX2015VBT by Amount` >= input$comparison_eda5_2[1] &
               `Expected Death QX2015VBT by Amount` <= input$comparison_eda5_2[2]) %>% 
      filter(`Expected Death QX7580E by Policy` >= input$comparison_edp1_2[1] &
               `Expected Death QX7580E by Policy` <= input$comparison_edp1_2[2]) %>% 
      filter(`Expected Death QX2001VBT by Policy` >= input$comparison_edp2_2[1] &
               `Expected Death QX2001VBT by Policy` <= input$comparison_edp2_2[2]) %>% 
      filter(`Expected Death QX2008VBT by Policy` >= input$comparison_edp3_2[1] &
               `Expected Death QX2008VBT by Policy` <= input$comparison_edp3_2[2]) %>% 
      filter(`Expected Death QX2008VBTLU by Policy` >= input$comparison_edp4_2[1] &
               `Expected Death QX2008VBTLU by Policy` <= input$comparison_edp4_2[2]) %>% 
      filter(`Expected Death QX2015VBT by Policy` >= input$comparison_edp5_2[1] &
               `Expected Death QX2015VBT by Policy` <= input$comparison_edp5_2[2]) %>% 
      mutate(Series = "Series 2")
  })
  
  salesdata_comparison_age_plot <- reactive({
    rbind(salesdata_reactive_comparison1(), salesdata_reactive_comparison2()) %>%
      group_by(`Attained Age`, Series) %>% 
      summarise(Policies = sum(`Policies Exposed`),
                Exposure = sum(`Amount Exposed`),
                Deaths_Num = sum(`Number of Deaths`),
                Deaths_Amt = sum(`Death Claim Amount`),
                EDeaths_Num_QX2001VBT = sum(`Expected Death QX2001VBT by Policy`),
                EDeaths_Amt_QX2001VBT = sum(`Expected Death QX2001VBT by Amount`),
                EDeaths_Num_QX2008VBT = sum(`Expected Death QX2008VBT by Policy`),
                EDeaths_Amt_QX2008VBT = sum(`Expected Death QX2008VBT by Amount`),
                EDeaths_Num_QX2008VBTLU = sum(`Expected Death QX2008VBTLU by Policy`),
                EDeaths_Amt_QX2008VBTLU = sum(`Expected Death QX2008VBTLU by Amount`),
                EDeaths_Num_QX2015VBT = sum(`Expected Death QX2015VBT by Policy`),
                EDeaths_Amt_QX2015VBT = sum(`Expected Death QX2015VBT by Amount`),
                EDeaths_Num_QX7580E = sum(`Expected Death QX7580E by Policy`),
                EDeaths_Amt_QX7580E = sum(`Expected Death QX7580E by Amount`)) %>% 
      mutate(Deaths_Num_Cap = replace(Deaths_Num, Deaths_Num > 3007, 3007)) %>%
      mutate(AE_Num_QX2001VBT = Deaths_Num/EDeaths_Num_QX2001VBT,
             AE_Amt_QX2001VBT = Deaths_Amt/EDeaths_Amt_QX2001VBT,
             AE_Num_QX2008VBT = Deaths_Num/EDeaths_Num_QX2008VBT,
             AE_Amt_QX2008VBT = Deaths_Amt/EDeaths_Amt_QX2008VBT,
             AE_Num_QX2008VBTLU = Deaths_Num/EDeaths_Num_QX2008VBTLU,
             AE_Amt_QX2008VBTLU = Deaths_Amt/EDeaths_Amt_QX2008VBTLU,
             AE_Num_QX2015VBT = Deaths_Num/EDeaths_Num_QX2015VBT,
             AE_Amt_QX2015VBT = Deaths_Amt/EDeaths_Amt_QX2015VBT,
             AE_Num_QX7580E = Deaths_Num/EDeaths_Num_QX7580E,
             AE_Amt_QX7580E = Deaths_Amt/EDeaths_Amt_QX7580E) %>% 
      mutate(Cred_AE_Num_QX2001VBT = (sqrt(Deaths_Num_Cap/3007)*AE_Num_QX2001VBT) + (1-sqrt(Deaths_Num_Cap/3007)),
             Cred_AE_Amt_QX2001VBT = (sqrt(Deaths_Num_Cap/3007)*AE_Amt_QX2001VBT) + (1-sqrt(Deaths_Num_Cap/3007)),
             Cred_AE_Num_QX2008VBT = (sqrt(Deaths_Num_Cap/3007)*AE_Num_QX2008VBT) + (1-sqrt(Deaths_Num_Cap/3007)),
             Cred_AE_Amt_QX2008VBT = (sqrt(Deaths_Num_Cap/3007)*AE_Amt_QX2008VBT) + (1-sqrt(Deaths_Num_Cap/3007)),
             Cred_AE_Num_QX2008VBTLU = (sqrt(Deaths_Num_Cap/3007)*AE_Num_QX2008VBTLU) + (1-sqrt(Deaths_Num_Cap/3007)),
             Cred_AE_Amt_QX2008VBTLU = (sqrt(Deaths_Num_Cap/3007)*AE_Amt_QX2008VBTLU) + (1-sqrt(Deaths_Num_Cap/3007)),
             Cred_AE_Num_QX2015VBT = (sqrt(Deaths_Num_Cap/3007)*AE_Num_QX2015VBT) + (1-sqrt(Deaths_Num_Cap/3007)),
             Cred_AE_Amt_QX2015VBT = (sqrt(Deaths_Num_Cap/3007)*AE_Amt_QX2015VBT) + (1-sqrt(Deaths_Num_Cap/3007)),
             Cred_AE_Num_QX7580E = (sqrt(Deaths_Num_Cap/3007)*AE_Num_QX7580E) + (1-sqrt(Deaths_Num_Cap/3007)),
             Cred_AE_Amt_QX7580E = (sqrt(Deaths_Num_Cap/3007)*AE_Amt_QX7580E) + (1-sqrt(Deaths_Num_Cap/3007)))  
  })
  
  salesdata_comparison_duration_plot <- reactive({
    rbind(salesdata_reactive_comparison1(), salesdata_reactive_comparison2()) %>%
      group_by(Duration, Series) %>% 
      summarise(Policies = sum(`Policies Exposed`),
                Exposure = sum(`Amount Exposed`),
                Deaths_Num = sum(`Number of Deaths`),
                Deaths_Amt = sum(`Death Claim Amount`),
                EDeaths_Num_QX2001VBT = sum(`Expected Death QX2001VBT by Policy`),
                EDeaths_Amt_QX2001VBT = sum(`Expected Death QX2001VBT by Amount`),
                EDeaths_Num_QX2008VBT = sum(`Expected Death QX2008VBT by Policy`),
                EDeaths_Amt_QX2008VBT = sum(`Expected Death QX2008VBT by Amount`),
                EDeaths_Num_QX2008VBTLU = sum(`Expected Death QX2008VBTLU by Policy`),
                EDeaths_Amt_QX2008VBTLU = sum(`Expected Death QX2008VBTLU by Amount`),
                EDeaths_Num_QX2015VBT = sum(`Expected Death QX2015VBT by Policy`),
                EDeaths_Amt_QX2015VBT = sum(`Expected Death QX2015VBT by Amount`),
                EDeaths_Num_QX7580E = sum(`Expected Death QX7580E by Policy`),
                EDeaths_Amt_QX7580E = sum(`Expected Death QX7580E by Amount`)) %>% 
      mutate(Deaths_Num_Cap = replace(Deaths_Num, Deaths_Num > 3007, 3007)) %>%
      mutate(AE_Num_QX2001VBT = Deaths_Num/EDeaths_Num_QX2001VBT,
             AE_Amt_QX2001VBT = Deaths_Amt/EDeaths_Amt_QX2001VBT,
             AE_Num_QX2008VBT = Deaths_Num/EDeaths_Num_QX2008VBT,
             AE_Amt_QX2008VBT = Deaths_Amt/EDeaths_Amt_QX2008VBT,
             AE_Num_QX2008VBTLU = Deaths_Num/EDeaths_Num_QX2008VBTLU,
             AE_Amt_QX2008VBTLU = Deaths_Amt/EDeaths_Amt_QX2008VBTLU,
             AE_Num_QX2015VBT = Deaths_Num/EDeaths_Num_QX2015VBT,
             AE_Amt_QX2015VBT = Deaths_Amt/EDeaths_Amt_QX2015VBT,
             AE_Num_QX7580E = Deaths_Num/EDeaths_Num_QX7580E,
             AE_Amt_QX7580E = Deaths_Amt/EDeaths_Amt_QX7580E) %>% 
      mutate(Cred_AE_Num_QX2001VBT = (sqrt(Deaths_Num_Cap/3007)*AE_Num_QX2001VBT) + (1-sqrt(Deaths_Num_Cap/3007)),
             Cred_AE_Amt_QX2001VBT = (sqrt(Deaths_Num_Cap/3007)*AE_Amt_QX2001VBT) + (1-sqrt(Deaths_Num_Cap/3007)),
             Cred_AE_Num_QX2008VBT = (sqrt(Deaths_Num_Cap/3007)*AE_Num_QX2008VBT) + (1-sqrt(Deaths_Num_Cap/3007)),
             Cred_AE_Amt_QX2008VBT = (sqrt(Deaths_Num_Cap/3007)*AE_Amt_QX2008VBT) + (1-sqrt(Deaths_Num_Cap/3007)),
             Cred_AE_Num_QX2008VBTLU = (sqrt(Deaths_Num_Cap/3007)*AE_Num_QX2008VBTLU) + (1-sqrt(Deaths_Num_Cap/3007)),
             Cred_AE_Amt_QX2008VBTLU = (sqrt(Deaths_Num_Cap/3007)*AE_Amt_QX2008VBTLU) + (1-sqrt(Deaths_Num_Cap/3007)),
             Cred_AE_Num_QX2015VBT = (sqrt(Deaths_Num_Cap/3007)*AE_Num_QX2015VBT) + (1-sqrt(Deaths_Num_Cap/3007)),
             Cred_AE_Amt_QX2015VBT = (sqrt(Deaths_Num_Cap/3007)*AE_Amt_QX2015VBT) + (1-sqrt(Deaths_Num_Cap/3007)),
             Cred_AE_Num_QX7580E = (sqrt(Deaths_Num_Cap/3007)*AE_Num_QX7580E) + (1-sqrt(Deaths_Num_Cap/3007)),
             Cred_AE_Amt_QX7580E = (sqrt(Deaths_Num_Cap/3007)*AE_Amt_QX7580E) + (1-sqrt(Deaths_Num_Cap/3007)))   
  })
  
  # salesdata_comparison_plot <- reactive({
  #   rbind(salesdata_reactive_comparison1(), salesdata_reactive_comparison2()) %>% 
  #     subset(select = c("month","Series","sales")) %>%
  #     group_by(month, Series) %>%
  #     summarise(Total_Sales = sum(sales)) %>%
  #     mutate_if(is.numeric, round, 2)
  # })
  
  #### Comparison Tab Plots ####
  # 
  # output$comparison_age_scatterplot <- renderPlotly({
  #   gg_age_comparison <- ggplot(data = salesdata_comparison_age_plot(), aes(x = `Attained Age`)) +
  #     # geom_line(aes(color = Series)) +
  #     # geom_line(aes(y = AE, color = Series)) +
  #     geom_line(aes(y = Cred_AE, color = Series)) +
  #     geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  #     xlab("Attained Age") +
  #     ylab("A/E Deaths") +
  #     labs(title = "A/E Deaths vs. Attained Age of Policies") +
  #     theme(legend.position = "none")
  #   ggplotly(gg_age_comparison)
  # })
  # 
  # output$comparison_duration_scatterplot <- renderPlotly({
  #   gg_duration_comparison <- ggplot(data = salesdata_comparison_duration_plot(), aes(x = Duration)) +
  #     # geom_line(aes(color = Series)) +
  #     # geom_line(aes(y = AE, color = Series)) +
  #     geom_line(aes(y = Cred_AE, color = Series)) +
  #     # scale_color_manual(values = c("#27A039",
  #     #                               "#B12E39")) +
  #     geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  #     xlab("Duration") +
  #     ylab("A/E Deaths") +
  #     labs(title = "A/E Deaths vs. Duration of Policies") +
  #     theme(legend.position = "none")
  #   ggplotly(gg_duration_comparison)
  # })
  
  output$comparison_QX7580E_ageplot <- renderPlotly({
    comparison_QX7580E_ageplot <- ggplot(salesdata_comparison_age_plot(), aes(x = `Attained Age`)) +
      # geom_line(aes(y = AE_Num_QX7580E, color = "AE_Num_QX7580E")) +
      geom_line(aes(y = Cred_AE_Num_QX7580E, color = Series)) +
      geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
      # scale_color_manual(values = c("#29894C",
      #                               "#9F284C")) +
      xlab("Attained Age") +
      ylab("Actual/Expected Deaths") +
      labs(title = "A/E Deaths vs. Attained Age of Policy Holders") +
      theme(legend.position = "none")
    
    ggplotly(comparison_QX7580E_ageplot)
  })
  
  output$comparison_QX2001VBT_ageplot <- renderPlotly({
    comparison_QX2001VBT_ageplot <- ggplot(salesdata_comparison_age_plot(), aes(x = `Attained Age`)) +
      # geom_line(aes(y = AE_Amt_QX2001VBT, color = "AE_Amt_QX2001VBT")) +
      geom_line(aes(y = Cred_AE_Amt_QX2001VBT, color = Series)) +
      geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
      # scale_color_manual(values = c("#20E500",
      #                               "#E54300")) +
      xlab("Attained Age") +
      ylab("Actual/Expected Death Amounts") +
      labs(title = "A/E Death Amounts vs. Attained Age of Policies") +
      theme(legend.position = "none")
    
    ggplotly(comparison_QX2001VBT_ageplot)
  })
  
  output$comparison_QX2008VBT_ageplot <- renderPlotly({
    comparison_QX2008VBT_ageplot <- ggplot(salesdata_comparison_age_plot(), aes(x = `Attained Age`)) +
      # geom_line(aes(y = AE_Amt_QX2008VBT, color = "AE_Amt_QX2008VBT")) +
      geom_line(aes(y = Cred_AE_Amt_QX2008VBT, color = Series)) +
      geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
      # scale_color_manual(values = c("#22CE13",
      #                               "#D33C13")) +
      xlab("Attained Age") +
      ylab("Actual/Expected Death Amounts") +
      labs(title = "A/E Death Amounts vs. Attained Age of Policies") +
      theme(legend.position = "none")
    
    ggplotly(comparison_QX2008VBT_ageplot)
  })
  
  output$comparison_QX2008VBTLU_ageplot <- renderPlotly({
    comparison_QX2008VBTLU_ageplot <- ggplot(salesdata_comparison_age_plot(), aes(x = `Attained Age`)) +
      # geom_line(aes(y = AE_Amt_QX2008VBTLU, color = "AE_Amt_QX2008VBTLU")) +
      geom_line(aes(y = Cred_AE_Amt_QX2008VBTLU, color = Series)) +
      geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
      # scale_color_manual(values = c("#24B726",
      #                               "#C23526")) +
      xlab("Attained Age") +
      ylab("Actual/Expected Death Amounts") +
      labs(title = "A/E Death Amounts vs. Attained Age of Policies") +
      theme(legend.position = "none")
    
    ggplotly(comparison_QX2008VBTLU_ageplot)
  })
  
  output$comparison_QX2015VBT_ageplot <- renderPlotly({
    comparison_QX2015VBT_ageplot <- ggplot(salesdata_comparison_age_plot(), aes(x = `Attained Age`)) +
      # geom_line(aes(y = AE_Amt_QX2015VBT, color = "AE_Amt_QX2015VBT")) +
      geom_line(aes(y = Cred_AE_Amt_QX2015VBT, color = Series)) +
      geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
      # scale_color_manual(values = c("#27A039",
      #                               "#B12E39")) +
      xlab("Attained Age") +
      ylab("Actual/Expected Death Amounts") +
      labs(title = "A/E Death Amounts vs. Attained Age of Policies") +
      theme(legend.position = "none")
    
    ggplotly(comparison_QX2015VBT_ageplot)
  })
  
  output$comparison_QX7580E_durplot <- renderPlotly({
    comparison_QX7580E_durplot <- ggplot(salesdata_comparison_duration_plot(), aes(x = Duration)) +
      # geom_line(aes(y = AE_Num_QX7580E, color = "AE_Num_QX7580E")) +
      geom_line(aes(y = Cred_AE_Num_QX7580E, color = Series)) +
      geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
      # scale_color_manual(values = c("#29894C",
      #                               "#9F284C")) +
      xlab("Duration") +
      ylab("Actual/Expected Deaths") +
      labs(title = "A/E Deaths vs. Duration of Policy Holders") +
      theme(legend.position = "none")
    
    ggplotly(comparison_QX7580E_durplot)
  })
  
  output$comparison_QX2001VBT_durplot <- renderPlotly({
    comparison_QX2001VBT_durplot <- ggplot(salesdata_comparison_duration_plot(), aes(x = Duration)) +
      # geom_line(aes(y = AE_Amt_QX2001VBT, color = "AE_Amt_QX2001VBT")) +
      geom_line(aes(y = Cred_AE_Amt_QX2001VBT, color = Series)) +
      geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
      # scale_color_manual(values = c("#20E500",
      #                               "#E54300")) +
      xlab("Duration") +
      ylab("Actual/Expected Death Amounts") +
      labs(title = "A/E Death Amounts vs. Duration of Policies") +
      theme(legend.position = "none")
    
    ggplotly(comparison_QX2001VBT_durplot)
  })
  
  output$comparison_QX2008VBT_durplot <- renderPlotly({
    comparison_QX2008VBT_durplot <- ggplot(salesdata_comparison_duration_plot(), aes(x = Duration)) +
      # geom_line(aes(y = AE_Amt_QX2008VBT, color = "AE_Amt_QX2008VBT")) +
      geom_line(aes(y = Cred_AE_Amt_QX2008VBT, color = Series)) +
      geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
      # scale_color_manual(values = c("#22CE13",
      #                               "#D33C13")) +
      xlab("Duration") +
      ylab("Actual/Expected Death Amounts") +
      labs(title = "A/E Death Amounts vs. Duration of Policies") +
      theme(legend.position = "none")
    
    ggplotly(comparison_QX2008VBT_durplot)
  })
  
  output$comparison_QX2008VBTLU_durplot <- renderPlotly({
    comparison_QX2008VBTLU_durplot <- ggplot(salesdata_comparison_duration_plot(), aes(x = Duration)) +
      # geom_line(aes(y = AE_Amt_QX2008VBTLU, color = "AE_Amt_QX2008VBTLU")) +
      geom_line(aes(y = Cred_AE_Amt_QX2008VBTLU, color = Series)) +
      geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
      # scale_color_manual(values = c("#24B726",
      #                               "#C23526")) +
      xlab("Duration") +
      ylab("Actual/Expected Death Amounts") +
      labs(title = "A/E Death Amounts vs. Duration of Policies") +
      theme(legend.position = "none")
    
    ggplotly(comparison_QX2008VBTLU_durplot)
  })
  
  output$comparison_QX2015VBT_durplot <- renderPlotly({
    comparison_QX2015VBT_durplot <- ggplot(salesdata_comparison_duration_plot(), aes(x = Duration)) +
      # geom_line(aes(y = AE_Amt_QX2015VBT, color = "AE_Amt_QX2015VBT")) +
      geom_line(aes(y = Cred_AE_Amt_QX2015VBT, color = Series)) +
      geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
      # scale_color_manual(values = c("#27A039",
      #                               "#B12E39")) +
      xlab("Duration") +
      ylab("Actual/Expected Death Amounts") +
      labs(title = "A/E Death Amounts vs. Duration of Policies") +
      theme(legend.position = "none")
    
    ggplotly(comparison_QX2015VBT_durplot)
  })
  
  #### App Metrics: output session info ####
  output$sessionInfo <- renderPrint({
    capture.output(sessionInfo())
  })
  #### Stop app when browser closes ####
  session$onSessionEnded(stopApp)
})