#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(shinycssloaders)



# Accuracy Stuff ---
# based off the TP=5, FN = 5, FP =1, TN=89 confusion matrix
# Given we know the state of the facing... what's the probability we alert or not.....
corrOOS <- function(){sample(x = c("OOS", "Stocked"), size = 1, prob = c(0.5, 0.5), replace=T)}
corrStock <- function(){sample(x = c("OOS", "Stocked"), size = 1, prob = c(0.01, 0.99), replace=T)}

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  tags$head(
    tags$style(HTML("hr {border-top: 1px solid #000000;}"))
  ),
  
    shinybusy::add_busy_bar(color = "red", height = "8px"),
    # Application title
    titlePanel("Simulated Effect of Accuracy on Savings",
               windowTitle = "Deming ROI"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("days",
                        "Simulated Days:",
                        min = 1, max = 30,
                        value = 15, step = 5),
            sliderInput("products",
                        "Number of Products:",
                        min = 100, max = 2000,
                        value = 1000, step = 200),
            hr(),
            sliderInput("logarithmicP",
                        "Logarithmic Probability:",
                        min = 0, max = 1,
                        value = 0.45, step = 0.05),          
            
            hr(),
            sliderInput("precision",
                        "% Correctly Detect (In Stock):",
                        min = 0, max = .99,
                        value = 0.5, step = 0.1),
            sliderInput("recall",
                        "% Correctly Detect (Out of Stock):",
                        min = 0.8, max = .999,
                        value = 0.99, step = 0.01),
            
            actionButton("run", label= "Run Simulation")
        ),

        # Show a plot of the generated distribution
        mainPanel(
          tabsetPanel(type = "tabs",
                      tabPanel("Accuracy Change",plotOutput("distPlot")), # %>% withSpinner(color="#0dc5c1")),
                      tabPanel("Sell Through",plotOutput("sellthru")), # %>% withSpinner(color="#0dc5c1")),
                      tabPanel("Notes",
                               tags$p("We are initially assuming a couple of thing about shopping at this simulated store:"),
                               tags$p("1. That for the products select that they are sold at roughly a logarithmic probability rate"),
                               tags$p("2. That the shelf depth can hold up to 10 products"))
            
          )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # specs around stores and products
  # days <- c(1:20) #what day is this sold
  hours <- c(1:12) #hours of sellable merchandise
  # products <- c(1:1000) #how many sku does this store have
  shelf_amnt <- c(10) #how many products can fit on a shelf... this is effectively what the starting stock is.
  
observeEvent(input$run , {
    base_prods <- reactive({
    expand.grid(
      prodID = c(1:input$products), day = c(1:input$days)
    ) %>% 
      as_tibble() %>% 
      rowwise() %>% 
      mutate(quant_start = sample(c(1:10),1, prob = c(0,0.05, 0.05, 0.05, 0.05, .1, .1 ,0.1, 0.4,0.1) ,replace = T)) %>% 
      ungroup() %>% 
      crossing(., hour = hours)
  })
  


    
  
  base_prd_removal <- reactive({
    base_prods() %>% 
      rowwise() %>% 
      # mutate(prod_pur = round(rexp(1, rate = 1.7),0)) %>% #assume products are purchased at roughly an exponential rate (this produces a sale through rate ~50-70%)
      mutate(prod_pur = actuar::rlogarithmic(n=1, prob=input$logarithmicP)-1) %>% #trying with a logarithmic distribution, center on 0 instead of 1
      ungroup() %>% 
      group_by(prodID, day, quant_start) %>% 
      mutate(cumProds = cumsum(prod_pur),
             ProdStatus = quant_start - cumProds,
             OOS = ifelse(ProdStatus <= 0, "OOS", "Stocked")) %>% 
      ungroup() %>% 
      rowwise() %>% 
      mutate(Alert = ifelse(OOS == "OOS", 
                            sample(x = c("OOS", "Stocked"), size = 1, prob = c((1-input$precision), input$precision), replace=T), #corrOOS()
                            sample(x = c("OOS", "Stocked"), size = 1, prob = c((1-input$recall), input$recall), replace=T) #corrStock()
                            )) %>% 
      ungroup() %>% 
      mutate(CorrFlag = ifelse(OOS == Alert, "Correct", "Missed")) 
  })

  base_prd_removal2 <- reactive({
    base_prd_removal() %>% 
      count(day, hour, OOS, CorrFlag) %>% 
      group_by(day, hour, OOS) %>%
      mutate(CountStock = sum(n)) %>%
      ungroup() %>% 
      group_by(day, hour) %>% 
      mutate(TotalProd = sum(n)) %>% 
      ungroup() %>% 
      mutate(OSS_Corr = n/TotalProd,
             OSS_Stock = CountStock/TotalProd) %>% 
      filter(OOS == "OOS",
             CorrFlag == "Correct") %>% 
      # filter(day == 1) %>% 
      mutate(hour = factor(hour))
  })    
  
  
  
    output$distPlot <- renderPlot({
        # draw the histogram with the specified number of bins
        base_prd_removal2()  %>% 
          ggplot(aes(group = day))+
          geom_line(aes(hour, OSS_Corr), color = "blue")+
          geom_point(aes(hour, OSS_Corr), color = "blue")+
          geom_line(aes(hour, OSS_Stock), color = "grey")+
          geom_point(aes(hour, OSS_Stock), color = "grey")+
          geom_ribbon(aes(hour, ymin = OSS_Corr, ymax = OSS_Stock), alpha = 0.2, fill = "#028A0F") +
          geom_ribbon(aes(hour, ymin = 0, ymax = OSS_Corr), alpha = 0.2, fill = "#FFD300") +
          scale_y_continuous(expand = c(0,0))+
          scale_x_discrete(expand = c(0,0))+
          labs(title = "Savings with Variable Accuracy",
               subtitle = "Green: Savings with Current Model Accuracy, \nYellow: Additional Savings with Perfect Accuracy",
               y = "Percent of Products Out of Stock") 
        
        
    })
    
    
    output$sellthru <- renderPlot({
        base_prd_removal() %>% 
        group_by(prodID, day) %>% 
        summarise(TotalProds = sum(prod_pur),
                  Inventory = mean(quant_start)) %>% 
        mutate(CorrSold = ifelse(TotalProds > Inventory, Inventory, TotalProds),
               LossSales = ifelse(TotalProds > Inventory, TotalProds-Inventory, 0)) %>% 
        mutate(SellThru = CorrSold/Inventory) %>% 
        group_by(day) %>% 
        mutate(AvgSell = mean(SellThru)) %>% 
        ungroup() %>% 
        ggplot(aes(day, SellThru, group = day))+
        geom_point(position = position_jitter(width = 0.2, height = 0.01), aes(color =Inventory ))+
        geom_violin(alpha = 0.5)+
        geom_point(aes(day, AvgSell), color = "red", size = 2)+
        geom_hline(yintercept = c(0.4, 0.7), linetype = "dashed")+
        scale_y_continuous(limits = c(0,1), breaks = seq(0,1,by=.1), expand = c(0,0))+
        labs(title = "Sell-Through Percentage by Day",
             x= "Simulation Day", y = "Sell-Through (%)",
             subtitle = "Assuming a Logarithmic Sell Rate of Product Throughout the Day \nAnd that shelves are fully restocked at the start of each day")
      
      
      
      
    })
    
    
    
})  
}

# Run the application 
shinyApp(ui = ui, server = server)
