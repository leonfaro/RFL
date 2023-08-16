#devtools::install_github("wbreymann/FEMS")
#install.packages("devtools")
#install_github("wbreymann/FEMS")

# Deaktiviere Warnungen temporär
options(warn=-1)
options(width=100)
# Load required libraries
library(shiny)
library(ggplot2)
library(stringr)
library(scales)
### Import the reshape2 library for the melt function
library(reshape2)
library(devtools)

library(scales)
library(zoo)
# Load the xts package
library(xts)
library(FEMS)
library(DT)
library(eurostat)

#t0 <- format(Sys.Date(), "%Y-%m-%d")
t0 <- 2013-01-01
library(eurostat)

# Custom function to convert nested list to data frame
list_to_df <- function(input_list, parent_name = "") {
  output_df <- data.frame()
  
  for (item_name in names(input_list)) {
    if (is.list(input_list[[item_name]])) {
      output_df <- rbind(output_df, list_to_df(input_list[[item_name]], paste(parent_name, item_name, sep = "¦")))
    } else {
      output_df <- rbind(output_df, data.frame(Name = paste(parent_name, item_name, sep = "¦"), Value = input_list[[item_name]]))
    }
  }
  
  return(output_df)
}


library(timeSeries)
# Function for generating time series with geometric brownian motion and shocks
p.sim0 <- function(n, P.0, t.0, by= "1 year", shocks) {
  # Generate random returns following a normal distribution
  returns <- runif(n, -0.10, 0.10)
  
  # Compute the cumulative product of the returns
  cumulative_returns <- cumprod(1 + returns)
  
  # Compute the price prediction series
  price_series <- P.0 * cumulative_returns
  
  # Introduce shocks
  for(i in 1:shocks) {
    # Select a random point in the series
    shock_point <- sample(1:n, 1)
    
    # Compute the shock factor
    shock_factor <- ifelse(runif(1) < 0.5, runif(1, -0.50, -0.30), runif(1, 0.30, 0.50))
    
    # Apply the shock to the selected point
    price_series[shock_point] <- price_series[shock_point] * (1 + shock_factor)
  }
  
  # Generate the time series
  return(timeSeries(
    data = price_series,
    timeSequence(from=t.0, by=by, length.out=n)
  ))
}

p.simel <- function() {
  
  # el
  id.biann <- "nrg_pc_205"
  id.ann <- "nrg_pc_205_c"
  # GAS: we use
  #      - 28 Europa-Länder Durchschnittspreis
  #      - Preis inklusive aller Steuern
  #      - Preis für Konsum-Band:  1 000 GJ < Consumption < 10 000 GJ
  el.raw.biann <- get_eurostat(id = id.biann)
  el.raw.ann <- get_eurostat(id = id.ann)
  
  el.biann <- subset(as.data.frame(el.raw.biann),
                     geo=="EU27_2020" &  unit=="KWH" 
                     & tax=="I_TAX" & currency=="EUR" 
                     # & consom == 4142902
                     # & nrg_cons =="GJ1000000-3999999"
  )
  
  el <- el.biann[el.biann[,2]=="MWH70000-149999",]
  #el <- el.biann
  el.annual <- data.frame(
    time <- sort(grep("-01-01", el$time, fixed=TRUE, value=TRUE)),
    price <- tapply(el$values, substring(el$time, 1, 4), mean))
  colnames(el.annual) <- c("time", "price")
  
  library(timeSeries)
  el.ts <- timeSeries(data=as.numeric(el.annual[,2]), 
                      charvec=as.character(el.annual[,1]))
  el.ts<- tail(el.ts, n =10)
  # Generate the time series
  return(el.ts)
}



p.simgas <- function() {
  
  
  # gas
  id.biann <- "nrg_pc_203"
  id.ann <- "nrg_pc_203_c"
  
  library(eurostat)
  # GAS: we use
  #      - 28 Europa-Länder Durchschnittspreis
  #      - Preis inklusive aller Steuern
  #      - Preis für Konsum-Band:  1 000 GJ < Consumption < 10 000 GJ
  gas.raw.biann <- get_eurostat(id = id.biann)
  gas.raw.ann <- get_eurostat(id = id.ann)
  
  gas.biann <- subset(as.data.frame(gas.raw.biann),
                      geo=="EU27_2020" &  unit=="KWH" 
                      & tax=="I_TAX" & currency=="EUR" 
                      # & consom == 4142902
                      # & nrg_cons =="GJ1000000-3999999"
  )
  
  gas <- gas.biann[gas.biann[,2]=="GJ1000000-3999999",]
  #gas <- gas.biann
  gas.annual <- data.frame(
    time <- sort(grep("-01-01", gas$time, fixed=TRUE, value=TRUE)),
    price <- tapply(gas$values, substring(gas$time, 1, 4), mean))
  colnames(gas.annual) <- c("time", "price")
  
  
  colnames(gas.annual) <- c("time", "price")
  # Extract the value from the 'gas' data frame where the time is "2022-07-01"
  (new_value <- gas[gas$time == "2022-07-01", "values"])
  
  # Create a new row
  new_row <- data.frame(time = "2023-01-01", price = new_value)
  
  # Add the new row to the 'gas.annual' data frame
  gas.annual <- rbind(gas.annual, new_row)
  
  # Set the row names explicitly
  rownames(gas.annual) <- format(as.Date(gas.annual$time), "%Y")
  
  gas.ts <- timeSeries(data=as.numeric(gas.annual[,2]), 
                       charvec=as.character(gas.annual[,1]))
  gas.ts <- tail(gas.ts, n =11)
  return(gas.ts)

}


ui <- fluidPage(
  
  tags$head(
    tags$style(
      HTML(
        "
        .button {
          display: inline-block;
          vertical-align: middle;
        }
        .blue-background {
          background-color: #F5F5FF;
          padding: 10px;
        }
        "
      )
    )
  ),
  
  fluidRow(
    
    column(width = 4,
           div(
             img(src = "/Users/leonkiafaro/Desktop/FUM/Code/Riskandfinancelab/www/soelogo.jpg", height = "144", width = "250"),
             style = "margin-top: 10px; margin-left: 10px;"
           )
    ),
    
    column(width = 8,
           div(
             style = "text-align:left;",
             h2("Finanzielle Unternehmensmodellierung"),
             p("Das Modul „Finanzielle Unternehmensmodellierung“ befasst sich mit der Modellierung und 
               Analyse der finanziellen Seite von (Finanz- und Nicht-Finanz-) Institutionen. Für zukünftige 
               Wirtschaftsingenieure ist es wichtig, über die finanziellen Auswirkungen technischer 
               Entscheidungen Bescheid zu wissen. In diesem Kurs liegt der Schwerpunkt auf den 
               wirtschaftlichen Auswirkungen.
               
               Finanzierung der Investition mit einem Kredit zu einem festen Zinssatz beim aktuellen 
               Marktzinsniveau zur Zeit tO und mit einer Laufzeit entsprechend der Lebensdauer der Anlage.
               Die Anlage hat eine Produktionskapazität von 100 MW bei einem Wirkungsgrad von 40%.
               Stark vereinfacht kann also davon ausgegangen werden, dass die Anlage jede Stunde 100
               MWh Strom produziert und dabei 250 MWh Erdgas verbrennt. Die Anlage ist im Durchschnitt 24h 
               pro Tag und 300 Tage im Jahr in Betrieb und die gesamte Menge an produziertem Strom kann 
               abgesetzt werden. Dabei sollen die einzukaufenden Gas-mengen und abgesetzten Strommengen 
               jährlich berechnet und mit dem jeweiligen mittleren Jahres Erdgas- und Strompreis verrechnet 
               werden."),
             
             div(
               style = "text-align:center;",
               a(href = "#gaskraftwerk", target="_self", 
                 style="text-decoration:none; margin-right:20px;",
                 div(class = "button", 
                     img(src="kraftwerk.png", height="100", width="100", style="margin-right:10px;"),
                     div("Gaskraftwerk", style="font-size:18px; font-weight:bold;")
                 )
               )
             ),
             
             div(
               style = "text-align:center;",
               p("Für weitere Informationen kontaktieren Sie uns bitte unter ",
                 a("info@financialmodeling.zhaw.ch", href="mailto:info@financialmodeling.zhaw.ch"))
             )
           )
    )
  ),
  sidebarLayout(
    mainPanel(
      plotOutput("pricePlot"),
      conditionalPanel(condition = "input.enter > 0",
                       h3("Power plant structure"),
                       verbatimTextOutput("PowerPlantOutput1"),
                       h3("Assets: Long-term contract"),
                       verbatimTextOutput("PowerPlantOutput2"),
                       h3("Liabilities: Debt Contracts"),
                       verbatimTextOutput("PowerPlantOutput3"),
                       h3("Assets: Current Contracts"),
                       verbatimTextOutput("PowerPlantOutput4"),
                       h3("Operations: Revenue Events"),
                       verbatimTextOutput("PowerPlantOutput5"),
                       h3("Operations: Expenses Events"),
                       verbatimTextOutput("PowerPlantOutput55"),
                       h3("Assets: Long Term Events"),
                       verbatimTextOutput("PowerPlantOutput6"),
                       h3("Liabilities: Debt Events"),
                       verbatimTextOutput("PowerPlantOutput7"),
                       h3("Liquidität/Kapitalfluss (in Mio €)"),
                       DT::dataTableOutput("PowerPlantOutput8"),
                       h3("Erlös (in Mio €)"),
                       DT::dataTableOutput("PowerPlantOutput9"),
                       h3("Nominalwert (in Mio €)"),
                       DT::dataTableOutput("PowerPlantOutput10"),
                       h3("Marktnaher-Wert (in Mio €)"),
                       DT::dataTableOutput("PowerPlantOutput11")
      )
    ),
    
    sidebarPanel(
      numericInput("electricityprice", "Electricity Price (€/kWh):", 0.1801),
      numericInput("gasprice", "Gas Price (€/kWh):", 0.0486),
      numericInput("yieldcurve", "Zins (%):", 2),
      numericInput("inve", "Investment:", 100000000),
      actionButton("enter", "Enter")
    )
  )
)



# Define the Shiny server
server <- function(input, output) {
  
  # Create reactive values to store series and shocks
  rv <- reactiveValues()
  
  observeEvent(input$enter, {
    # gas
    id.biann <- "nrg_pc_203"
    id.ann <- "nrg_pc_203_c"
    
    
    # GAS: we use
    #      - 28 Europa-Länder Durchschnittspreis
    #      - Preis inklusive aller Steuern
    #      - Preis für Konsum-Band:  1 000 GJ < Consumption < 10 000 GJ
    gas.raw.biann <- get_eurostat(id = id.biann)
    gas.raw.ann <- get_eurostat(id = id.ann)
    
    gas.biann <- subset(as.data.frame(gas.raw.biann),
                        geo=="EU27_2020" &  unit=="KWH" 
                        & tax=="I_TAX" & currency=="EUR" 
                        # & consom == 4142902
                        # & nrg_cons =="GJ1000000-3999999"
    )
    
    gas <- gas.biann[gas.biann[,2]=="GJ1000000-3999999",]
    #gas <- gas.biann
    gas.annual <- data.frame(
      time <- sort(grep("-01-01", gas$time, fixed=TRUE, value=TRUE)),
      price <- tapply(gas$values, substring(gas$time, 1, 4), mean))
    colnames(gas.annual) <- c("time", "price")
    
    
    colnames(gas.annual) <- c("time", "price")
    # Extract the value from the 'gas' data frame where the time is "2022-07-01"
    (new_value <- gas[gas$time == "2022-07-01", "values"])
    
    # Create a new row
    new_row <- data.frame(time = "2023-01-01", price = new_value)
    
    # Add the new row to the 'gas.annual' data frame
    gas.annual <- rbind(gas.annual, new_row)
    
    # Set the row names explicitly
    rownames(gas.annual) <- format(as.Date(gas.annual$time), "%Y")
    
    gas.ts <- timeSeries(data=as.numeric(gas.annual[,2]), 
                         charvec=as.character(gas.annual[,1]))
    
    
    t0 <- '2013-01-01'
    
    # el
    id.biann <- "nrg_pc_205"
    id.ann <- "nrg_pc_205_c"
    # GAS: we use
    #      - 28 Europa-Länder Durchschnittspreis
    #      - Preis inklusive aller Steuern
    #      - Preis für Konsum-Band:  1 000 GJ < Consumption < 10 000 GJ
    el.raw.biann <- get_eurostat(id = id.biann)
    el.raw.ann <- get_eurostat(id = id.ann)
    
    el.biann <- subset(as.data.frame(el.raw.biann),
                       geo=="EU27_2020" &  unit=="KWH" 
                       & tax=="I_TAX" & currency=="EUR" 
                       # & consom == 4142902
                       # & nrg_cons =="GJ1000000-3999999"
    )
    
    el <- el.biann[el.biann[,2]=="MWH70000-149999",]
    #el <- el.biann
    el.annual <- data.frame(
      time <- sort(grep("-01-01", el$time, fixed=TRUE, value=TRUE)),
      price <- tapply(el$values, substring(el$time, 1, 4), mean))
    colnames(el.annual) <- c("time", "price")
    
    # Extract the value from the 'gas' data frame where the time is "2022-07-01"
    (new_value <- el[el$time == "2022-07-01", "values"])
    
    # Create a new row
    new_row <- data.frame(time = "2023-01-01", price = new_value)
    
    # Add the new row to the 'gas.annual' data frame
    el.annual <- rbind(el.annual, new_row)
    
    # Set the row names explicitly
    rownames(el.annual) <- format(as.Date(el.annual$time), "%Y")
    
    el.ts <- timeSeries(data=as.numeric(el.annual[,2]), 
                        charvec=as.character(el.annual[,1]))
    
    el.ts <- tail(el.ts, n=11)
    gas.ts <- tail(gas.ts, n=11)
    
    
    
    # Get the value from numericInput and divide by 100
    zins <- input$yieldcurve / 100
    
    # Zinskurve
    yc.tnr <- c("1M","10Y")
    yc.rts <- c(zins,zins)
    
    yc <- YieldCurve(label = "MARKTZINS",
                     ReferenceDate = '2013-01-01',
                     Tenors = yc.tnr, Rates = yc.rts)
    
    
    #el.p <- p.sim(11, input$electricityprice, t0, shocks=3)
    el.p <-el.ts
    el.idx <- Index(data=el.p, label="EL")
    
    
    #gas.p <- p.sim(11, input$gasprice, t0, shocks=3)
    gas.p <- gas.ts
    
    gas.idx <- Index(data=gas.p, label = "GAS")
    
    
    rf <- RFConn(list(gas.idx, el.idx, yc))
    
    diskont <- DcEngine(dc.spread=0.0, dc.object=yc)
    set(diskont, rf)
    
    rv$prices <- data.frame(
      Time = index(gas.p),
      Electricity = el.p$TS.1,
      Gas = gas.p$TS.1
    )
    
    
    
    #3. --------------------------------------------------------------------------------
    # Modellieren des Gas Kraftwerks
    investition.nominal <- input$inve # Investitionskosten
    # Modellieren des Kredits
    Credit <- loan(
      start = '2013-01-01',
      maturity = '2023-01-01',  # 10 years from today
      nominal = investition.nominal,
      ir = zins, # Marktzinsniveau per t0
      ContractID = "Credit01",
      Currency = "EUR",
      role = "short" # Alternative Namen: Passiva (Liabilities)
    )
    
    #4. Modellieren der Investition inkl. Abschreibungen:---------------------------------------------------
    # Operations Zeitachse
    ops.times <- timeSequence(from = '2013-01-01', by = "1 years", length.out = 11)
    # Funktion für die Abschreibungen über Zeit
    inv.func <- function(times) {
      seq_len <- length(times)
      timeSeries(seq(investition.nominal, 0, length.out=seq_len), times)
    }
    # Erstellen des Investitions-Kontrakts
    inv <- Investments(pattern=inv.func,
                       args=list(times=ops.times
                       ), Currency="EUR", ContractID="Invest01")
    
    
    #5. Modellieren der operativen Kosten (Einkauf von Gas):--------------------------------------------------------------------
    # Definiere Gas-Einkaufs-Pattern
    gas.func <- function(model) {
      timeSeries(-24*300*250*1000*as.timeSeries.RiskFactor(model[["GAS"]]),
                 ops.times)
    }
    # Erstellen des Kontrakts für die operativen Kosten
    gas <- OperationalCF(ContractID="Gas01", Currency="EUR", pattern=gas.func,
                         args=list(model=rf) )
    
    
    
    #6. Modellieren des Erlöses aus dem Verkauf von Strom:----------------------------------------
    # Definiere Strom-Verkaufs-Pattern
    el.func <- function(model) {
      timeSeries(24*300*100*1000*as.timeSeries.RiskFactor(model[["EL"]]),
                 ops.times)
    }
    # Erstellen des Kontrakts für den operativen Erlös
    el <- OperationalCF(ContractID="El01", Currency="EUR", pattern=el.func, args=list(model=rf))
    
    
    #8. Modellieren der Unternehmung:------------------------------------------------------------
    # Erstellen der Institution
    PowerPlant <- institution("PowerPlant")
    # Hinzufügen der Kontrakte
    addContracts(list(Loan=Credit), FindNode(PowerPlant, "Debt"))
    addContracts(list(Invest=inv), FindNode(PowerPlant, "LongTerm"))
    addContracts(list(Revenue=el), FindNode(PowerPlant, "Revenues"))
    addContracts(list(Expense=gas), FindNode(PowerPlant, "Expenses"))
    
    rv$PowerPlant <- PowerPlant  # Store PowerPlant in reactive values
    
    #9. Generieren der Events (= Ausführen der Simulation):------------------------------------------------
    events(PowerPlant, t0, rf, end_date=tail(ops.times,1))
    
    #10. Analysieren der Unternehmung:--------------------------------------------------------------------------
    
    # Define Zeitachse
    by <- timeSequence(t0, by = "1 years", length.out = 11)
    
    # Update bucketLabs in timeBuckets
    bucketLabs <- format(by[-length(by)], "%Y")  # Exclude the last label
    
    # Create time buckets
    tb <- timeBuckets(by, bucketLabs = bucketLabs)
    
    scale <- 1000000
    
    rv$a <- round(liquidity(PowerPlant, tb, "marginal")/scale,2)
    rv$b <- income(PowerPlant, tb, type="marginal", revaluation.gains=FALSE)/scale
    rv$c <- value(PowerPlant, tb, "nominal")/scale
    rv$d <- value(PowerPlant, tb, "market", method=diskont)/scale
    
    
    
    
  })
  
  
  
  
  output$pricePlot <- renderPlot({
    req(rv$prices)
    data <- rv$prices
    data$Year <- 2012 + data$Time
    
    ggplot(data, aes(x = Year)) +
      geom_line(aes(y = Electricity, color = "Electricity")) +
      geom_line(aes(y = Gas, color = "Gas")) +
      scale_x_continuous(name = "Time (Years)", breaks = 2013:2023) +
      scale_y_continuous(name = "Price (€)") +
      ggtitle("Electricity and Gas Price Simulation") +
      theme(legend.title = element_blank(), 
            plot.title = element_text(size = 20),
            axis.title.x = element_text(size = 16),
            axis.title.y = element_text(size = 16),
            axis.text.x = element_text(size = 14),
            axis.text.y = element_text(size = 14),
            legend.text = element_text(size = 14))
  })
  
  
  
  
  
  #output$shockTable <- renderTable({
  #  req(rv$shocks)
  #  rv$shocks
  #})
  
  output$PowerPlantOutput1 <- renderPrint({
    req(rv$PowerPlant)  # Make sure PowerPlant is available
    rv$PowerPlant  # Use reactive PowerPlant
    
  })
  
  output$PowerPlantOutput2 <- renderPrint({
    req(rv$PowerPlant)  # Make sure PowerPlant is available
    
    rv$PowerPlant$Assets$LongTerm$contracts# Use reactive PowerPlant
    
  })
  
  output$PowerPlantOutput3 <- renderPrint({
    req(rv$PowerPlant)  # Make sure PowerPlant is available
    
    rv$PowerPlant$Liabilities$Debt$contracts  # Use reactive PowerPlant
    
  })
  
  output$PowerPlantOutput4 <- renderPrint({
    req(rv$PowerPlant)  # Make sure PowerPlant is available
    
    rv$PowerPlant$Assets$Current$contracts # Use reactive PowerPlant
  })
  
  output$PowerPlantOutput5 <- renderPrint({
    req(rv$PowerPlant)  # Make sure PowerPlant is available
    
    rv$PowerPlant$Operations$Revenues$eventList # Use reactive PowerPlant
  })
  
  output$PowerPlantOutput55 <- renderPrint({
    req(rv$PowerPlant)  # Make sure PowerPlant is available
    
    rv$PowerPlant$Operations$Expenses$eventList # Use reactive PowerPlant
  })
  
  output$PowerPlantOutput6 <- renderPrint({
    req(rv$PowerPlant)  # Make sure PowerPlant is available
    
    rv$PowerPlant$Assets$LongTerm$eventList # Use reactive PowerPlant
  })
  
  output$PowerPlantOutput7 <- renderPrint({
    req(rv$PowerPlant)  # Make sure PowerPlant is available
    
    rv$PowerPlant$Liabilities$Debt$eventList # Use reactive PowerPlant
  })
  
  
  output$PowerPlantOutput8 <- DT::renderDataTable({
    req(rv$PowerPlant)  # Make sure PowerPlant is available
    data <- rv$a # Use reactive PowerPlant
    
    # Clean up the row names by removing "¦--", "°--", and replacing "¦" with "/"
    rownames(data) <- gsub("¦--|°--", "", rownames(data))
    rownames(data) <- gsub("¦", "/", rownames(data))
    
    # Convert the row names into a new column in your data frame
    data <- cbind(Name = rownames(data), data)
    
    # Reset the row names if needed
    rownames(data) <- NULL
    
    # Change column names to only include the year
    colnames(data)[-1] <- sub("-.*", "", colnames(data)[-1])
    
    # Format the numbers in the data frame to have no digits to the right of the decimal point
    data[,-1] <- format(round(data[,-1], 0), nsmall = 0, scientific = FALSE)
    
    
    # Return the modified data
    DT::datatable(data, rownames = FALSE,options = list(pageLength = 20,  autoWidth = TRUE, columnDefs = list(list(className = 'dt-right', targets = '_all'))))
  })
  
  
  output$PowerPlantOutput9 <- DT::renderDataTable({
    req(rv$PowerPlant)  # Make sure PowerPlant is available
    data <- rv$b # Use reactive PowerPlant
    
    # Clean up the row names by removing "¦--", "°--", and replacing "¦" with "/"
    rownames(data) <- gsub("¦--|°--", "", rownames(data))
    rownames(data) <- gsub("¦", "/", rownames(data))
    
    # Convert the row names into a new column in your data frame
    data <- cbind(Name = rownames(data), data)
    
    # Reset the row names if needed
    rownames(data) <- NULL
    
    # Change column names to only include the year
    colnames(data)[-1] <- sub("-.*", "", colnames(data)[-1])
    
    # Format the numbers in the data frame to have no digits to the right of the decimal point
    data[,-1] <- format(round(data[,-1], 0), nsmall = 0, scientific = FALSE)
    
    
    # Return the modified data
    DT::datatable(data, rownames = FALSE,options = list(pageLength = 20, autoWidth = TRUE, columnDefs = list(list(className = 'dt-right', targets = '_all'))))
  })
  
  
  
  
  output$PowerPlantOutput10 <- DT::renderDataTable({
    req(rv$PowerPlant)  # Make sure PowerPlant is available
    data <- rv$c # Use reactive PowerPlant
    
    # Clean up the row names by removing "¦--", "°--", and replacing "¦" with "/"
    rownames(data) <- gsub("¦--|°--", "", rownames(data))
    rownames(data) <- gsub("¦", "/", rownames(data))
    
    # Convert the row names into a new column in your data frame
    data <- cbind(Name = rownames(data), data)
    
    # Reset the row names if needed
    rownames(data) <- NULL
    
    # Change column names to only include the year
    colnames(data)[-1] <- sub("-.*", "", colnames(data)[-1])
    
    # Format the numbers in the data frame to have no digits to the right of the decimal point
    data[,-1] <- format(round(data[,-1], 0), nsmall = 0, scientific = FALSE)
    
    
    # Return the modified data
    DT::datatable(data,rownames = FALSE, options = list(pageLength = 20, autoWidth = TRUE, columnDefs = list(list(className = 'dt-right', targets = '_all'))))
  })
  
  output$PowerPlantOutput11 <- DT::renderDataTable({
    req(rv$PowerPlant)  # Make sure PowerPlant is available
    data <- rv$d # Use reactive PowerPlant
    
    # Clean up the row names by removing "¦--", "°--", and replacing "¦" with "/"
    rownames(data) <- gsub("¦--|°--", "", rownames(data))
    rownames(data) <- gsub("¦", "/", rownames(data))
    
    # Convert the row names into a new column in your data frame
    data <- cbind(Name = rownames(data), data)
    
    # Reset the row names if needed
    rownames(data) <- NULL
    
    # Change column names to only include the year
    colnames(data)[-1] <- sub("-.*", "", colnames(data)[-1])
    
    # Divide the numeric values by 1,000,000 to present them in millions
    data[,-1] <- as.data.frame(lapply(data[,-1], function(x) as.numeric(as.character(x))))
    
    # Format the numbers in the data frame to have no digits to the right of the decimal point
    data[,-1] <- format(round(data[,-1], 0), nsmall = 0, scientific = FALSE)
    
    # Return the modified data
    DT::datatable(data, rownames = FALSE,options = list(pageLength = 20, autoWidth = TRUE, columnDefs = list(list(className = 'dt-right', targets = '_all'))))
  })
  
  
  
  
  
}

# Run the Shiny application
shinyApp(ui = ui, server = server)

