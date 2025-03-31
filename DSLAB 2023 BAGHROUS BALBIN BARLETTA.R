# importazione dataset e preprocessing

{

ecommerce <- read.csv('ecommerceOR.csv', sep = ';')
View(ecommerce)

#rimuovo le righe con data nulla

ecommerce <- ecommerce[ecommerce$data != '',] 

# lista settori

{
listSett <- unique(ecommerce$settore); listSett

df <- data.frame(Sector = character(0), NumberOfRows = numeric(0))

for (i in 1:length(listSett)) {
  sector <- listSett[i]
  num_rows <- nrow(ecommerce[ecommerce$settore == sector,])
  
  temp_df <- data.frame(Sector = sector, NumberOfRows = num_rows)
  
  df <- rbind(df, temp_df)
  if (i == 30) {
    rm(temp_df)
  }
}
}

# riordina per settore in ordine decrescente

df <- df[order(df$NumberOfRows, decreasing = TRUE), ]

# crea lista dei 3 settori più numerosi

newList <- df$Sector[1:3]; newList

}

# pesca

{
  
  # Preparazione dataframe
  
  {
    pesca <- ecommerce[ecommerce$settore == newList[1],]; View(pesca)
    
    # rimozione colonna settore, reset indice e conversione data in as.Date
    
    pesca <- pesca[1:length(pesca)-1]
    row.names(pesca) <- NULL
    pesca$data <- as.Date(x = pesca$data, format = "%d-%m-%Y")
    
    library(ggplot2)
    library(lubridate)
    
    plot(pesca$data, pesca$totale, type = 'l', xlab = "Time", ylab = "Profits")
    
    # creazione colonna di anno
    
    pesca$anno <- year(pesca$data)
    
    # controllo numero osservazioni per anno (369 giorni?)
    
    for (i in unique(pesca$anno)) {
      cat(i,": ", nrow(pesca[pesca$anno == i,]),"\n")
    }
    
    # rimozione duplicati
    
    dupPesca <- pesca[duplicated(pesca$data) | duplicated(pesca$data, fromLast = TRUE), ]
    
    for (i in seq(from = 1, to = nrow(dupPesca), by = 2)) {
      pesca[pesca$data == dupPesca$data[i],][1,2] <- pesca[pesca$data == dupPesca$data[i],][1,2] + pesca[pesca$data == dupPesca$data[i],][2,2]
    }
    for (i in seq(from = 2, to = nrow(dupPesca), by = 2)) {
      pesca <- subset(pesca, !(pesca$data == pesca[pesca$data == dupPesca$data[i],][1,1] & pesca$totale == pesca[pesca$data == dupPesca$data[i],][2,2]))
    }
    
    
    # rimozione anno 2014 e 2015
    
    for (i in c(2014,2015)) {
      pesca <- pesca[pesca$anno != i, ]
    }
    
    row.names(pesca) <- NULL
    
    # ricontrollo numero osservazioni (2019 mancano due giorni)
    
    for (i in unique(pesca$anno)) {
      cat(i,": ", nrow(pesca[pesca$anno == i,]),"\n")
    }
    
    #grafico con linea media
    
    plot(pesca$data, pesca$totale, type = 'l', xlab = "Years", ylab = "Profits"); abline(h = mean(pesca$totale), col = "red", lwd = 2)
    
    #the variance appears to be constant over time, anche la media
    
    # stazionarietà dati
    
    library(tseries)
    adf.test(pesca$totale)
    
    # it appears that your time series does not have a stochastic trend and is stationary.
    
    acf(pesca$totale) # no white noise perchè le linee sono tutte fuori dalle linee tratteggiate
    
  }
  
  # Modello ARIMA
  
  {
    
    # Test Modello
    
    {
      
      library(forecast)
      arimaPesca <- auto.arima(pesca$totale, seasonal = FALSE)
      
      #grafico (da rimuovere)
      
      plot(pesca$totale, type = 'l', xlab = "Years", ylab = "Profits")
      lines(arimaPesca$fitted, col = 'red')
      
      # calcolo rmse, per valutare la precisione del modello
      
      rmse <- function(actual, predicted) {
        sqrt(mean((predicted - actual)^2))# /(max(actual)-min(actual)) per normalizzare?
      }
      rmseNorm <- function(actual, predicted) {
        sqrt(mean((predicted - actual)^2))/(max(actual)-min(actual))
      }
      
      rmse(pesca$totale, arimaPesca$fitted)
      rmseNorm(pesca$totale, arimaPesca$fitted)
      
      #calcolo acf sui residui
      
      acf(arimaPesca$residuals)
      
    }
    
    # Previsione
    
    {
      # forecast, dei restanti giorni del 2023
      forecastArimaPesca <- forecast(arimaPesca, h = 233)
      forecastArimaPesca <- data.frame(data = seq(pesca$data[nrow(pesca)]+1, by = "1 day", length.out = 233),
                                       totale = as.numeric(forecastArimaPesca$mean))
      
      
      plot(tail(pesca$data, 132), tail(pesca$totale, 132), type = "l", col = "blue",
           xlab = "Data", ylab = "Totale")
      
      # Add plot for the last 233 days
      lines(forecastArimaPesca$data, forecastArimaPesca$totale, type = "l", col = "red")
      
      # combino pesca e i restanti giorni stimati del 2023
      combinedPesca <- rbind(pesca[1:2], forecastArimaPesca)
      
      # plot dell'anno 2023
      plot(tail(combinedPesca$data, 365), tail(combinedPesca$totale, 365), type = "l", xlab = "Data", ylab = "Totale")
      
      # evidenzio i valori restanti stimati del 2023
      lines(forecastArimaPesca$data, forecastArimaPesca$totale, type = "l", col = "red")
    }
    
  }
  
  # Modello Prophet
  
  {
    
    # Test Modello
    
    {
      
      library(prophet)
      library(timetk)
      
      # Copia solo le prime due colonne di pesca
      pesca_prophet <- pesca[, 1:2]
      
      # Rinomina le colonne
      colnames(pesca_prophet) <- c('ds', 'y')
      
      # Imposta la proporzione di dati da utilizzare per il set di addestramento
      train_proportion <- 0.8
      
      # Calcola il numero di osservazioni per il set di addestramento
      train_size <- round(nrow(pesca_prophet) * train_proportion)
      
      # Dividi il dataframe in set di addestramento e test
      train_pesca <- pesca_prophet[1:train_size, ]
      test_pesca <- pesca_prophet[(train_size + 1):nrow(pesca_prophet), ]
      
      # Inizializza il modello
      modello_pesca <- prophet()
      
      # Addestra il modello sul set di addestramento
      modello_pesca <- fit.prophet(modello_pesca, train_pesca)
      
      # Crea un dataframe per le previsioni future
      futuro_pesca <- data.frame(ds = test_pesca$ds)
      
      # Ottieni le previsioni
      previsioni_pesca <- predict(modello_pesca, futuro_pesca)
      
      # RMSE
      rmse_value <- sqrt(mean((test_pesca$y - previsioni_pesca$yhat)^2))
      cat("RMSE:", rmse_value, "\n")
      
      # RMSE percentuale
      
      rmse_percentuale <- (rmse_value / mean(test_pesca$y)) * 100
      cat("%RMSE:", rmse_percentuale, "%\n")
      
      # MAPE
      mape_value <- mean(abs((test_pesca$y - previsioni_pesca$yhat) / test_pesca$y)) * 100
      cat("MAPE:", mape_value, "%\n")
      
      # MdAPE
      mdape_value <- median(abs((test_pesca$y - previsioni_pesca$yhat) / test_pesca$y)) * 100
      cat("MdAPE:", mdape_value, "%\n")
      
    }
    
    # Previsione
    
    {
      library(ggplot2)
      library(cowplot)
      
      # Inizializza il modello Prophet
      modello_prophet <- prophet()
      
      # Addestra il modello con i dati esistenti
      modello_prophet <- fit.prophet(modello_prophet, pesca_prophet)
      
      # Crea un dataframe per le previsioni future
      futuro_pesca_pr <- make_future_dataframe(modello_prophet, periods = 232)
      
      # Ottieni solo le previsioni future
      previsioni_prophet <- predict(modello_prophet, futuro_pesca_pr)
      previsioni_futures <- subset(previsioni_prophet, ds > max(pesca_prophet$ds))
      
      # grafico
      
      pesca_prophet$ds <- as.Date(pesca_prophet$ds)
      previsioni_futures$ds <- as.Date(previsioni_futures$ds)
      
      ggplot() +
        geom_line(data = pesca_prophet, aes(x = ds, y = y), color = "blue") +
        geom_ribbon(data = previsioni_futures, aes(x = ds, ymin = yhat_lower, ymax = yhat_upper), fill = "red", alpha = 0.3) +
        geom_line(data = previsioni_futures, aes(x = ds, y = yhat), color = "red") +
        ggtitle("Previsioni Pesca") +
        theme(plot.title = element_text(hjust = 0.5))
      
      #grafico trend annuale, settimanale e mensile
      
      prophet_plot_components(modello_prophet, previsioni_prophet)
      
    }
    
  }
  
}



# calcio

{
  
calcio <- ecommerce[ecommerce$settore == newList[2],]; View(calcio)
calcio <- calcio[1:length(calcio)-1]
row.names(calcio) <- NULL
calcio$data <- as.Date(x = calcio$data, format = "%d-%m-%Y")

plot(calcio$data, calcio$totale, type = 'l', xlab = "Time", ylab = "Profits")

}



# casual

{
  
casual <- ecommerce[ecommerce$settore == newList[3],]; View(casual)
casual <- casual[1:length(casual)-1]
row.names(casual) <- NULL
casual$data <- as.Date(x = casual$data, format = "%d-%m-%Y")

plot(casual$data, casual$totale, type = 'l', xlab = "Time", ylab = "Profits")

}

  