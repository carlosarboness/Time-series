print_t_ratio <- function(m1) {
  t_ratios <- round(m1$coef / sqrt(diag(m1$var.coef)), 2)
  cat("\nT-ratios:", t_ratios)
  
  significant <- abs(t_ratios) > 2
  cat("\nSignificant?:", significant)
  
  # Troba el paràmetre amb el T-rati més baix
  
  abs_t_ratios <- abs(t_ratios)
  if (length(abs_t_ratios[abs_t_ratios < 2 & abs_t_ratios != 0]) > 0) {
    min_index <- which.min(abs_t_ratios[abs_t_ratios < 2 & abs_t_ratios != 0])
    cat(
      "\nParameter with lowest T-ratio:", t_ratios[which.min(abs_t_ratios)],
      " ( index", min_index, ")"
    )
  }
}

stability <- function(m, m_12) {
  cat(
    " Coeficients de m:   ", m$coef, "\n",
    "Coeficients de m_12:", m_12$coef, "\n\n",
    "Diferència del coeficients:", abs(m$coef - m_12$coef), "\n\n",
    "T-ratios de m1:   ", format(round(m$coef / sqrt(diag(m$var.coef)), 2), 2), "\n",
    "T-ratios de m1_12:", format(round(m_12$coef / sqrt(diag(m_12$var.coef)), 2), 2), "\n"
  )
}

#==============PLOT FUNCTIONS==============#

plot_ser <- function(ser, ylab ="", point_size = 0, red_line_at_0 = FALSE, save="")
{
suppressMessages({  
  suppressWarnings({  
  df <- data.frame(Year = time(ser), value = as.numeric(ser))
  
  p <- ggplot(df, aes(x = Year, y = value)) +
    geom_line(color = "darkblue", size = 0.8) +
    geom_point(color = "darkblue", size = point_size)  +
    scale_y_continuous(name = ylab, labels = scales::comma, limits = c(min(ser)-0.1*abs(min(ser)), max(ser)+0.1*max(ser))) +
    theme_economist_white(base_size = 14) +
    theme(panel.grid.major.y = element_line(color = "gray90", size = 0.5),
          panel.grid.minor.y = element_blank(),
          panel.border = element_blank(),
          panel.background = element_rect(fill = "white"),
          plot.background = element_rect(fill = "white"),
          axis.title.x = element_text(size = 14, face = "bold", margin = margin(t = 15, r = 0, b = 0, l = 0)),
          axis.title.y = element_text(size = 14, face = "bold", margin = margin(t = 0, r = 15, b = 0, l = 0)))
  
    if (red_line_at_0 == TRUE){
      p <- p + geom_hline(yintercept = 0, color = "red", size = 0.5, linetype = "dashed")  
    }
  
    p
    print(p)  
 
    if (save != ""){
      ggsave(save, width = 8, height = 6)
    }
  })
  })
}

decompose_ser <- function(ser, save = "")
{
suppressMessages({ 
  p <- ser %>%
    decompose() %>%
    autoplot() +
    scale_color_npg(name = "Color Palette", palette = "nrc") +
    theme_classic() +
    labs(title = "", x = "") +
    theme(plot.title = element_text(hjust = 0.5, size = 14),
          axis.title.x = element_text(size = 12),
          axis.text.x = element_text(size = 10),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_line(color="gray90", size=.1),
          panel.grid.minor.y = element_blank(),
          panel.background = element_rect(fill="white"),
          plot.background=element_rect(fill="white")) +
    theme(axis.line.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank()) +
    theme(legend.position="none") +
    theme(plot.margin=unit(c(1,0.5,1,1), "cm"))
  p
  print(p)
  if (save != ""){
    ggsave(save)
  }
})
}

bivariate_plot <- function(x, y, xlab, ylab, save="") 
{
suppressMessages({   
  
  df <- data.frame(x = x, y = y)
  sepal.labels <- labs(x = xlab, y = ylab, title = "")
  
  my.theme <- theme_classic()
  
  p <- ggplot(df, mapping = aes(x = x, y = y)) +
    geom_jitter() +  
    geom_smooth(span = 0.95) +
    sepal.labels + my.theme
  
  p
  print(p)  
  if (save != ""){
    ggsave(save, width = 8, height = 6)
  }
  
  })  
}

boxplot_ser <- function(ser, xlab, ylab, save="")
{
suppressMessages({ 
  df = data.frame(x = as.factor(floor(time(ser))), y = as.numeric(ser))
  
  p<- ggplot(df, aes(x = x, y = y)) +
    geom_boxplot(fill = "#0072B2", color = "black", outlier.color = "black") +
    scale_y_continuous(limits = c(min(df$y), max(df$y) * 1.1), expand = c(0, 0.05)) +
    labs(x = xlab, y = ylab) +
    theme_bw() +
    theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(color = "black"),
          axis.text = element_text(size = 8),
          axis.title = element_text(size = 12, face = "bold"),
          plot.title = element_text(size = 16, face = "bold", hjust = 0.5))
  p
  print(p)
  if (save != ""){
    ggsave(save, width = 8, height = 6)
  }
})
}

monthplot_ser <- function(ser, save="")
{
suppressMessages({ 
  
  p <- ggsubseriesplot(ser)+
    labs(x = "", y = "") +
    theme_grey()
  p
  print(p)
  if (save != ""){
    ggsave(save, width = 8, height = 6)
  }
})
}

seasonal_ser <- function(ser, save="")
{
  theme_set(theme_bw())
  
  p <- ggseasonplot(ser, year.labels = FALSE, continuous = TRUE)
  
  p <- p + labs(title = NULL, x = NULL, y = "") +
    theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
          axis.title.y = element_text(size = 12, vjust = 1.5),
          axis.text = element_text(size = 10),
          panel.grid = element_blank(),
          panel.border = element_rect(color = "black", fill = NA),
          panel.background = element_rect(fill = "white", color = NA),
          axis.text.x = element_text(size = 12, face="bold"))
  p
  print(p)
  if (save != ""){
    ggsave(save, width = 8, height = 6)
  }
}

plot_residuals <- function(resi, save="")
{
suppressMessages({ 
  p <- ggplot(data = data.frame(resi = resi), aes(x = 1:length(resi), y = resi)) +
    geom_line(color = "blue") +
    geom_hline(yintercept = 0, color = "black", size = 1) +
    geom_hline(yintercept = c(-3*sd(resi), 3*sd(resi)), color = "red", size = 1, linetype = "dashed") +
    theme_classic() +labs(x = "", y = "") +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_line(colour = "black"),
      axis.text = element_text(size = 10),
      axis.title = element_text(size = 12, face = "bold"),
      plot.title = element_text(size = 14, face = "bold")
    )  +
    scale_y_continuous(breaks = seq(-3*sd(resi), 3*sd(resi), by = sd(resi)),
                       limits = c(-3*sd(resi), 3*sd(resi))) +
    scale_y_continuous(labels = scales::number_format(accuracy = 0.01))
  p
  print(p)
  if (save != ""){
    ggsave(save, width = 8, height = 6)
  }
})  
}

plot_acf_pacf <- function(ser, lagmax, save = "", w = 10) {
  suppressMessages({
  par(mfrow = c(1, 2))
  acf(ser, ylim = c(-1, 1), lag.max = lagmax, lwd = 2, col = c(2, rep(1, 11)), main = "")
  pacf(ser, ylim = c(-1, 1), lag.max = lagmax, lwd = 2, col = c(rep(1, 11), 2), main = "")
  if (save != "") {
    pdf(file = save, width = w, height = 5)
    par(mfrow = c(1, 2))
    acf(ser, ylim = c(-1, 1), lag.max = lagmax, lwd = 2, col = c(2, rep(1, 11)), main = "")
    pacf(ser, ylim = c(-1, 1), lag.max = lagmax, lwd = 2, col = c(rep(1, 11), 2), main = "")
    dev.off()
  }
  })
}

plot_predicted <- function(ser, pr, tl, tu, minYear, maxYear, save = "", num_predicted = 12, margin = 0.1){
  suppressWarnings({
  df <- data.frame(year = c(time(ser), time(pre$pred)),
                   value = c(coredata(ser), pr),
                   lower = c(coredata(ser), tl),
                   upper = c(coredata(ser), tu),
                   type = c(rep("observed", length(ser)), rep("predicted", num_predicted)))
  
  min_plot <- min(subset(ser, index(ser) >= minYear & index(ser) <= maxYear))
  max_plot <- max(subset(ser, index(ser) >= minYear & index(ser) <= maxYear))
  
  min_plot <- min_plot - abs(margin*min_plot)
  max_plot <- max_plot + abs(margin*max_plot)
  
  # Create the plot
  p <- ggplot(df, aes(x = year, y = value, colour = type)) +
    geom_line() +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, linetype = 0) +
    scale_x_continuous(limits = c(minYear, maxYear), breaks = seq(minYear, maxYear, 1)) +
    scale_y_continuous(limits = c(min_plot, max_plot)) + 
    labs(x = "Year", y = "Billions of euros", colour = "") +
    theme_classic() +
    theme(legend.position = c(0.05, 0.95), legend.justification = c(0, 1)) +
    geom_point(alpha = 0.7, size = 2, shape = 21, fill = "white", stroke = 0.5) +
    scale_colour_manual(values = c("observed" = alpha("dodgerblue", 0.7), "predicted" = alpha("firebrick2", 0.5)))
  
  p
  print(p)
  
  if (save != ""){
    ggsave(save, width = 10, height = 6)
  }
  })
}

plot_forecast <- function(ser, pr, tl, tu, minYear, maxYear, save = "", num_predicted = 12, margin = 0.1){
  suppressWarnings({
    df <- data.frame(year = c(time(ser), time(pre$pred)),
                     value = c(coredata(ser), pr),
                     lower = c(coredata(ser), tl),
                     upper = c(coredata(ser), tu),
                     type = c(rep("observed", length(ser)), rep("predicted", num_predicted)))
    
    # Add the new row to df
    new_row <- data.frame(year = tail(index(ser), 1),
                          value = tail(ser, 1),
                          lower = tail(ser, 1),
                          upper = tail(ser, 1),
                          type = "predicted")
    names(new_row) <- c("year", "value", "lower", "upper", "type")
    df <- rbind(df, new_row)
    
    
    min_plot <- min(subset(ser, index(ser) >= minYear & index(ser) <= maxYear))
    max_plot <- max(subset(ser, index(ser) >= minYear & index(ser) <= maxYear))
    
    min_plot <- min_plot - abs(margin*min_plot)
    max_plot <- max_plot + abs(margin*max_plot)
    
    # Create the plot
    p <- ggplot(df, aes(x = year, y = value, colour = type, linetype = type)) +
      geom_line() +
      geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, linetype = 0) +
      scale_x_continuous(limits = c(minYear, maxYear), breaks = seq(minYear, maxYear, 1)) +
      scale_y_continuous(limits = c(min_plot, max_plot)) + 
      labs(x = "Year", y = "Billions of euros", colour = "", linetype = "") +
      theme_classic() +
      theme(legend.position = c(0.05, 0.95), legend.justification = c(0, 1)) +
      geom_point(alpha = 0.7, size = 2, shape = 21, fill = "white", stroke = 0.5) +
      scale_colour_manual(values = c("observed" = alpha("dodgerblue", 0.7), "predicted" = alpha("firebrick2", 0.5))) +
      scale_linetype_manual(values = c("observed" = "solid", "predicted" = "dashed"))
    
    
    p
    print(p)
    
    if (save != ""){
      ggsave(save, width = 10, height = 6)
    }
  })
}

res_qqplot <- function(res, save="")
{
  df <- data.frame(resi = res)
  
  my.theme <- theme(
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text = element_text(size = 10),
    axis.line = element_line(colour = "black"),
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )
  
  p<- ggqqplot(df, x = "resi",color = "#4C74BB" ) +
    ggtitle("") +
    labs(x = "Theoretical Quantiles", y = "Sample Quantiles") +
    scale_y_continuous(labels = scales::number_format(accuracy = 0.01))  +
    my.theme
  p
  print(p)
  if (save != ""){
    ggsave(save, width = 8, height = 6)
  }
}  

res_histogram <- function(resi, save="")
{
  p <- ggplot(data = data.frame(resi = resi), aes(x = resi)) +
    geom_histogram(aes(y = ..density..), bins = 50, fill = "#4C78A8", color = "black", alpha = 0.7) +
    geom_density(aes(color = "Residual distribution"), alpha = 1, size = 1, linetype = 1) +
    stat_function(aes(color = "Normal distribution"), fun = dnorm, args = list(mean = mean(resi), sd = sd(resi)), size = 1) +
    labs(x = "", y = "") +
    scale_color_manual(values = c("black","#F58518"),
                       guide = guide_legend(title = NULL, nrow = 2, override.aes = list(size = 1))) +
    guides(linetype = guide_legend(override.aes = list(size = 1))) +
    theme_classic() + 
    theme(axis.title = element_text(size = 14),
          axis.text = element_text(size = 12),
          legend.position = c(0.15, 0.89))
  p
  print(p)
  if (save != ""){
    ggsave(save, width = 8, height = 6)
  }
}


ljung_box_plot <- function(resi, save="")
{
  vector_of_pvalues <- c()
  for (i in 1:50){
    vector_of_pvalues <- c(vector_of_pvalues, Box.test(x = resi, type = "Ljung-Box", lag = i)$p.value)
  }
  
  # Plot p-values for each lag
  p <-ggplot(data = data.frame(lag = 1:50, p_value = vector_of_pvalues),
         aes(x = lag, y = p_value)) +
    geom_point(color = "#4C78A8", size = 2, shape = 1, stroke = 1.2) +
    geom_hline(yintercept = 0.05, linetype = "dashed", color = "#F58518") +
    scale_x_continuous(breaks = seq(5, 50, 5)) +
    labs(x = "Lag", y = "P-value") +
    theme_classic() +
    theme(axis.title = element_text(size = 14),
          axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10),
          axis.title.x = element_text(size = 14, face = "bold", margin = margin(t = 10, r = 0, b = 0, l = 0)),
          axis.title.y = element_text(size = 14, face = "bold", margin = margin(t = 0, r = 10, b = 0, l = 0)))
  p
  print(p)
  if (save != ""){
    ggsave(save, width = 8, height = 6)
  }
}

plot_ARMA_roots <- function(model, save = "")
{
  p<- autoplot(model) + theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))
  p
  print(p)
  if (save != ""){
    if (length(model$model$theta) == 0 || length(model$model$phi) == 0){
      ggsave(save, width = 5, height = 5)
    } else {
      ggsave(save, width = 10, height = 5)
    }
  }
}

plot_weights <- function(w, lag, save = "")
{
  data = data.frame(
    x=1:length(w),
    y=w
  )
  
  plot(data$x, data$y,
       xlim=c(0,lag) , ylim=c(min(w),max(w)), 
       pch=18, 
       cex=1.5, 
       col="#69b3a2",
       xlab="", ylab="",
       main="",
       cex.axis=0.8 
  )
  abline(h=0, col = "black", lty = 2)
  if (save != ""){
    pdf(file=save, width = 8, height = 4)
    plot(data$x, data$y,
         xlim=c(0,lag) , ylim=c(min(w),max(w)), 
         pch=18, 
         cex=1.5, 
         col="#69b3a2",
         xlab="", ylab="",
         main="",
         cex.axis=0.8 
    )
    abline(h=0, col = "black", lty = 2)
    dev.off()
  }
}

plot_ser_lin <- function(ser, serlin, ylab="", point_size = 0, save="") {
  suppressMessages({
    suppressWarnings({
      df <- data.frame(Year = time(ser), value = as.numeric(ser))
      dflin <- data.frame(Year = time(serlin), value = as.numeric(serlin))
      
      p <- ggplot() +
        geom_line(data = df, aes(x = Year, y = value), color = "darkblue", size = 0.7) +
        geom_line(data = dflin, aes(x = Year, y = value), color = "red", size = 0.7) +
        geom_point(data = df, aes(x = Year, y = value), color = "darkblue", size = point_size)  +
        scale_y_continuous(name = ylab, labels = scales::comma, limits = c(min(ser, serlin)-0.1*abs(min(ser, serlin)), max(ser, serlin)+0.1*max(ser, serlin))) +
        theme_economist_white(base_size = 14) +
        theme(panel.grid.major.y = element_line(color = "gray90", size = 0.5),
              panel.grid.minor.y = element_blank(),
              panel.border = element_blank(),
              panel.background = element_rect(fill = "white"),
              plot.background = element_rect(fill = "white"),
              axis.title.x = element_text(size = 14, face = "bold", margin = margin(t = 15, r = 0, b = 0, l = 0)),
              axis.title.y = element_text(size = 14, face = "bold", margin = margin(t = 0, r = 15, b = 0, l = 0)))
      
      print(p)
      
      if (save != ""){
        ggsave(save, width = 8, height = 6)
      }
    })
  })
}
