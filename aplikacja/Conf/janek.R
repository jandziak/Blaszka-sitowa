library(magrittr)
library(plotly)
library(DT)
library(ggplot2)
library(dplyr)
library(shinydashboard)
library(shiny)
library(FSelector)

dane1 <- read.csv("Conf/dane analiza medyczna.csv", encoding = "UTF-8")
theme_set(theme_light())
theme_update(legend.position = c(0, 1), 
             legend.justification = c(0, 1))
scale_fill_man<- scale_fill_brewer(palette="Blues")
scale_color_man<- scale_colour_brewer(palette="Blues")



# dir()
# zalezna <- "CWG"
# niezalezna <- "Wiek"
# formula_liniowa <- as.formula(paste(zalezna, "~", niezalezna))
# formula_kwadratowa <- as.formula(paste(zalezna, "~", niezalezna,  " + I(", niezalezna, "^2)" ))


#' Calculate res for leave one out crossvalidation without i-th observation
calculate_res_loo <- function(dane, i, formula,zalezna){
  model <- lm(data = dane[-i,], formula = formula)
  #print(model)
  res <- dane[i,zalezna] - predict(object = model, newdata = dane[i,])
  res
}

#calculate_res_loo(dane1, 5, formula = formula_kwadratowa,zalezna)
#sapply(1:dim(dane1)[1], function(x) calculate_res_loo(dane1, x, formula = formula_liniowa,zalezna))

#' Function to evaluate res leave one out for whole data set
evaluete_model <- function(dane, formula, zalezna){
  res <- 
    sapply(1:dim(dane)[1], 
           function(x) calculate_res_loo(dane, x, formula = formula, zalezna))
  #plot(res)
  sqrt(sum(res^2))/length(res)
}
#evaluete_model(dane1, formula_liniowa, zalezna)

#' Function to evaluate models and calculate mse for leave one out crossvalidation
fit_lm_model <- function(dane,
                         zalezna,
                         niezalezna#,
                         #grupujaca = NULL,
                         #formula_extra = NULL,
                         #sciezka_zapisu = NULL
                         ){
  # Ustalanie formul do przeliczen
  id_1 <- is.na(dane[,zalezna])
  id_2 <- is.na(dane[,niezalezna])
  id <- which(id_1 + id_2 ==0)
  dane <- dane[id, c(zalezna,niezalezna)]
  #print(dane[,c(zalezna,niezalezna)])
  formula_liniowa <- as.formula(paste(zalezna, "~", niezalezna))
  formula_kwadratowa <- as.formula(paste(zalezna, "~", niezalezna,  " + I(", niezalezna, "^2)" ))
  formula_exp <- as.formula(paste(zalezna, "~", " I(exp(", niezalezna, "))" ))
  
  # Evaluacja modelu przy użyciu metody leave one out
  # Zwraca błąd średniokwadratowy dla każdej z metod
  blad_liniowej <- evaluete_model(dane, formula_liniowa,zalezna)
  blad_kwadratowej <- evaluete_model(dane, formula_kwadratowa, zalezna)
  #blad_exp <- evaluete_model(dane, formula_exp,zalezna)
  
  # Dopasowywanie finalnych modeli
  model_liniowy <- lm(data = dane, formula = formula_liniowa)
  model_kwadratowy <- lm(data = dane, formula = formula_kwadratowa)
  #model_exp <- lm(data = dane, formula = formula_exp)
  
  # Obliczanie reszt dla modeli
  reszty_kwadratowy <- predict(model_kwadratowy) - dane[, zalezna]
  resty_liniowy <- predict(model_liniowy) - dane[, zalezna]
  #reszty_exp <- predict(model_exp) - dane[, zalezna]
  
  # Test korelacji dla zmiennej 
  test_korelacji <- cor.test(dane[, zalezna], dane[, niezalezna])

  return(list(
    model_liniowy = model_liniowy,
    #model_exp = model_exp,
    model_exp = 0,
    model_kwadratowy = model_kwadratowy,
    test_korelacji = test_korelacji,
    blad_exp = 0,
    #blad_exp = blad_exp,
    blad_liniowej = blad_liniowej,
    blad_kwadratowej = blad_kwadratowej
  ))  
}
#lm_fitted <- fit_lm_model(dane = dane1, zalezna = "CWG", niezalezna = "Wiek")

#' Function plotting diagnostics for particular model
RegressionPlots <- function(fit){
  
  # Extract fitted values from lm() object
  Fitted.Values <-  fitted(fit)
  
  # Extract residuals from lm() object
  Residuals <-  resid(fit) 
  
  # Extract standardized residuals from lm() object
  Standardized.Residuals <- MASS::stdres(fit)  
  
  # Extract fitted values for lm() object
  Theoretical.Quantiles <- qqnorm(Residuals, plot.it = F)$x
  
  # Square root of abs(residuals)
  Root.Residuals <- sqrt(abs(Standardized.Residuals))
  
  # Calculate Leverage
  Leverage <- lm.influence(fit)$hat
  
  # Create data frame 
  # Will be used as input to plot_ly
  
  regMat <- data.frame(Fitted.Values, 
                       Residuals, 
                       Standardized.Residuals, 
                       Theoretical.Quantiles,
                       Root.Residuals,
                       Leverage)
  
  # Plot using Plotly
  
  # Fitted vs Residuals
  # For scatter plot smoother
  LOESS1 <- loess.smooth(Fitted.Values, Residuals)
  
  plt1 <- regMat %>% 
    plot_ly(x = Fitted.Values, y = Residuals, 
            type = "scatter", mode = "markers", hoverinfo = "x+y", name = "Data",
            marker = list(size = 10, opacity = 0.5), showlegend = F) %>% 
    
    add_trace(x = LOESS1$x, y = LOESS1$y, type = "scatter", mode = "line", name = "Smooth",
              line = list(width = 2)) %>% 
    
    layout(title = "Residuals vs Fitted Values", plot_bgcolor = "#e6e6e6", width = 1000)
  
  # QQ Pot
  plt2 <- regMat %>% 
    plot_ly(x = Theoretical.Quantiles, y = Standardized.Residuals, 
            type = "scatter", mode = "markers", hoverinfo = "x+y", name = "Data",
            marker = list(size = 10, opacity = 0.5), showlegend = F) %>% 
    
    add_trace(x = Theoretical.Quantiles, y = Theoretical.Quantiles, type = "scatter", mode = "line", name = "",
              line = list(width = 2)) %>% 
    
    layout(title = "Q-Q Plot", plot_bgcolor = "#e6e6e6")
  
  # Scale Location
  # For scatter plot smoother
  LOESS2 <- loess.smooth(Fitted.Values, Root.Residuals)
  
  plt3 <- regMat %>% 
    plot_ly(x = Fitted.Values, y = Root.Residuals, 
            type = "scatter", mode = "markers", hoverinfo = "x+y", name = "Data",
            marker = list(size = 10, opacity = 0.5), showlegend = F) %>% 
    
    add_trace(x = LOESS2$x, y = LOESS2$y, type = "scatter", mode = "line", name = "Smooth",
              line = list(width = 2)) %>% 
    
    layout(title = "Scale Location", plot_bgcolor = "#e6e6e6", width = 1000)
  
  # Residuals vs Leverage
  # For scatter plot smoother
  LOESS3 <- loess.smooth(Leverage, Residuals)
  
  plt4 <- regMat %>% 
    plot_ly(x = Leverage, y = Residuals, 
            type = "scatter", mode = "markers", hoverinfo = "x+y", name = "Data",
            marker = list(size = 10, opacity = 0.5), showlegend = F) %>% 
    
    add_trace(x = LOESS3$x, y = LOESS3$y, type = "scatter", mode = "line", name = "Smooth",
              line = list(width = 2)) %>% 
    
    layout(title = "Leverage vs Residuals", plot_bgcolor = "#e6e6e6")
  
  plt = list(p1=plt1, p2=plt2, p3=plt3, p4=plt4)
  return(plt)
}


#plt = RegressionPlots(lm_fitted$model_liniowy)
#plt

plot_lm <- function(dane, zalezna, niezalezna){
  id_1 <- is.na(dane[,zalezna])
  id_2 <- is.na(dane[,niezalezna])
  id <- which(id_1 + id_2 ==0)
  dane <- dane[id, ]
  title <- paste("Regression of ", zalezna, " vs ", niezalezna, sep = "")
  p <- ggplot(dane, aes_string(y=zalezna, x=niezalezna)) + 
    ggtitle(title) + scale_fill_man + 
    scale_color_man + labs(fill="", color = "") 
  p1 <- p + stat_smooth(method="lm", formula=y ~ x+I(x^2)) + geom_point()
  p2 <- p + stat_smooth(method="lm") + geom_point()
  #p3 <- p + stat_smooth(method="lm", formula=y ~ I(exp(x))) + geom_point() 
  p1
  p1 <- ggplotly(p1)
  p2 <- ggplotly(p2)
 # p3 <- ggplotly(p3)
  plt = list(p1=p1, p2=p2)#, p3=p3)
  return(plt)
}

q75 <-function(x) return(quantile(x,0.75,na.rm = TRUE))
q25 <-function(x) return(quantile(x,0.25,na.rm = TRUE))
mi <- function(x) return(min(x, na.rm = TRUE))
ma <- function(x) return(max(x, na.rm = TRUE))
mea <- function(x) return(mean(x, na.rm = TRUE))
std <- function(x) return(sd(x, na.rm = TRUE))
counter <- function(x) return(sum(x!="", na.rm = TRUE))

BasicStats <- function(data, variables, grouping = NULL) {
  if(variables == "All"){
    variables <- names(data)[as.vector(sapply(names(data), function(x) is.numeric(data[, x])))]
  }
  if(is.null(grouping)) {
    used_data <- data[variables]
    base_stats <- data.frame(variables)
    base_stats <- cbind(base_stats,"Full Set", apply(used_data,2,mi),apply(used_data,2,q25),
                        apply(used_data,2,q75),apply(used_data,2,ma),
                        apply(used_data,2,mea), apply(used_data,2,std), apply(used_data,2,counter))
    base_stats <- base_stats[,c(2,1,3,4,5,6,7,8,9)]
    names(base_stats)<- c( "Group", "Variable", "Min", "Q25","Q75","Max","Mean","Std_Dev", "Count")
    rownames(base_stats) <- c(1:length(variables))
  } else {
    used_data <- data[c(variables,grouping)]
    amount <- length(variables)+1
    base_stats <- used_data %>% group_by_(grouping)  %>% summarise_each(funs(mi)) %>% gather(Variable, Min, 2:amount)
    base_stats <- used_data %>% group_by_(grouping)  %>% summarise_each(funs(q25)) %>% gather(Variable, Q25, 2:amount) %>% right_join(base_stats, by=c(grouping, "Variable"))
    base_stats <- used_data %>% group_by_(grouping)  %>% summarise_each(funs(q75)) %>% gather(Variable, Q75, 2:amount) %>% right_join(base_stats, by=c(grouping, "Variable"))
    base_stats <- used_data %>% group_by_(grouping)  %>% summarise_each(funs(ma)) %>% gather(Variable, Max, 2:amount) %>% right_join(base_stats, by=c(grouping, "Variable"))
    base_stats <- used_data %>% group_by_(grouping)  %>% summarise_each(funs(mea)) %>% gather(Variable, Mean, 2:amount) %>% right_join(base_stats, by=c(grouping, "Variable"))
    base_stats <- used_data %>% group_by_(grouping)  %>% summarise_each(funs(std)) %>% gather(Variable, Std_Dev, 2:amount) %>% right_join(base_stats, by=c(grouping, "Variable"))
    base_stats <- used_data %>% group_by_(grouping)  %>% summarise_each(funs(counter)) %>% gather(Variable, Count, 2:amount) %>% right_join(base_stats, by=c(grouping, "Variable"))
    base_stats <- base_stats[,c(1,2,9,8,7,6,5,4,3)]
    names(base_stats)[1]<-"Group"
  }
  return(base_stats)
}
#plot_lm(dane=dane1, zalezna=zalezna, niezalezna=niezalezna)


box_2_plot <- function(df, var_1, var_2, group = "all"){
  if(group != "all")
  {df<-df[df$metoda==group,]}
  # {df <- df %>% filter(grupa == group) }
  values <- c(df[, var_1], df[,var_2])
  names <- factor(c(rep(var_1, length(df[,var_1])),
                    rep(var_2, length(df[,var_2]))))
  temp_df <- data.frame(values=values, names=names)
  p <- ggplot(temp_df, aes(names, values))
  p <- p + geom_boxplot(aes(fill = names)) + geom_jitter()
  p <- p + theme(legend.justification=c(1,0), legend.position=c(1,0))
  p <- p + xlab("Variables") +ylab("Values") 
  p <- p + scale_fill_discrete(name="Variables")
  return(p)
}

box_plot<- function(data, variable, group = "None"){
  values <- data[, variable]
  if(group != "None")
  {
    names <-data[,group]
  } else{
    names <- factor(c(rep(variable, length(data[,variable]))))
  }
  temp_df <- data.frame(values=values, names=names)
  p <- ggplot(temp_df, aes(names, values))
  p <- p + geom_boxplot(aes(fill = names)) + geom_jitter()
  p <- p + theme(legend.justification=c(1,0), legend.position=c(1,0))
  p <- p + xlab("Categories") +ylab("Values") 
  p <- p + scale_fill_discrete(name="Categories")
  return(p)
}

wilcox_test <- function(var_1, var_2, data){
  test <- wilcox.test(data[, var_1], data[, var_2])
  stat <- data.frame(test$statistic)
  p_val <- data.frame(round(test$p.value, 3))
  n <- length(data[, var_1])
  wynik <- data.frame(Var_1 = var_1, Var_2 = var_2, n = n, 
                      'p-value' = as.numeric(p_val), 'Null hypothesis' = "Difference between the pairs follows \n a symmetric distribution around zero")
  return(wynik)
}

u_mann_whitney_test <- function(var_1, factor_1, data){
  formula <- as.formula(paste(var_1, "~", factor_1, sep = " "))
  test <- wilcox.test(data = data, formula)
  stat <- data.frame(test$statistic)
  p_val <- data.frame(round(test$p.value, 3))
  n <- length(data[, var_1])
  wynik <- data.frame(Variable = var_1, Grups = factor_1,  n = n,
                      'p-value' = as.numeric(p_val), 'Null hypothesis' = "Groups have identical distributions")
  return(wynik)
}
