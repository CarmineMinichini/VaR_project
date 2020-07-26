# Libraries
library(tidyverse)
library(reshape2)
library(fOptions)
library(plotly)
library(rio)
# setto la working directory
setwd("~/Desktop/Materiale Uni/Gestione del Rischio/Rischio Finanziario e Assicurativo/R Project")

# Importare gli sheets dal file excel
dataset <- import_list("volat.xls",setclass = 'tbl')

# trasformare le variabili
for( i in 1:length(dataset))
  {
  dataset = lapply(dataset,na.omit)
  dataset[[i]][,c(2:14,16)] = dataset[[i]][,c(2:14,16)]/100
  }

# Dati al 01-05-2020
x1month=as.numeric(tail(dataset$`1M`[2:16],1))
x3month=as.numeric(tail(dataset$`3M`[2:16],1))
x6month=as.numeric(tail(dataset$`6M`[2:16],1))
x9month=as.numeric(tail(dataset$`9M`[2:16],1))
x12month=as.numeric(tail(dataset$`12M`[2:16],1))
x18month=as.numeric(tail(dataset$`18M`[2:16],1))
x24month=as.numeric(tail(dataset$`24M`[2:16],1))

# lista dati scadenze 1-24 mesi
data=list(x1month,x3month,x6month,x9month,x12month,x18month,x24month)

###################################################################### PUNTO 1
# Punto 1 ####
options_plot= function(optionstype) 
{
  #moneyness
  moneyness= seq(70,130,5)/100
  #time
  time=c(1/12,1/4,2/4,3/4,1,1.5,2)
  #prezzo del sottostante
  price =2830.71
  # prezzo di esercizio
  K = (price*moneyness)
  #matrix
  matrix = matrix(0, nrow = 13, ncol = 7)
  for (k in 1:length(data))
  {
    matrix[,k]= GBSOption(TypeFlag=optionstype,
                          S=price, X=K ,
                          Time=time[[k]],
                          r=data[[k]][15],
                          b=data[[k]][15],
                          sigma=data[[k]][1:13])@price
  }
  colnames(matrix)= c('one_month','three_months','six_months',
                      'nine_months','twelve_months','eighteen_months','twentyfour_months')
  matrix=data.frame(moneyness,matrix)
  melt_matrix= melt(matrix,id.vars = 'moneyness',variable.name='time')
  plot = melt_matrix %>% ggplot(aes(x=moneyness*100, y=value, color=time)) + 
    geom_point() + 
    stat_smooth() +
    scale_color_manual(labels=c('1 month','3 months','6 months','9 months ','12 months','18 months','24 months'),
                       values= c('black','purple','dodgerblue4','coral','green','yellow','red')) +
    labs(x='Moneyness',y='Price',title=ifelse(optionstype=='p','Put\'s Prices','Call\'s Prices'),colour='') +
    theme_classic() +
    theme(legend.position="bottom",
          legend.key.size = unit(0.1, "cm"),
          legend.key.width = unit(2,"cm"))
  out <- list("Price's Plot" = plot, "Price's Matrix" = matrix)
  return(out)
}

# Plot Opzioni B&S
options_plot('p')
# Punto 1-Greek's Plot ####
greeks_plot= function(optionstype,greektype) 
{
  #moneyness
  moneyness= seq(70,130,5)/100
  #time
  time=c(1/12,1/4,2/4,3/4,1,1.5,2)
  #prezzo del sottostante
  price =2830.71
  # prezzo di esercizio
  K = (price*moneyness)
  #matrix
  matrix = matrix(0, nrow = 13, ncol = 7)
  for (k in 1:length(data))
  {
    matrix[,k]= GBSGreeks(Selection=greektype,
                          TypeFlag=optionstype,
                          S=price, X=K ,
                          Time=time[[k]],
                          r=data[[k]][15],
                          b=data[[k]][15],
                          sigma=data[[k]][1:13])
  }
  colnames(matrix)= c('one_month','three_months','six_months',
                      'nine_months','twelve_months','eighteen_months','twentyfour_months')
  matrix=data.frame(moneyness,matrix)
  melt_matrix= melt(matrix,id.vars='moneyness',variable.name='time')
  plot = melt_matrix %>% ggplot(aes(x=moneyness*100,y=value,color=time)) + 
    geom_point() + 
    stat_smooth() +
    scale_color_manual(labels=c('1 month','3 months','6 months','9 months ','12 months','18 months','24 months'),
                       values= c('black','purple','dodgerblue4','coral','green','yellow','red')) +
    labs(x='Moneyness',y= ifelse(greektype=='delta',bquote(Delta),bquote(nu)),
         title=ifelse(greektype=='delta',bquote(Delta),bquote(nu)),
         colour='') + 
    theme_classic() +
    theme(legend.position="bottom",
          legend.key.size = unit(0.1, "cm"),
          legend.key.width = unit(2,"cm"))
  out <- list("Price's Plot" = plot, "Price's Matrix" = matrix)
  return(out)
}

# Per visualizzare le diverse greche selezionare la diversa greca in greektype
#Plot Greche B&S
greeks_plot(optionstype = 'c',greektype = 'vega')

############################################################################################### PUNTO 2
# Punto 2#####
# Moneyness
moneyness= seq(70,130,5)
# Scadenze
time= c(1,3,6,9,12,18,24)
#  bind vol datas
vol_surf_data=c(x1month[1:13],x3month[1:13],x6month[1:13],
                x9month[1:13],x12month[1:13],x18month[1:13],
                x24month[1:13])
# matrice 13x7 volatilità 
imp_vol= matrix(vol_surf_data,nrow=13, byrow=F)
imp_vol

axz = list(title ='Volatility',nticks = 10,range = c(0, 0.7))
axx = list(title = 'Times',nticks = 6,range = c(0, 24))
axy = list(title = 'Moneyness',nticks=6)

# Plot Surface volatility
vol_surface = plot_ly(type = 'surface', z = imp_vol, y = moneyness,
                      x = time,
                      colors = 'magma', opacity = 1, showscale=F,
                      contours = list(z = list(show=TRUE,usecolormap=TRUE,highlightcolor="#ff0000",
                          project=list(z=TRUE))))

vol_surface %>% 
  layout(title='Volatility Surface',scene = list(zaxis= axz, xaxis = axx, yaxis = axy,
                                                   aspectratio = list(x = .9, y = .8, z = 0.6)))

############################################################################################ PUNTO 3
# Punto 3 ####
draw_payoff= function(S,K)
  {
  # Sequenza 0-K*2
  stock=seq(0,K*2,length.out=11)
  payoff=rep(0,length(stock))
  data_payoff= matrix(0,ncol=4,nrow=11)
  for(i in 1:length(stock))
    {
    if(stock[i]< K) payoff[i] = max(stock[i],K)
    if(stock[i]>= K) payoff[i] = max(stock[i],K)
  for (k in 1:4)
    {
    data_payoff[i,k] = payoff[i]
    }
    }
  data_payoff=data.frame(stock,data_payoff)
  names(data_payoff) = c('StockPrice','6 months','12 months','18 months','24 months')
  df_payoff_melt = melt(data_payoff,id.vars='StockPrice',variable.name='Time')
  data_dashed = data.frame(stock_dashed=data_payoff$StockPrice[5:11])
  #Plot Payoff diverse scadenze
  plt_payoff= ggplot() + geom_line(data=df_payoff_melt,aes(x=StockPrice,y=value,color=Time)) +
    geom_line(data=data_dashed,aes(x=stock_dashed,y=stock_dashed),linetype='dashed') + 
    geom_hline(yintercept=2830.71, linetype="dashed", color = "grey40") +
    scale_color_manual(labels=c('6 months','12 months','18 months','24 months'),
                       values=c('dodgerblue4','green','yellow','red')) + 
    labs(title=bquote('Payoff'~tau[+0.5]~tau[+1]~tau[+1.5]~tau[+2]), y='Payoff',colour='') +
    theme_classic() +
    theme(legend.position="bottom",
          legend.key.size = unit(1, "cm"),
          legend.key.width = unit(1,"cm"))
   out=list(DataPayoff=data_payoff,PlotPayoff=plt_payoff)
   return(out)
}

draw_payoff(S=2830.71,K=2830.71)
########################################################################################## PUNTO 4
# Punto 4 ####
# Valore del Portafoglio per ogni cross-section
data2=list(dataset[[3]],dataset[[5]],dataset[[6]],dataset[[7]])
matrix= matrix(0,nrow=669,ncol=1)
for (k in 1:669) 
  {
  for (i in 1:length(data2))
    {
    matrix[k,] = 4*as.numeric(data2[[i]][k,15]) + 
      GBSOption(TypeFlag='p',S=as.numeric(data2[[1]][k,15]),
                 X=as.numeric(data2[[1]][k,15]),
                 Time=0.5,
                 r=as.numeric(data2[[1]][k,16]) ,
                 b=as.numeric(data2[[1]][k,16]) ,
                 sigma=as.numeric(data2[[1]][k,8]))@price +
      GBSOption(TypeFlag='p',S=as.numeric(data2[[2]][k,15]),
                X=as.numeric(data2[[2]][k,15]),
                Time=1,
                r=as.numeric(data2[[2]][k,16]),
                b=as.numeric(data2[[2]][k,16]) ,
                sigma=as.numeric(data2[[2]][k,8]))@price +
      GBSOption(TypeFlag='p',S=as.numeric(data2[[3]][k,15]),
                 X=as.numeric(data2[[3]][k,15]),
                 Time=1.5,
                 r=as.numeric(data2[[3]][k,16]),
                b=as.numeric(data2[[3]][k,16]) ,
                 sigma=as.numeric(data2[[3]][k,8]))@price +
      GBSOption(TypeFlag='p',S=as.numeric(data2[[4]][k,15]),
                 X=as.numeric(data2[[4]][k,15]),
                 Time=2,
                 r=as.numeric(data2[[4]][k,16]),
                 b=as.numeric(data2[[4]][k,16]) ,
                sigma=as.numeric(data2[[4]][k,8]))@price
    }
}

portfolio_cs= data.frame(as.Date(dataset$`1M`$Timestamp),matrix)
names(portfolio_cs)= c('dates','ValorePortafoglio')

portfolio_cs %>% ggplot(aes(x=dates,y=ValorePortafoglio)) + geom_line() +
  labs(x='Dates',y='Portfolio Values',title='Portfolio values from 05/09/2017 to 01/05/2020') +
  theme_classic()

##################################################################################### Punti 5- 6- 7
# Punto 5-6 ####
####################################################################################### Dati
# Tassi risk free 6-12-18-24
vet_tas = c(as.numeric(dataset[[3]][669,16]),as.numeric(dataset[[5]][669,16]),
            as.numeric(dataset[[6]][669,16]),as.numeric(dataset[[7]][669,16]))
# Volatilità implicita 6-12-18-24
vet_sig = c(as.numeric(dataset[[3]][669,8]),as.numeric(dataset[[5]][669,8]),
            as.numeric(dataset[[6]][669,8]),as.numeric(dataset[[7]][669,8]))
# Volatilità implicita 1 mese
vol_imp1mese = tail(dataset$`1M`$`100`,1)
# Valore portafoglio
valpf = as.numeric(tail(matrix,1))
# Vettore dei prezzi
prices_vector = na.omit(dataset$`6M`$price)
# Volatilità storica
vol_storica = sd(diff(log(prices_vector)))

########################################################################################## Value-at-Risk
# Calcolare VaR
VaR=function(price,alpha=c(),rates,volatilities,vol,value_portfolio)
{
  VaR=c()
  for (i in 1:length(alpha))
  {
    quantile= qnorm(alpha[i], 0 ,ifelse(vol==vol_imp1mese,vol*sqrt(1/252),vol))
    price_1giorno = price*exp(quantile)
    valpf_1giorno= 4 * price_1giorno + 
      GBSOption(TypeFlag='p',S=price_1giorno,X=price,Time=0.5,
                r=rates[1],b=rates[1],
                sigma=volatilities[1])@price + 
      GBSOption(TypeFlag='p',S=price_1giorno,X=price,Time=1,
                r=rates[2],b=rates[2],
                sigma=volatilities[2])@price + 
      GBSOption(TypeFlag='p',S=price_1giorno,X=price,Time=1.5,
                r=rates[3],b=rates[3],
                sigma=volatilities[3])@price + 
      GBSOption(TypeFlag='p',S=price_1giorno,X=price,Time=2,
                r=rates[4],b=rates[4],
                sigma=volatilities[4])@price 
    VaR[i]= - (valpf_1giorno - value_portfolio)
    VaR_percentage= (VaR/value_portfolio)*100
  }
  out=list('VaR'= VaR,'VaR %'= VaR_percentage)
  return(out)
}

# Per calcolare il VaR con volatilità implicita a 1 mese inserire vol=vol_imp1mese
# Per calcolare il VaR con volatilità storica inserire vol=vol_storica
VaR(price=2830.71,alpha=c(0.05,0.02,0.01),rates=vet_tas,
    volatilities = vet_sig,vol=vol_storica,value_portfolio=valpf)

######################################################################################## Expected Shortfall

# Calcolare ES
ES=function(delta,price,alpha=c(),rates,volatilities,vol,value_portfolio)
{
  ES=c()
  for (j in 1:length(alpha))
  {
    delta_alpha = alpha[j]/delta
    v_delta_alpha = seq(0, alpha[j],by=delta_alpha)
    sum = 0
    for (k in 1:delta){
      quantile = qnorm(alpha[j] - v_delta_alpha[k], 0, ifelse(vol==vol_imp1mese,vol*sqrt(1/252),vol))
      price_1giorno = price * exp(quantile)
      valpf_1giorno= 4 * price_1giorno + 
        GBSOption(TypeFlag='p',S=price_1giorno,X=price,Time=0.5,
                  r=rates[1],b=rates[1],
                  sigma=volatilities[1])@price + 
        GBSOption(TypeFlag='p',S=price_1giorno,X=price,Time=1,
                  r=rates[2],b=rates[2],
                  sigma=volatilities[2])@price + 
        GBSOption(TypeFlag='p',S=price_1giorno,X=price,Time=1.5,
                  r=rates[3],b=rates[3],
                  sigma=volatilities[3])@price + 
        GBSOption(TypeFlag='p',S=price_1giorno,X=price,Time=2,
                  r=rates[4],b=rates[4],
                  sigma=volatilities[4])@price 
      VaR = - (valpf_1giorno - value_portfolio)
      ExS = VaR * delta_alpha
      sum = sum + ExS
      ES[j] = sum/alpha[j] 
      ES_percentage= (ES/value_portfolio)*100
    }
  }
  out=list('ES'= ES, 'ES %'= ES_percentage)
  return(out)
}
# Per calcolare l'ES con volatilità implicita a 1 mese inserire vol=vol_imp1mese
# Per calcolare l'ES con volatilità storica inserire vol=vol_storica
ES(delta=1000,price=2830.71,alpha=c(0.05,0.02,0.01),rates=vet_tas,
   volatilities = vet_sig,vol=vol_imp1mese,value_portfolio=valpf)

################################################################################ VaR simulazione storica
# Punto 7 ####
VaR_sim = function(returns,price,alpha=c(),rates,volatilities,value_portfolio)
{
  returns=diff(log(returns))
  returns=sort(returns)
  quant_returns=round(alpha*length(returns))
  VaR_sim = c()
  for (i in 1:length(alpha))
  {
    price_1giorno = price * exp(returns[quant_returns[i]])
    valpf_1giorno= 4 * price_1giorno + 
      GBSOption(TypeFlag='p',S=price_1giorno,X=price,Time=0.5,
                r=rates[1],b=rates[1],
                sigma=volatilities[1])@price + 
      GBSOption(TypeFlag='p',S=price_1giorno,X=price,Time=1,
                r=rates[2],b=rates[2],
                sigma=volatilities[2])@price + 
      GBSOption(TypeFlag='p',S=price_1giorno,X=price,Time=1.5,
                r=rates[3],b=rates[3],
                sigma=volatilities[3])@price + 
      GBSOption(TypeFlag='p',S=price_1giorno,X=price,Time=2,
                r=rates[4],b=rates[4],
                sigma=volatilities[4])@price 
    VaR_sim[i] = -(valpf_1giorno - value_portfolio)
    VaR_sim_percentage = (VaR_sim/value_portfolio)*100
  }
  out=list('VaR Simulazione Storica'=VaR_sim,'VaR Simulazione Storica %'=VaR_sim_percentage)
  return(out)
}

VaR_sim(returns=prices_vector,price=2830.71,alpha=c(0.05,0.02,0.01),rates=vet_tas,
        volatilities = vet_sig,value_portfolio=valpf)

################################################################################## ES con simulazione storica
ES_sim=function(returns,price,alpha=c(),rates,volatilities,value_portfolio)
{
  returns=diff(log(returns))
  returns=sort(returns)
  quant_returns=round(alpha*length(returns))
  ES_sim = c()
  for (i in 1:length(alpha))
  {
    price_1giorno = price * exp(returns[1:quant_returns[i]])
    valpf_1giorno= 4 * price_1giorno + 
      GBSOption(TypeFlag='p',S=price_1giorno,X=price,Time=0.5,
                r=rates[1],b=rates[1],
                sigma=volatilities[1])@price + 
      GBSOption(TypeFlag='p',S=price_1giorno,X=price,Time=1,
                r=rates[2],b=rates[2],
                sigma=volatilities[2])@price + 
      GBSOption(TypeFlag='p',S=price_1giorno,X=price,Time=1.5,
                r=rates[3],b=rates[3],
                sigma=volatilities[3])@price + 
      GBSOption(TypeFlag='p',S=price_1giorno,X=price,Time=2,
                r=rates[4],b=rates[4],
                sigma=volatilities[4])@price  
    ES_sim[i] = mean(-(valpf_1giorno - value_portfolio))
    ES_sim_percentage= (ES_sim/value_portfolio)*100
  }
  out=list('ES Simulazione Storica'=ES_sim,'ES Simulazione Storica %'=ES_sim_percentage)
  return(out)
}

ES_sim(returns=prices_vector,price=2830.71,alpha=c(0.05,0.02,0.01),rates=vet_tas,
       volatilities = vet_sig,value_portfolio=valpf)
################################################################################### METODO DELTA-NORMAL
# Punto 8 ####
# Fattori di rischio
rates_6mesi=dataset$`6M`$rate
rates_12mesi=dataset$`12M`$rate
rates_18mesi=dataset$`18M`$rate
rates_24mesi=dataset$`24M`$rate

# Varianza 4 fattori di rischio
var_tas=c(var(diff(rates_6mesi)),
          var(diff(rates_12mesi)),
          var(diff(rates_18mesi)),
          var(diff(rates_24mesi)))

# Covarianza 4 fattori di rischio
cov_612=cov(diff(rates_6mesi),diff(rates_12mesi))
cov_618=cov(diff(rates_6mesi),diff(rates_18mesi))
cov_624=cov(diff(rates_6mesi),diff(rates_24mesi))
cov_1218=cov(diff(rates_12mesi),diff(rates_18mesi))
cov_1224=cov(diff(rates_12mesi),diff(rates_24mesi))
cov_1824=cov(diff(rates_18mesi),diff(rates_24mesi))

# Matrice varianze tassi risk-free
matvar = matrix(c(var_tas[1],0,0,0,0,var_tas[2],
                  0,0,0,0, var_tas[3],0,0,0,0, 
                  var_tas[4]), nrow = 4, ncol = 4)

# Matrice varianze e covarianze tassi risk-free
matvarcov = matrix(c(var_tas[1],cov_612,cov_618,cov_624,cov_612,var_tas[2],
                     cov_1218,cov_1224,cov_618,cov_1218, var_tas[3],
                     cov_1824,cov_624, cov_1224, cov_1824, 
                     var_tas[4]), nrow = 4, ncol = 4)

############################################################################################ VaR DELTA NORMAL
VaR_deltanormal=function(rates,volatilities,alpha=c(),times=c(),matrix,value_portfolio)
{
  # Matrice delle derivate
  g = matrix(0, ncol = 1, nrow =4)
  for (i in 1:length(vet_tas))
  {
    g[i,]=GBSGreeks(Selection='rho',TypeFlag = 'p',S=2830.71,X=2830.71,
                    Time = times[i], r = vet_tas[i], b=vet_tas[i], sigma=vet_sig[i])
  }
  # Varianza delta- normal 
  varianza_deltanormal = t(g)%*%matrix%*%g
  VaR_deltanormal=c()
  for (i in 1:length(alpha))
  {
    VaR_deltanormal[i] = - qnorm(alpha[i]) * sqrt(varianza_deltanormal)
  }
  VaR_deltanormal_percentage=(VaR_deltanormal/value_portfolio)*100
  out=list('VaR Delta Normal'=VaR_deltanormal,'VaR Delta Normal Percentage'=VaR_deltanormal_percentage)
  return(out)
}

# Per calcolare il Value-at-Risk con matrice di varianze,e covarianze nulle, inserire matrix = matvar
# Per calcolare il Value-at-Risk con matrice di varianze e covarianze inserire matrix = matvarcov
VaR_deltanormal(rates= vet_tas,volatilities = vet_sig,alpha=c(0.05,0.02,0.01),
                times=c(0.5,1,1.5,2), matrix= matvarcov, value_portfolio = valpf)

###############################################################################################  ES DELTA NORMAL

ES_deltanormal= function(rates,volatilities,alpha=c(),times=c(),matrix,value_portfolio)
{
  ES_deltanormal = c()
  # Matrice delle derivate
  g = matrix(0, ncol = 1, nrow =4)
  for (i in 1:length(vet_tas))
  {
    g[i,]=GBSGreeks(Selection='rho',TypeFlag = 'p',S=2830.71,X=2830.71,
                    Time = times[i], r = vet_tas[i], b=vet_tas[i],sigma=vet_sig[i])
  }
  # Varianza delta- normal 
  varianza_deltanormal = t(g)%*%matrix%*%g
  for (j in 1:length(alpha))
  {
    ES_deltanormal[j] = dnorm(qnorm(alpha[j]))/alpha[j] * sqrt(varianza_deltanormal)
  }
  ES_deltanormal_percentage=(ES_deltanormal / value_portfolio)*100
  out=list('ES Delta Normal'=ES_deltanormal,'ES Delta Normal Percentage'=ES_deltanormal_percentage)
  return(out)
}

# Per calcolare l'ES con matrice di varianze,e covarianze nulle, inserire matrix = matvar
# Per calcolare l'ES Value-at-Risk con matrice di varianze e covarianze inserire matrix = matvarcov
ES_deltanormal(rates= vet_tas,volatilities = vet_sig,alpha=c(0.05,0.02,0.01),
               times=c(0.5,1,1.5,2), matrix= matvarcov, value_portfolio = valpf)