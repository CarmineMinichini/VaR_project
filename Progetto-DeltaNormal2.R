# Varianza del Fattore di rischio sottostante
var_sottostante=var(diff(prices_vector))

cov_1=cov(diff(prices_vector),diff(rates_6mesi))
cov_2=cov(diff(prices_vector),diff(rates_12mesi))
cov_3=cov(diff(prices_vector),diff(rates_18mesi))
cov_4=cov(diff(prices_vector),diff(rates_24mesi))

# Matrice con varianza dei tassi e  varianza del sottostante - 5 Fattori di rischio
mat_rs = matrix(c(var_tas[1],0,0,0,0,0 ,var_tas[2], 0,0,0,0,0, var_tas[3],0,0,0,0,0 ,var_tas[4],0,0,0,0,0, 
                  var_sottostante), nrow = 5, ncol = 5)

# Matrice varianze e covarianze tassi risk-free
mat_covrs = matrix(c(var_tas[1],cov_612,cov_618,cov_624,cov_1,cov_612,var_tas[2],
                     cov_1218,cov_1224,cov_2,cov_618,cov_1218,var_tas[3],
                     cov_1824,cov_3, cov_624,cov_1224, cov_1824,
                     var_tas[4],cov_4,cov_1,cov_2,cov_3,cov_4,var_sottostante),nrow = 5, ncol = 5)

##################################################################################### VaR con 5 Fattori di rischio
VaR_deltanormal2=function(rates,volatilities,alpha=c(),times=c(),matrix,value_portfolio)
{
  # Matrice delle derivate
  g = matrix(0, ncol = 1, nrow =4)
  for (i in 1:length(vet_tas))
  {
    g[i,]=GBSGreeks(Selection='rho',TypeFlag = 'p',S=2830.71,X=2830.71,
                    Time = times[i], r = vet_tas[i], b=vet_tas[i],sigma=vet_sig[i]) 
  }
  g_2 = matrix(0, ncol = 1, nrow =4)
  for (i in 1:length(vet_tas))
  {
    g_2[i,]=GBSGreeks(Selection='delta',TypeFlag = 'p',S=2830.71,X=2830.71,
                    Time = times[i], r = vet_tas[i], b=vet_tas[i],sigma=vet_sig[i]) + 4 
  }
  somma_dsottostante= sum(g_2[i,])
  g=rbind(g,somma_dsottostante)
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

VaR_deltanormal2(rates= vet_tas,volatilities = vet_sig,alpha=c(0.05,0.02,0.01),
                times=c(0.5,1,1.5,2), matrix= mat_covrs, value_portfolio = valpf)
##################################################################################### ES con 5 Fattori di rischio
ES_deltanormal2= function(rates,volatilities,alpha=c(),times=c(),matrix,value_portfolio)
{
  ES_deltanormal = c()
  # Matrice delle derivate
  g = matrix(0, ncol = 1, nrow =4)
  for (i in 1:length(vet_tas))
  {
    g[i,]=GBSGreeks(Selection='rho',TypeFlag = 'p',S=2830.71,X=2830.71,
                    Time = times[i], r = vet_tas[i], b=vet_tas[i],sigma=vet_sig[i])
  }
  g_2 = matrix(0, ncol = 1, nrow =4)
  for (i in 1:length(vet_tas))
  {
    g_2[i,]=GBSGreeks(Selection='delta',TypeFlag = 'p',S=2830.71,X=2830.71,
                      Time = times[i], r = vet_tas[i], b=vet_tas[i],sigma=vet_sig[i]) + 4 
  }
  somma_dsottostante= sum(g_2[i,])
  g=rbind(g,somma_dsottostante)
  # Varianza delta-normal 
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
ES_deltanormal2(rates= vet_tas,volatilities = vet_sig,alpha=c(0.05,0.02,0.01),
               times=c(0.5,1,1.5,2), matrix= mat_covrs, value_portfolio = valpf)


