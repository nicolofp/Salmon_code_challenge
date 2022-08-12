library(data.table)
library(readxl)

data_path = "C:/Users/nicol/Downloads/Data Scientist assigment - Backround data.xlsx"
weight = data.table(read_xlsx(data_path,sheet = 1))
time_series = data.table(read_xlsx(data_path,sheet = 2,skip = 1,
                                   col_names = c("Month","Number of individuals",
                                                 "Biomass")))

gen_weight = function(dt){
  tmp = sapply(1:NROW(dt), function(i){
    round(rnorm(n = dt$pop_p[i],
                mean = dt$`Average weight`[i],
                sd = dt$`std (s)`[i]),3)
  })
  tmp = do.call(c,tmp)
  return(tmp)
}

# Weight should be a data.table with same column names
# par is the vector of parameter to optimize 
# here only the lambda of the distribution for the 
# overall population


opt_function = function(weight,par,n_pop,biomass){
  set.seed(90)
  weight[, prop_p := dpois(0:60,par)]
  tmp = sample(61,n_pop, replace = T,prob = weight$prop_p)
  pop_group = rep(0,61)
  pop_group[as.numeric(names(table(tmp)))] = table(tmp)
  weight[, pop_p := pop_group]
  return(abs(round(sum(gen_weight(dt = weight)),0) - biomass))
}

opt_results = optim(c(2.5),
                    method = "L-BFGS-B",
                    opt_function,
                    weight = weight,
                    lower = 2, 
                    upper = 50,
                    n_pop = 19175,
                    biomass = 86197
                    #,control = list(maxit = 100000)
)

bbb = gen_weight(dt = weight)
length(bbb[bbb>4])/length(bbb)
hist(ccc)

length(ccc[ccc>4])/length(ccc)
ccc = rnorm(19175,mean = weighted.mean(weight$`Average weight`,weight$prop_p),
            sd = weighted.mean(weight$`std (??)`,weight$prop_p))

pnorm(4,mean = weighted.mean(weight$`Average weight`,weight$prop_p),
      sd = weighted.mean(weight$`std (??)`,weight$prop_p),lower.tail=FALSE)

pnorm(84, mean=72, sd=15.2, lower.tail=FALSE) 

sim_tmp = data.table(set_seed = 1:1000, results = NA)
for(i in 1:1000){
  print(paste0("Sim --> ",i))
  set.seed(i)
  weight[, prop_u := rep(1/61,61)]
  weight[, prop_p := dpois(0:60,5.35)]
  # p = as.numeric(rdirichlet(n = 1, alpha = rep(1,61)))
  tmp = sample(61,156077, replace = T,prob = weight$prop_u)
  weight[, pop_u := table(tmp)]
  
  tmp = sample(61,156077, replace = T,prob = weight$prop_p)
  weight[, pop_p := c(table(tmp),rep(0,61-length(table(tmp))))]
  
  sim_tmp$results[i] = round(sum(gen_weight(dt = weight)),0)
}
set.seed(90)
weight[, prop_u := rep(1/61,61)]
weight[, prop_p := dpois(0:60,5.3)]
# p = as.numeric(rdirichlet(n = 1, alpha = rep(1,61)))
tmp = sample(61,156077, replace = T,prob = weight$prop_u)
weight[, pop_u := table(tmp)]

tmp = sample(61,156077, replace = T,prob = weight$prop_p)
weight[, pop_p := c(table(tmp),rep(0,61-length(table(tmp))))]

gen_weight = function(dt){
  tmp = sapply(1:NROW(dt), function(i){
    round(rnorm(n = dt$pop_p[i],
                mean = dt$`Average weight`[i],
                sd = dt$`std (s)`[i]),3)
  })
  tmp = do.call(c,tmp)
  return(tmp)
}
round(sum(gen_weight(dt = weight)),0)
tmp2 = gen_weight(dt = weight)

tmp1 = gen_weight(dt = weight)
maxit = 1
w = rep(1,156077)
set.seed(90)
while(sum(w) >= 317895.499 & sum(w) <= 317894.501 & maxit < 1000){
  w = gen_weight(dt = weight)
  maxit = maxit + 1
  return(w)
}

time_series[, mean_w := Biomass/`Number of individuals`]
aaa = rtruncnorm(n = 156077, a = 0.4615186, b = 7.76,mean = 2.03, sd = 0.55)
sum(aaa)
length(aaa[aaa>4])
# Goal: 317895






qnorm(p = 0.995,mean = 7.5,sd = 0.1)
quantile(rnorm(5000,mean = 7.5,sd = 0.1),0.995)
# 317895.499
# 317894.501

# eval_f1 = function(par){
#   tmp = rfsrc(fetal_health2 ~ ., data = DT[train,-c(22,24),with=F],
#               ntree = ceiling(par[1]),
#               mtry = ceiling(par[2]),
#               nodesize = ceiling(par[3]),
#               sampsize =  1275, #zceiling(0.75*NROW(x)),
#               importance = TRUE)
#   error_oob = tmp$err.rate[NROW(tmp$err.rate),1]
#   return(error_oob)
# }
# 
# 
# res2 = nloptr(x0 = c(500,5,4), eval_f = eval_f1,
#               lb = c(251,2,2), ub = c(3000,10,30),
#               opts = list("algorithm" = "NLOPT_GN_DIRECT",
#                           "xtol_rel" = 1.0e-8))