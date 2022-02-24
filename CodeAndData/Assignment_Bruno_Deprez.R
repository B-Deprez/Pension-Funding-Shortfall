#################################
#### Assignment Bruno Deprez ####
####       r0630718          ####
#################################

library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(readxl)
library(copula)
library(MASS)
library(fGarch) 
library(purrr)

#### Setup ####
#Relative path to make life easier
dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(dir)
KULbg <- "#116E8A" 

#### The graph of replacement rates ####
X <- c(c(2:9)*10000, c(3:5)*50000)
Y <- c(89, 84, 80, 77, 76, 77, 77, 78, 85, 88, 88)/100

repl_rat <- tibble(Salary = X, Ratio = Y)
ggplot(repl_rat, aes(Salary, Ratio))+
  geom_point(color = KULbg, alpha = 0.8, size = 2.5) +
  geom_line(color = KULbg, alpha = 0.4, size = 1) +
  geom_text(aes(label = Ratio), color = KULbg ,hjust=-0.1, vjust=-0.3) +
  labs(title = "The Optimal Replacement Ratio over Different Incomes") +
  xlab("Salary ($)") +
  ylab("Replacement Ratio")+
  theme_bw()
ggsave("Replacement.pdf")


#### Copula of the BEL20 ####
#The code is done in a scalable way

temp = list.files(pattern="*.csv") #List all csv files with stock data

test = FALSE #Needed in the for loop below
for(file in temp){
  #We read all the csv files one by one
  data <- read_csv(file, na = "null") %>% 
    drop_na() %>%
    dplyr::select(Date, `Adj Close`) #Only need date and adj. closing price
  
  # We calculate the return and take the first part of the csv name as stock name
  data['Return'] <- c(0,diff(data$`Adj Close`))/data$`Adj Close`
  data['Stock'] <- strsplit(file, '\\.')[[1]][1]
  #If it is the first time in the loop (test = FALSE), the data become the full file
  #Otherwise (test = TRUE) we paste the new data to the previous stocks
  if(test){
    stock_data <- bind_rows(stock_data, data)
  } else {
    stock_data <- data
    test = TRUE
  }
}

#We only select dates where data is present for all
#This is needed to be able to construct our copula
dates_present_everywhere <- stock_data %>%
  count(Date) %>%
  dplyr::filter(n==length(temp)) %>%
  dplyr::select(Date)

stock_data <- stock_data %>%
  dplyr::filter(Date %in% dates_present_everywhere$Date) 
#%>% dplyr::filter(Date < "2020-02-01") #This can be added to 
# not take the Covid-19 crisis into account.

#We make a nested version of the dataset
#This makes life much easier when fitting the univariate t-distributions
stock_data_nested <- stock_data %>%
  group_by(Stock) %>%
  nest()
  
#We define a function that is used later to fit the t-distribution
est_t <- function(Returns){
  est <- fitdistr(Returns, "t") #Fit the t-distribution
  est <- est$estimate
  est[2] = est[2] * sqrt( est[3] / (est[3]-2) ) #To get the proper variance
  return(est)
}

#We fit the t-distributions to each data,
#calculate the quantiles of the observed data (for the copula)
#and select at the last closing price observed
stock_data_t <- stock_data_nested %>%
  mutate(t_dist = map(data, ~est_t(.x$Return))) %>%
  mutate(qunatiles = map2(data, t_dist, 
                         ~pstd(.x$Return, .y[1], .y[2], .y[3]) ))%>%
  mutate(last_close = map(data, ~.x$`Adj Close`[length(.x$`Adj Close`)]))

#Construct df needed to fit our copula
df <- stock_data_t$qunatiles[[1]]
for(i in 2:nrow(stock_data_t)){
  df <- cbind(df, stock_data_t$qunatiles[[i]])
}

#We fit the t-copula using fitCopula from the copula package
cop <- fitCopula(copula = tCopula(dim = nrow(stock_data_t)), data =df, method = "ml")


#### Monte Carlo simulations ####
set.seed(1997) #To be able to replicate the results
n_sim = 1000 #Number of simulations
simulated_values <- c()

#We calculate the last value of our portfolio 
#This portfolio is just investing in 1 stock of each
last_value <- stock_data_t %>%
  dplyr::select(Stock, last_close) %>%
  unnest(last_close) %>%
  ungroup()%>%
  summarise(sum(last_close))
last_value <- last_value[[1]] 

for(simulation in 1:n_sim){
  stock_data_sim <- stock_data_t #Start fresh every time
  
  #Simulate a year of data (n=52 weeks)
  rcop <- rCopula(n = 52, copula = tCopula(param = cop@estimate[1], dim = nrow(stock_data_sim), df = cop@estimate[2]))
  
  #Below, we want to have the column with simulated percentages (from copula)
  #for each stock saved in the tibble as before to make it easier 
  #to do a quantile to quantile transformation. 
  #This may not be the most understandable part of the code due to the way
  #we work with the tibble. 
  tibble_columns <- c()
  names <- stock_data_nested$Stock
  for (i in 1:ncol(rcop)){
    tib <- tibble(rcop[,i])
    names(tib) <- names[i]
    tibble_columns[i] <- tib
  }
  
  stock_data_sim$sims <- tibble_columns
  
  #Below, we move back to the return values from the quantiles gotten
  #from the copula
  #Note that we use  +1 in the second line. This way we can just multiply
  #everything to obtain the final values of the stocks and portfolio.
  stock_data_sim <- stock_data_sim %>%
    mutate(sims_q = map2(sims, t_dist, ~qstd(.x, .y[1], .y[2], .y[3]) + 1 )) %>% #Note that we used +1 here!!
    mutate(total_growth = map(sims_q, ~prod(pmax(.x, 0)))) #We do not allow .x = 1+rate to be lower than 0, because then the stock has a negative value
   
  portfolio_final_value <- stock_data_sim %>%
    dplyr::select(Stock, last_close, total_growth) %>%
    unnest(c(last_close,total_growth)) %>%
    mutate(final_value = last_close*total_growth) %>% #Absolute growth of stocks
    ungroup() %>%
    summarise(sum(final_value)) #Absolute growth of portfolio
  simulated_values[simulation] <- portfolio_final_value[[1]]/last_value-1 #Simulated return
  
}

#Some visualisations
hist(simulated_values)
round(mean(simulated_values),4)
round(var(simulated_values),4)

#### Simulations for the OLO ####
OLO <- read_excel("kern.xls", skip= 2) %>% 
  dplyr::filter(...1 == "Lange rente (OLO, 10 jaar) (%)") %>%
  dplyr::select(-...1)
values <- as.numeric(OLO[1,])/100
hist(values) #Looks bimodal

years <- as.numeric(names(OLO))
probs <- (years-1990)/sum(years-1990) #More weight to recent observations

set.seed(1997) #To be able to replicate the results
sim_olo <- sample(values, n_sim, prob = probs, replace = TRUE)
hist(sim_olo) #Relatively more lower values

#### Summary statistics portfolio and OLOs ####
mu_eq = mean(simulated_values)+1 # we need 1+ rate of return
var_eq = var(simulated_values)

mu_tr = mean(sim_olo)+1
var_tr = var(sim_olo)

#### Select d ####
#First we read the mortality table and transform it to be able to use it easily
mort_table <- read.delim("mort.txt", dec = ".")
mort_table <- mort_table["XR"]
mort_table["XR"] <- lapply(mort_table["XR"], function(x){as.numeric(gsub("\\.", "",x))})
mort_table["px"] <- 0
for(i in 1:nrow(mort_table)){
  if(mort_table[i,1] >0){ #In order to not divide by 0
    mort_table[i,2] <- mort_table[i+1,1]/mort_table[i,1]
  }else{
    mort_table[i,2]=0
  }
}

#We will make a table that will be used in my paper
ages <- c(seq(50,90, by = 5))
wealth_to_consumption_ratio <- (seq(6,16))

df_age_ratio <- data.frame()
theoretical <- data.frame()
d_optimal <- data.frame()

for(r in 1:length(wealth_to_consumption_ratio)){
  W_C = wealth_to_consumption_ratio[r]
  for(a in 1:length(ages)){
    x = ages[a]
    solve_d <- function(D){
      sum_d = 0
      for(i in x:(120-x)){
        mortality <- prod(mort_table[(x+1):(i+1),2])
        sum_d = sum_d + mortality/(D^(i-x+1))
      }
      total = sum_d - W_C
      return(total)
    }
    #Little work-around since root finding not always possible
    #If root not possible, we just take the value for previous age
    D_try = try(uniroot(solve_d, c(0.6,2))$root, silent = TRUE) 
    if (!inherits(D_try, "try-error")) {
      D = uniroot(solve_d, c(0.6,2))$root
    }
    d = D*0.99; d
    
    #### Optimise for alpha ####
    Prob_lower_d <- function(alpha){
      #Work with 1+ rate of return
      ret_sim <- alpha*(1+sim_olo) + (1-alpha)*(1+simulated_values)
      prob_lower_d <- mean(ret_sim < d)
      return(prob_lower_d)
    }
    
    al <- seq(0,1, by=0.001)
    p <- c()
    for(i in 1:length(al)){
      p[i] = Prob_lower_d(al[i])
    }
    als_optim <- al[which(p == min(p))] 
    #When there is a tie, we select the first (most risky)
    al_optim <- als_optim[1]; 
    df_age_ratio[a, r] <- al_optim
    
    # Theoretical values 
    #if mu_tr < d, then put everything in equity (see paper)
    theoretical[a,r] = ((var_eq*(mu_tr-d))/(var_eq*(mu_tr-d)+var_tr*(mu_eq-d)))*(mu_tr > d) # *(mu_tr > d) is an indicator function
    
    # Corresponding d
    d_optimal[a,r] <- d
    
  }
}

#### Little comparison with theoretical values ####
difference <- df_age_ratio-theoretical
diff_heat <- matrix(as.numeric(as.matrix(difference)),nrow =9)
colnames(diff_heat) <- wealth_to_consumption_ratio
rownames(diff_heat) <- ages

heatmap(diff_heat, Colv = NA, Rowv = NA, scale = 'none', col = topo.colors(99))

mean(difference < 0) #73% lower
mean(difference > 0) #10% higher
mean(difference == 0) #17% the same

#### Table for report ####
names(df_age_ratio) <- wealth_to_consumption_ratio
row.names(df_age_ratio) <- ages
stargazer::stargazer(df_age_ratio, summary=FALSE) #stargazer library to get nice table for LaTeX

#### Funding 25 y/o ####
i = 2/100
x = 25
n = 67-25
v = 1/(1+i)

#We initialise the Dx and Nx columns
mort_table["Dx"] = 0
mort_table["Nx"] = 0

#Now we calculate the values
for(a in 1:121){
  mort_table["Dx"][a, ] = mort_table[a,1]*v^(a-1) #Since a = 1 is age = 0
}
for(a in 1:121){
  mort_table["Nx"][a, ] = sum(mort_table["Dx"][a:121,]) 
}

#For the annuities and endowments
nEx = mort_table["Dx"][x+n,]/mort_table["Dx"][x,]; nEx
pre_axn = (mort_table["Nx"][x,]- mort_table["Nx"][x+n,])/mort_table["Dx"][x,]; pre_axn

P = 1.5*30000*nEx/(0.95*pre_axn); P
P/30000
