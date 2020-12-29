#Read and clean data
messages <- read.csv("~/Desktop/messages in market.csv", header = T)

attach(messages)

#load libraries
library(tidyverse)
library(ggplot2)
library(jtools)
library(ggpubr)

#create metric by funnel
rate <- messages %>% mutate(
  UF_CTR = scale(CTR),
  MF_BFE = scale(BFER),
  CS_BFE = scale(BFER),
  LF_CVR = scale(CVR),
  Target = case_when(funnel2 == 'Upper Funnel' ~ UF_CTR,
                     funnel2 == 'Mid-Funnel' ~ MF_BFE,
                     funnel2 == 'Cross-Sell' ~ CS_BFE,
                     funnel2 == 'Lower Funnel' ~ LF_CVR))

#messages in market regression offers as character
subs=split(messages,list(messages$LOB,messages$funnel2,messages$Partner2))
models=map(subs,safely(function(dd)lm(rate ~ as.character(X.offers), data=dd)))
m1 <- models %>% transpose()
m2 <- m1$error %>% map_lgl(is_null)
m3 <- m1$result[m2] 
lapply(m3,function(x)plot_coefs(x))
lapply(m3,function(x)summary(x))



#scatterplot
p = ggplot(messages, aes(X.offers, rate$Target)) + geom_point()
p + facet_grid(Partner2~funnel2~LOB)

#correlation for each plot
cors <- rate %>% group_by(Partner2, funnel2, LOB) %>% dplyr::summarise(r = cor(X.offers, Target))
p + geom_text(data=cors, aes(labels=labels))



p2=map(subs,safely(function(dd)ggscatter("X.offers", "rate$Target", 
                                         add = "reg.line", conf.int = TRUE, 
                                         cor.coef = TRUE, cor.method = "pearson",
                                         xlab = "offers", ylab = "rate", data=dd)))


ggscatter(messages, x = "X.offers", y = "rate$Target", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "offers", ylab = "rate")


#messages in market regression offers as number
subs=split(messages,list(messages$funnel2,messages$Partner2))
models=map(subs,safely(function(dd)lm(rate ~ as.numeric(X.offers), data=dd)))
m1 <- models %>% transpose()
m2 <- m1$error %>% map_lgl(is_null)
m3 <- m1$result[m2] 
lapply(m3,function(x)summary(x))

################### Function to label a facetted graph
lm_labels <- function(dat) {
  mod <- lm(Target ~ X.offers, data = dat)
  formula <- sprintf("italic(y) == %.2f %+.2f * italic(x)",
                     round(coef(mod)[1], 2), round(coef(mod)[2], 2))
  
  r <- cor(dat$X.offers, dat$Target)
  r2 <- sprintf("italic(R^2) == %.2f", r^2)
  data.frame(formula = formula, r2=r2, stringsAsFactors = FALSE)
}
##################

### Exploratory Regression to control for spending

Model1 <- glm(Target ~ X.offers + Spend, data = rate)
summary(Model1)


### Bucketing correlation

bucketing_offers <- messages %>% 
  dplyr::mutate(offer_bins=ifelse(X.offer <= 5), 'very_low',
                                       ifelse(between(X.offers, 6, 10), 'low',
                                              ifelse(between(X.offers, 11, 15), '3',
                                                     ifelse(between(X.offers, 16, 20), '4',
                                                            ifelse(between(X.offers, 21, 25), '5', '>5')))))

cor(m, method="kendall", use="pairwise") 
