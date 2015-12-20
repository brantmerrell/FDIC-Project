devtools::install_github('rstudio/shinyapps')
shinyapps::setAccountInfo(name='brantmerrell',
                          token='AC09F55A80B4C5F73175FEEA636F8D60',
                          secret='AOUo7fIk3auZLTI7vAsjf4hkkAi4f1bNQDHHIwyU')
library(shinyapps)
shinyapps::deployApp()

head(quarters)
qPattern<-quarters[1]
save.image()
