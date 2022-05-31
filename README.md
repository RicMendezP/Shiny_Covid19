# Shiny_Covid19

The following code was written with R and shiny to show how a predictive LR model would work in the health sector.

The starting data for a pandemic such as COVID19 is not enough to create a well trained 
predictive model. In such a case, a simple linear regression model will help to follow 
the pandemic trend on a daily basis. In the following project the first 41 days , March 06th 
to April the 13th, will bring data to develop an initial LR model. It will be tested with 
real data after april the 13th. While the predicted range intervals contain the new test data, 
the prediction LR model will be helpful for two weeks; otherwise, the new data will be loaded as input for 
the next iteration, which will end up with a new predictive LR slope, and new range predictive intervals 
for the expected model. The process may continue, until there is enough data to develop a more 
accurate predictive model.

##### DATA SOURCE - ADAPTED FROM THE COLOMBIAN HEALTH MINISTER ######################
####  https://www.ins.gov.co/Noticias/Paginas/Coronavirus.aspx  ###
###################################################################

THE CODE WILL REPRODUCE THE FOLLOWING CASE IN SHINY:  https://inisghtdiscovery.shinyapps.io/Covid19/
