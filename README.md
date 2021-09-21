# The Effects of Institutional and Social Trust on Life Satisfaction: a Multilevel Logit Analysis in 35 Countries #

## Aims of the project ##
The aim of this study is to measure the effect of social and institutional trust on life satisfaction. Furthermore, the effect individual social trust on life satisfaction will be tested in the context of high or low social trust at the country level. That is, cross-level interactions will be examined.
Data from the most recent wave (Wave 7) of the World Values Survey is utilised and includes data collected between 2017 and 2019 in the following countries: Andorra, Argentina, Australia, Bangladesh, Bolivia, Brazil, Chile, China, Colombia, Cyprus, Germany, Ecuador, Greece, Hong Kong SAR, China, Indonesia, Iraq, Kazakhstan, Japan, Jordan, Republic of Korea, Lebanon, Mexico, Nigeria, Pakistan, Peru, Philippines, Puerto Rico, Romania, Russian Federation, Serbia, Thailand, Tunisia, Turkey, United States.


## Data ##
For more information on the data utilised for this project, please refer to the Data section in the 'QuantsFinal.pdf' document in this repository.

## Methods ##
All of the data exploration and analyses were run using the programming language R. Before any statistical analysis could be performed, the data was checked for any missing values and these entries were removed. Age, age squared and GDP per capita val- ues were scaled to SD units in order to build our models.

Two-level logit models were used for this investigation using the GLMER package. This was appropriate as the dependent variable is binary (individuals have either high satisfaction or low satisfaction) and control- ling for country-level effects on life satisfac- tion was necessary. The multilevel model also allows for controlling for individual level variables. In the model the only coefficients allowed to vary randomly were the random intercepts. That is, levels of high or low life satisfaction were allowed to vary randomly across countries (random intercepts model).

The first model that was specified was a null random intercepts model: that is, a model with no predictors. Following models included added explanatory variables, start- ing first the control variables sex, age, age squared, marital status, employment status, GDP. Later models included social trust and institutional trust separately as explanatory variables, and then together in the same model. Finally cross-level interactions were tested using individual level social trust and country-level social trust (aggregated). The flexibility of multilevel modelling allows for the exploration of this sort of effect. The aim was to test the effect of contextual so-cial trust (country-level) on the impact of individual-level social trust on life satisfaction outcomes.

The different models were compared us- ing the AIC statistic. The final model gives the lowest AIC estimate and includes the cross-level interaction term.

## Results and Discussion ##
Please refer to the Results and Conclusions sections in the 'QuantsFinal.pdf' document in this repository.


