# Refugee Employment Analysis using Multiple Regression

Overview:

This project aimed to analyze and determine what global factors are the most important in predicting the employment rates of refugees, immigrants, and citizens. This project was a follow on analysis to my research “Does Public Opinion Matter?:  An Analysis of the Impact of National Public Opinion on Refugee Labor Force Participation” in which I analyzed the correlations between public sentiment and refugee employment. This project found over three different analyses that the most valuable variables in predicting employment in the relevant groups are a country’s democracy rating, their GDP per capita, and the number of refugees in that country. 

Background:

This project was done for the Georgetown statistics class Math 240: Regression Analysis in which we learned the mathematical backings of a variety of regression methods and their implementation in R. This project was inspired by my previous research as I was interested in the predictability power of national statistics on employment for refugees, immigrants, and citizens. I used R and multiple regression to perform the analysis. 

Analysis Process

I gathered data to answer this question from a variety of sources including The World Bank, National Labor Force Statistics Offices, The Economist Democracy Ratings project, academic papers, and other sources. After gathering the data I used R to clean and organize it into observations consisting of  a single country with data primarily being used from 2016 as it was the most recent year available from many sources. For each of the populations (refugees, immigrants, and citizens) I then ran step forward and step-back regression algorithms to analyze which variables had the most predictive power and the lowest Z-scores. After arriving at my results I checked each round of analysis for normality, independence, with normal QQ plots, residual plots, and in one case a Shapiro wilks test for normality. 

Results

In the end I found that a country’s Democracy Rating and 2016 GDP Per Capita were the highest predictors of refugee employment, their national Labor Force Participation rates and number of Refugees under UNHCR's mandate were the strongest predictors of immigrant employment, and their Democracy Score and the number of Refugees they had were the strongest predictors of national labor force participation rates. 
Overall there was a general trend that the more democratic a nation was and the more refugees that it accepted the low the model predicted that its employment rates would be. 
I concluded that this data could be skewed due to national interference or difference in statistics from non-democratic nations
I also concluded that if there are more refugees there is often a large informal labor market which can artificially deflate employment numbers


