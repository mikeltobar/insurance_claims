# Insurance claim analysis

![calculator-385506_1280](https://user-images.githubusercontent.com/81832365/216644407-200349ab-4987-46fa-8af7-e1a23793f62f.jpg)

## Intro

In this project, I have gone through a dataset that contains insurance claims made by about 90,000 workers. This project combines inferential statistics, regressions, predictions or ANOVA, among others.

## Data

The dataset used is available [here](https://www.kaggle.com/c/actuarial-loss-estimation/data), and it has been produced by the Actuaries Institute of Australia, the Singapore Actual Society, and the Institute and Faculty of Actuaries. The dataset will be added to the project as train1.csv.

## The analysis

As introduced before, we will apply a variety of analysis and algorithms to the data. The analysis performed includes the following:

* Normality analysis
* Inferential statistical analysis
  * Confidence interval of the poblational mean of the final cost
  * Hypothesis contrast 
* Linear regression
  * Interpretation
  * Quality analysis
  * Prediction
* Logistical regression
  * Predictive model creation
  * Interpretation and quality analysis
  * Prediction
* ANOVA (one factor)
  * Hypothesis contrast
  * Model and factor analysis
  * Quality and fitness analysis
* ANOVA (multi factor)
  * Interactions and effects analysis
  * Model design and results interpretation

## Conclusions

We depart from a dataset with variables that don't follow normal distributions, so in some cases we will work with logarithmical ones. Thanks to to the hypothesis contrast we can assert that the mean payment for women is more than 1000$ higher than men's.

We get some significative variables in the linear regression model, with a fairly good adjustment, with a median value far from the mean one. The logistic regression model doesn't excel with precision, sensitivity or specificity, but it has a good accuracy. 

Finally, the first ANOVA signals a significative classifier, with not a big influence in the final values, whereas the second ANOVA (multi-factor) asserts the factors it studies are significative, but without interaction.
