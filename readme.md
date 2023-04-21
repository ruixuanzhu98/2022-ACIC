# Applying double machine learning and BART methods to the American Causal Inference Conference 2022 Data Challenge

 This GitHub repository is built for the R language implementation of Bayesian Additive Regression Tree(BART), Bayesian Causal Forest(BCF), and Double Machine Learning methods for American Causal Inference Conference 2022 Data Challenge. This work was topic for my master thesis in Technical University of Munich(TUM). 
- - -
## Script Structure:

```
.
├── BART
│   ├── bart_method1.r
│   └── bart_method2.r
├── LICENSE
├── bayesian_causal_forest
│   └── bcf_test.r
├── double_machine_learning
│   ├── dml_subgroup.r
│   ├── dml_test.r
│   └── dml_time.r
├── evaluation.r
├── p_score.r
└── readme.md
```
Introduction:
- **p_score.r**:   Estimation of propensity score via BART for classification.
- **bart_method1.r**: Estimation of SATTs and construction of corresponding 90% credible intervals via BART method1.
- **bart_method2.r**: Estimation of SATTs and construction of corresponding 90% credible intervals via BART method2.
- **bcf_test.r**: Estimation of SATTs and construction of corresponding 90% credible intervals via BCF.
- **dml_test.r**: Estimation of $SATT_{overall}$, $SATT_{yearly}$,  and construction of corresponding 90% confidence intervals via DML.
- **dml_time.r**: Estimation of $SATT_{overall}$, $SATT_{yearly}$,  and construction of corresponding 90% confidence intervals via DML with year as an additional covariate.
- **dml_subgroup.r**: Estimation of $SATT_{subgroup}$ and construction of corresponding 90% confidence intervals via DML.
- **evaluation.r**: Functions about metrics RMSE and uncertainty interval coverage rate which could be used for evaluation of model performances.
---
**Comparison of BART, Bayesian Causal Forest(BCF), and Double Machine Learning(DML) regarding RMSE**:

![plot6-10](https://user-images.githubusercontent.com/110237197/233748430-a7ba5f47-8402-4d89-b1fa-a25f82453362.jpeg =250x250)


**Comparison of BART, Bayesian Causal Forest(BCF), and Double Machine Learning(DML) regarding uncertainty interval coverage rate**:


![plot6-11](https://user-images.githubusercontent.com/110237197/233748253-aad76bfd-16e7-4dca-8e91-9fada30aa084.jpeg)

**Figure that shows treatment effect heterogeneity**:

![plot6-13](https://user-images.githubusercontent.com/110237197/233748358-f7ab8f16-fb21-4ff0-a820-b6c3fbf21b8c.jpeg)

