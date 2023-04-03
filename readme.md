# Applying double machine learning and BART methods to the American Causal Inference Conference 2022 Data Challenge

 This GitHub repository is built for the R language implementation of Bayesian Additive Regression Tree(BART), Bayesian Causal Forest(BCF), and Double Machine Learning methods for American Causal Inference Conference 2022 Data Challenge. This work was topic for my master thesis in Technical University of Munich(TUM). 
- - -
The following R scripts are included:
- **p_score.r**:   Estimation of propensity score via BART for classification.
- **bart_method1.r**: Estimation of SATTs and construction of corresponding 90% credible intervals via BART method1.
- **bart_method2.r**: Estimation of SATTs and construction of corresponding 90% credible intervals via BART method2.
- **bcf_test.r**: Estimation of SATTs and construction of corresponding 90% credible intervals via BCF.
- **dml_test.r**: Estimation of $SATT_{overall}$, $SATT_{yearly}$,  and construction of corresponding 90% confidence intervals via DML.
- **dml_time.r**: Estimation of $SATT_{overall}$, $SATT_{yearly}$,  and construction of corresponding 90% confidence intervals via DML with year as an additional covariate.
- **dml_subgroup.r**: Estimation of $SATT_{subgroup}$ and construction of corresponding 90% confidence intervals via DML.
- **evaluation.r**: Functions about metrics RMSE and uncertainty interval coverage rate which could be used for evaluation of model performances.