# cMCD-cMCSS
We first select differential features with the least absolute shrinkage and selection operator method (LASSO) and recursive feature elimination (RFE), as described in FeatureSlection.
Before model construction, ADASYN was used to ensure the comparability of the training data.
The final models, cMCD (for diagnosis) and cMCSS (for stage stratification), were constructed with XGBoost.
