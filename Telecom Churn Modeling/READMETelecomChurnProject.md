Telecom Customer Churn Analysis Using Machine Learning

This project focuses on predicting customer churn in the telecom industry using machine learning techniques. The objective was to analyze customer data and build predictive models that identify customers likely to discontinue services.

The project began with data handling and exploration using pandas and NumPy for cleaning, structuring, and analyzing the dataset. Missing values were addressed using imputers, and categorical variables were transformed using encoding techniques during the data preprocessing phase.

To better understand trends and relationships within the data, visualizations were created using matplotlib and seaborn, identifying patterns related to churn behavior.

The dataset was then split into training and testing sets to ensure proper model evaluation. Multiple models were developed, including:
  Logistic Regression
  Decision Tree models
  Random Forest

To improve performance, hyperparameter tuning was performed using RandomizedSearchCV, optimizing model parameters efficiently.
Techniques such as SMOTE (Synthetic Minority Over-sampling Technique) and RandomUnderSampler were applied to balance class distributions and improve predictive performance.

Model performance was evaluated using metrics such as:
  Accuracy
  ROC Curve and AUC
  Confusion Matrix
  Precision, Recall, and F1-score
The final model provided actionable insights into customer retention strategies and demonstrated a complete end-to-end machine learning pipeline—from data preprocessing to model optimization and evaluation.
