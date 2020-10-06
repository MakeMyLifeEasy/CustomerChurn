# CustomerChurn

Customer retention - Data analysis
Data Source: kaggle
https://www.kaggle.com/blastchar/telco-customer-churn

Telco Customer Churn - Focused customer retention programs Summary

Problem Scenario

Customer churn, also known as customer attrition, is the loss of clients or customers.
Voluntary churn occurs due to a decision made by the customer to switch to another company or service provider.  Companies tend to concentrate on voluntary churn, because it typically occurs due to factors that companies can control like company-customer relationship, how billing interactions are handled or how after-sales help is provided.
The companies are interested in identifying segments of these customers because the price for acquiring a new customer is usually higher than retaining the old one. It could result in revenue loss and indicate service deficiencies.

Churn is one of the biggest problems in the telecom industry. Research has shown that an average monthly voluntary churn rate is 1.9% - 2.1% (the customers who quit their service). The annual churn rate for telecom companies averages between 10% and 67%. 

The main goal of this project is to create a model to predict the behavior of customers. The model should be able to identify the customers who have a high probability of leaving and help devise suitable customer retention programs. 

Data Analysis

We use Telco Customer Churn dataset from the UCI repository for churn prediction. 
Each row in the dataset represents a customer record. There are 7043 customers in the dataset and 19 features without customerID (non-informative) and Churn column (target variable). Most of the categorical features have 4 or fewer unique values. 

The information included in the dataset:

Input: 
●	Services that each customer has signed up for like phone, multiple lines, internet, online security, online backup, device protection, tech support, streaming TV and movies
●	Customer account information like tenure, contract, payment method, paperless billing, monthly charges, and total charges
●	Demographic info about customers include gender, age range, and if they have partners and dependents 
Output:
●	Churn column, which indicates whether a customer has left within the last month

Methodology

We chose a decision tree to model churned and not churned customers, because of its interpretability. The decision tree will identify segments of customers with similar likelihood of churning based on the features available in the data set. The decision tree will also rank features by importance in predicting churning within each segment. The segments will be a useful basis for devising a customer retention plan.

For comparison, we will investigate a random forest classification model. A random forest creates several decision trees using smaller parts of the data set and ranks features by importance. We will compare the random forest feature rankings to the single decision tree formed with the entire data set to see how well the decision tree represents the entire data set. The feature ranking in the random forest model can also guide the customer retention plan.

We can try clustering to group similar customers together for identifying the problem and give recommendations.

For additional comparison we propose a regression model for predicting churned and not-churned customers. Comparing a regression model based on all features to a regression model based on the set of features identified as important by the decision tree and random forest can help quantify the difference between using all features or a smaller important set.

