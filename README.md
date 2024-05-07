## MLB-Pythagorean-Expectation-and-Playoff-Performance-Analysis

## Project Overview

This project explores how various baseball statistics impact playoff success in Major League Baseball (MLB) from 2012 to 2023. We examine batting and pitching performance metrics and their correlations with playoff winning percentages using Pythagorean expectations. By analyzing comprehensive team data, this study reveals how ratios like home runs per plate appearance, singles per plate appearance, and strikeout ratios predict playoff success.

## Objectives

Examine Relationships: Explore the relationships between batting/pitching statistics and playoff success.

Identify Key Predictors: Apply statistical analysis to uncover key metrics that predict playoff performance.

Visualize Statistical Impact: Utilize scatter plots and regression analysis to show the impact of these statistics on playoff win percentages.

## Data Sources

Baseball Reference: MLB team data from Baseball Reference, spanning the 2012-2023 seasons.

Playoff Metrics: Outcomes and performance metrics for playoff games during the same period.

## Methodology

## Data Scraping

Used rvest to scrape MLB statistics for batting (both for and against) from Baseball Reference.

Extracted multiple tables, processed them for consistency, and created ratios for comparative analysis.

## Data Cleaning and Transformation

Processed the scraped data to calculate relevant metrics, such as home runs per plate appearance, singles per plate appearance, unintentional walks, and strikeouts.

Adjusted data fields for consistent team naming and corrected inaccuracies.

## Statistical Analysis

Applied linear regression to explore relationships between calculated ratios (e.g., home run ratio) and playoff winning percentages.

Conducted residual analysis to identify model accuracy and unexplained variability.

Performed exploratory data analysis to summarize trends and patterns in the data.

## Visualization

Created scatter plots with regression lines to visualize data trends and identify influential variables.

Utilized ggplot2 for advanced graphical representations of the relationships.

## Key Findings

Statistical Significance

Home Runs per Plate Appearance (HR/PA) and On-base Plus Slugging (OPS) ratios have a strong correlation with playoff win percentages. Teams with higher ratios tend 
to perform better in the playoffs.

Model Fit and Residual Analysis

Residual analysis highlighted variability unexplained by the model, providing insights into potential outliers and model refinements.

Identified teams that exceeded or underperformed relative to their expected Pythagorean expectations.

## Predictive Analysis

Pythagorean expectations provided valuable insights into predicting team success based on their run differentials.

Regression models utilizing home run ratios, singles ratios, and strikeouts identified key performance drivers.

## Conclusion

This project comprehensively analyzes the influence of MLB performance metrics on playoff outcomes. By using detailed statistical analysis and visualizations, this 
study reveals key predictors that directly impact team success. The combination of historical data and predictive models provides useful insights for future playoff 
predictions and strategic decision-making in professional baseball.

