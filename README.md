# MLB-Pythagorean-Expectation-and-Playoff-Performance-Analysis

## Project Overview
This project explores the impact of various baseball statistics on Major League Baseball (MLB) playoff win percentages. By analyzing data from MLB seasons 2012-2023, we investigate how different performance metrics such as home runs per plate appearance, on-base plus slugging (OPS) ratios, and Pythagorean win expectations correlate with teams' success in the playoffs.

## Objectives
- To determine the relationship between various batting and pitching statistics and MLB playoff outcomes.
- To apply statistical analysis techniques to identify key predictors of playoff success.
- To visualize the impact of these statistics on playoff win percentages using scatter plots and regression analysis.

## Data Sources
- MLB team performance data scraped from Baseball Reference covering multiple seasons from 2012 to 2023.
- Playoff game outcomes and team performance metrics collected over the same period.

## Methodology
- **Data Scraping**: Utilized the `rvest` package to scrape MLB statistics for batting against and batting for from Baseball Reference.
- **Data Cleaning and Transformation**: Processed the scraped data to calculate ratios like home runs per plate appearance and singles per plate appearance for better comparative analysis.
- **Statistical Analysis**:
  - Performed linear regression to explore the relationships between the calculated ratios and playoff winning percentages.
  - Used residual analysis to assess the fit of the models and identify outliers.
- **Visualization**:
  - Created scatter plots with regression lines to visualize trends.
  - Used `ggplot2` for advanced graphical representations.

## Key Findings
- **Statistical Significance**: Certain statistics like HR per PA Ratio and OPS Ratio showed a significant correlation with playoff win percentages, indicating that teams with higher values in these metrics tend to perform better in playoffs.
- **Model Fit**: Residual analysis helped in understanding the variability in playoff performances that wasn't explained by the models.
- **Predictive Analysis**: The use of Pythagorean expectation provided insights into the expected performance of teams based on their run differentials.

In Conclusion, This project provides a thorough analysis of MLB team performance metrics and their impact on postseason outcomes, using a data-driven approach supported by statistical models and visualizations
