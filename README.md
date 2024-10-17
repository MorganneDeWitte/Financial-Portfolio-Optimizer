
# Portfolio Optimization Dashboard and Deep Learning Exploration

### Author: Morganne De Witte

This repository contains two main sections:

1. **Financial Portfolio Optimization in R**:
    - `app.R`: A Shiny app for financial portfolio analysis and optimization, using historical data from Yahoo Finance. This script utilizes libraries such as `quantmod` and `PortfolioAnalytics` to create an interactive dashboard for efficient frontier calculations and portfolio optimizations.
   
2. **Deep Learning Exploration**:
    - A set of Jupyter Notebooks where various deep learning architectures are explored, focusing on predicting future asset prices. **Long Short-Term Memory (LSTM)** networks and **Transformer** models are tested for their ability to dynamically adjust portfolio weights based on predicted returns.

## Key Features:

* **Data Handling & Financial Metrics**: Retrieves real-world financial data using the Yahoo Finance API and computes critical financial metrics such as Sharpe Ratio, annualized returns, and volatility.

* **Portfolio Optimization**: Implements classical MPT optimization as well as ROI optimization to maximize the Sharpe Ratio, ensuring optimal asset allocation with minimal risk.

* **Deep Learning Exploration**: Uses both LSTM and Transformer neural networks to predict future asset prices, dynamically adjusting portfolio weights based on forecasted returns.

* **Out-of-sample testing**: Incorporates a framework to evaluate the performance of portfolios on out-of-sample data, ensuring real-world applicability.

* **Performance Comparison**: Compares the results from MPT, LSTM, and Transformers including a detailed analysis of risk-adjusted returns relative to the S&P 500, used as a benchmark for portfolio performance.

## Project Structure:
- [`/shiny-portfolio-optimizer/`](./shiny-portfolio-optimizer/): Contains the R Shiny app for financial data analysis.
- [`/deep-learning-approach/`](./deep-learning-approach/): Contains Jupyter Notebooks for exploring deep learning methods (LSTM and Transformers).


## Future Directions:
- Expand financial modeling with reinforcement learning techniques.
- Enhance the Shiny app with real-time stock prediction using deep learning models.