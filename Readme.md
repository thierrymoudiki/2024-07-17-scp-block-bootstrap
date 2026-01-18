# Sequential split conformal prediction + Block bootstrap for time series 

This is a web app illustrating a recipe (`scp-block-bootstrap`) implemented in [Python package `nnetsauce`](https://thierrymoudiki.github.io/blog/2024/07/03/python/quasirandomizednn/forecasting/nnetsauce-mts-isf2024) for time series forecasting **uncertainty quantification**. 

## How it works:

In R Console, run: 

```R
shiny::runGitHub(repo = "2024-07-17-scp-block-bootstrap", username = "thierrymoudiki")
```

Click on "Open In the browser". That's it. 

## How it looks like

![xxx](./img.png)


## Underlying process

- Split data into training, calibration and test set
- Obtain point forecast on calibration set
- Obtain calibrated residuals
- Simulate calibrated residuals using moving block bootstrap
- Obtain Point forecast on test set
- Prediction = Calibrated residuals simulations + point forecast on test set

## See also 
- https://thierrymoudiki.github.io/blog/2024/07/03/python/quasirandomizednn/forecasting/nnetsauce-mts-isf2024
- https://www.researchgate.net/publication/382589729_Probabilistic_Forecasting_with_nnetsauce_using_Density_Estimation_Bayesian_inference_Conformal_prediction_and_Vine_copulas

- https://www.researchgate.net/publication/379643443_Conformalized_predictive_simulations_for_univariate_time_series

## References 

@InProceedings{pmlr-v266-moudiki25a,
  title = 	 {Conformal Predictive Simulations for Univariate Time Series},
  author =       {Moudiki, Thierry},
  booktitle = 	 {Proceedings of the Fourteenth Symposium on Conformal and Probabilistic Prediction with Applications},
  pages = 	 {751--752},
  year = 	 {2025},
  editor = 	 {Nguyen, Khuong An and Luo, Zhiyuan and Papadopoulos, Harris and Löfström, Tuwe and Carlsson, Lars and Boström, Henrik},
  volume = 	 {266},
  series = 	 {Proceedings of Machine Learning Research},
  month = 	 {10--12 Sep},
  publisher =    {PMLR},
  pdf = 	 {https://raw.githubusercontent.com/mlresearch/v266/main/assets/moudiki25a/moudiki25a.pdf},
  url = 	 {https://proceedings.mlr.press/v266/moudiki25a.html},
  abstract = 	 {Uncertainty quantification is useful because it allows, among other things, for the as- sessment of impact of alternative, hypothetical scenarios on business metrics of interest. For example, in the context of electricity load forecasting, uncertainty quantification can help in assessing the impact of a drop in temperature on electricity demand, and taking appropriate measures to avoid blackouts. In financial forecasting, uncertainty quantification can help in assessing the impact of an increase in stock market on a portfolio, and taking appropriate measures to avoid large losses. Another application, in insurance, is the calculation of capital requirements in extremely adverse situations. In this context, despite having been available for decades, Conformal Prediction (CP) is becoming more and more popular, and a gold standard technique. This study proposes a revisited approach to uncertainty quantification for univariate time series forecasting, that can be adapted to multivariate time series forecasting. The approach adapts split conformal prediction, usually applied to tabular data but never to sequential data, to sequential data.}
}
