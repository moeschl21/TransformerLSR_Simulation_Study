
# Simulation study to tvaluate the TransformerLSR approach for joint modeling

This repository contains the code used for the simulation study conducted as part of my master's thesis at the Technical University of Darmstadt.

The study evaluates the predictive performance of TransformerLSR under small-sample conditions and compares it with the classical joint modeling approach joineRML.


## Repository Structure

The repository is organized into the following directories:

- `TransformerLSR/` – TransformerLSR implementation based on Zhang et al. (2025), including modifications and bug fixes made for this study.

- `R-Scripts/` – R scripts used for analyses, model specification, diagnostic checks, and selection of the joineRML model configuration.

- `Notebooks/` – Notebooks used for data generation, model training, and evaluation, organized by censoring scenario:
  - `2%_censoring/` – Simulation notebooks for approximately 2% censoring.
  - `13%_censoring/` – Simulation notebooks for approximately 13% censoring.
  - `59%_censoring/` – Simulation notebooks for approximately 59% censoring.

Each censoring scenario contains Python notebooks for data generation, TransformerLSR training, and evaluation, as well as R notebooks for joineRML model fitting and evaluation.


## Code Attribution and Modifications

The TransformerLSR implementation is based on the original code developed by Zhang, Zhao, and Xu (2025).

The original implementation was adapted and extended for the purposes of this simulation study. Modifications include adjustments to the simulation framework, model training and evaluation procedures, as well as bug fixes and other implementation improvements.

The joineRML analyses were implemented in R using the joineRML package developed by Hickey et al. (2018).

## References

1. Zhang, Z., Zhao, Y., & Xu, Y. (2025). *TransformerLSR: Attentive Joint Model of Longitudinal Data, Survival, and Recurrent Events with Concurrent Latent Structure.*

2. Hickey, G. L., Philipson, P., Jorgensen, A., & Kolamunnage-Dona, R. (2018). *joineRML: A joint model and software package for time-to-event and multivariate longitudinal outcomes.* BMC Medical Research Methodology, 18, 50. https://doi.org/10.1186/s12874-018-0502-1

