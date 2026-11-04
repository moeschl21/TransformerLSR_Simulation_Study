
# Simulation study to tvaluate the TransformerLSR approach for joint modeling

This repository contains the code used for the simulation study conducted as part of my master's thesis at the Technical University of Darmstadt.

The study evaluates the predictive performance of TransformerLSR under small-sample conditions and compares it with the classical joint modeling approach joineRML.

## Repository Structure

- `TransformerLSR/` – Python implementation of TransformerLSR, including model training and evaluation.
- `joineRML/` – R scripts for model fitting and prediction.
- `simulation/` – Data generation and simulation settings.
- `notebooks/` – Jupyter and Kaggle notebooks used for experiments.
- `results/` – Simulation results, tables, and figures.
- `environment/` – Software versions and package dependencies.

## Code Attribution and Modifications

The TransformerLSR implementation is based on the original code developed by Zhang, Zhao, and Xu (2025).

The original implementation was adapted and extended for the purposes of this simulation study. Modifications include adjustments to the simulation framework, model training and evaluation procedures, as well as bug fixes and other implementation improvements.

The joineRML analyses were implemented in R using the joineRML package developed by Hickey et al. (2018).

## References

1. Zhang, Z., Zhao, Y., & Xu, Y. (2025). *TransformerLSR: Attentive Joint Model of Longitudinal Data, Survival, and Recurrent Events with Concurrent Latent Structure.*

2. Hickey, G. L., Philipson, P., Jorgensen, A., & Kolamunnage-Dona, R. (2018). *joineRML: A joint model and software package for time-to-event and multivariate longitudinal outcomes.* BMC Medical Research Methodology, 18, 50. https://doi.org/10.1186/s12874-018-0502-1

