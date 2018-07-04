Decision Tree and Random Forest
================

### Test case with the `iris` data

The `datasets::iris` is a basic data set in R provided by [UCI's R.A. Fisher](https://archive.ics.uci.edu/ml/datasets/iris)

### Comparison

Misclassifications (Without any parameter tuning)

-   Even split (75/75)

| Model | Decision Tree | Random Forest |
|-------|---------------|---------------|
| Train | 2             | 0             |
| Test  | 5             | 4             |

-   0.8 split (120/30)

| Model | Decision Tree | Random Forest |
|-------|---------------|---------------|
| Train | 5             | 0             |
| Test  | 2             | 1             |
