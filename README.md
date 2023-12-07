# Transfer Models

This project explored the trade off between machine learning model performance and fairness and developed bias mitigation techniques via transfer learning. The results of this project was subsequently published in: [Gardner, J., Yu, R., Nguyen, Q., Brooks, C., & Kizilcec, R. (2023, June). Cross-Institutional Transfer Learning for Educational Models: Implications for Model Performance, Fairness, and Equity. In Proceedings of the 2023 ACM Conference on Fairness, Accountability, and Transparency (pp. 1664-1684)](https://arxiv.org/pdf/2305.00927.pdf)

------

A repository for ongoing work on cross-institutional transfer learning.

__Note:__ At this time please do not commit institutional data (only scripts to process that data).

* `transfer` contains shared Python source code for preprocessing and modeling.

* `scripts/institution_specific` contains preprocessing scripts for individual institutions (in subdirectories). If you are writing preprocessing code for your institution, place it here.

## Virtual Environment

When running the Python code in this repository, do so within a conda environment. Use Python version 3.7 to ensure compatibility of trained models. *It is critical that all Python code is executed from within this virtual environment.*

You can set up a conda environment using the following steps. *Steps 1-2 only need to be completed

1. Ensure that you have [installed anaconda](https://docs.anaconda.com/anaconda/install/). Once conda is installed, update it by running

``` 
conda update conda
```

2. Create the virtual environment, which is named `transfer`:

```
conda env create -f environment.yml
```

3. Activate the new environment.

``` 
conda activate transfer
```

4. Install the source code module and `pandera`, which doesn't play nice with conda.

``` 
pip3 install pandera && pip3 install -e .
```

## Data pre-processing

To perform preprocessing on data for an institution, run the preprocessing script for that institution. Different institutional scripts may accept different flags; look at each script in `institution_specific` for documentation on how to use it.

As an example, UM data can be preprocessed by running

```
python scripts/institution_specific/um/preprocess.py
```

Institution-specific scripts should output a [feather](https://pandas.pydata.org/docs/reference/api/pandas.DataFrame.to_feather.html) file. 

The preprocessing step should only need to be run once, unless the raw data changes.

## Validation

Once an institution-specific dataset has been generated, the data must pass validation to ensure the downstream processing/training/evaluation pipeline work correctly.

To validate your institution-specific data after preprocessing, run

```
python scripts/validate_data.py --input_fp $DATA_FP
```

where `DATA_FP` is the path to your institution-specific data.

## Modeling

After the data from your institution(s) of interest has been preprocessed, train a model as follows; when the training/evaluation completes you will see results similar to the following:

```
$ python scripts/train.py --src_institution um
2021-12-03 16:51:09 INFO     src institution is um, target institution is um
2021-12-03 16:51:09 INFO     experiment uid is src_instutitionum_target_institutionum
2021-12-03 16:51:09 DEBUG    src reading data from ./data/preprocessed/um.feather
2021-12-03 16:51:09 DEBUG    reading src data complete; preprocessing src data
2021-12-03 16:51:09 DEBUG    processing column sex with 3 unique values
2021-12-03 16:51:09 DEBUG    processing column ethnicity with 8 unique values
2021-12-03 16:51:09 DEBUG    processing column urm_status with 3 unique values
2021-12-03 16:51:09 DEBUG    processing column cip2_major_1 with 25 unique values
2021-12-03 16:51:09 DEBUG    processing column cip2_major_2 with 18 unique values
2021-12-03 16:51:09 DEBUG    processing column modality with 1 unique values
2021-12-03 16:51:09 DEBUG    preprocessing src data complete
2021-12-03 16:51:09 DEBUG    splitting src data into train/test/validation
2021-12-03 16:51:09 DEBUG    splitting complete
2021-12-03 16:51:09 INFO     training model.
2021-12-03 16:51:09 INFO     fitting model.
2021-12-03 16:51:59 INFO     evaluating model.
2021-12-03 16:51:59 INFO     validation metrics on source institution:
{'accuracy': 0.8522292993630574,
 'auc': 0.9510539834374032,
 'confusion_matrix': array([[  39,    0],
       [ 348, 1968]]),
 'f_score_0': 0.18309859154929578,
 'f_score_1': 0.9187675070028012,
 'precision_0': 0.10077519379844961,
 'precision_1': 1.0,
 'recall_0': 1.0,
 'recall_1': 0.8497409326424871,
 'support_0': 39,
 'support_1': 2316}
2021-12-03 16:51:59 INFO     saved model to ./models/src_instutitionum_target_institutionum.joblib
```
