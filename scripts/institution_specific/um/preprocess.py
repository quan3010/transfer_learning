"""
Script to preprocess umich data.

Usage:
python scripts/institution_specific/um/preprocess.py
"""

import click
from math import floor
import os

import numpy as np
import pandas as pd
from scipy.stats import zscore

from transfer.keys import MISSING_VALUE
from transfer.config import UM_PREPROCESSED_CSV_FP, UM_PREPROCESSED_FEATHER_FP
from transfer.validation import COURSE_COMBINED_TYPES, COURSE_TYPES, \
    CIP2_CATEGORIES


def weightstats(x, weights=None, stat='mean'):
    """
    Extended stats function that accommodates missing values and sample weights.
    """
    x = np.asarray(x)
    if weights is None:
        weights = np.ones(len(x))
    else:
        weights = np.asarray(weights).astype(float)
    indices = ((~np.isnan(x)) & (~np.isnan(weights)))
    if ~indices.any():
        return np.nan
    else:
        x = x[indices]
        weights = weights[indices]
        mean = np.dot(x.T, weights) / weights.sum()
        if stat == 'mean':
            return mean
        std = np.sqrt(np.dot(((x - mean) ** 2).T, weights) / weights.sum())
        if stat == 'std':
            return std


@click.command()
@click.option('--year_from', default=2012, help='Filter data from this year')
@click.option('--year_to', default=2019, help='Filter data until this year')
def main(year_from, year_to, term="FA", cohort="Freshman"):
    student_term_fields_to_read = ['CUM_GPA',
                                   'CURR_GPA',
                                   'TERM_CD',
                                   'TERM_SHORT_DES',
                                   'ACAD_LVL_BOT_SHORT_DES',
                                   'STDNT_ID']

    stdnt_term_info = pd.read_csv(
        "/data0/power/LARC_20210209_STDNT_TERM_INFO.csv",
        usecols=student_term_fields_to_read)
    stdnt_term_info['term'] = stdnt_term_info['TERM_SHORT_DES'].astype(str).str[
                              :2]
    stdnt_term_info['year'] = stdnt_term_info['TERM_SHORT_DES'].astype(str).str[
                              3:7].astype(int)

    if term == "FA":
        month = 9
    elif term == "WN":
        month = 1
    else:
        month = 5

    list_retention = stdnt_term_info.copy().loc[
        (stdnt_term_info['year'].between(year_from + 1, year_to + 1) &
         (stdnt_term_info['term'] == term)), ['STDNT_ID', 'year']]

    list_retention['year'] = list_retention['year'] - 1
    list_retention['next_year'] = list_retention['year'] + 1

    stdnt_term_info = stdnt_term_info.loc[
        (stdnt_term_info['year'].between(year_from, year_to) &
         (stdnt_term_info['term'] == term) &
         (stdnt_term_info['ACAD_LVL_BOT_SHORT_DES'] == cohort)),]

    stdnt_term_info['next_year'] = stdnt_term_info['year'] + 1

    list_retention = stdnt_term_info.loc[:,
                     ['STDNT_ID', 'year', 'next_year']].merge(list_retention,
                                                              how='inner',
                                                              on=['STDNT_ID',
                                                                  'year',
                                                                  'next_year'])

    student_fields_to_read = ['STDNT_ID',
                              'UM_DGR_1_MAJOR_1_CIP_CD',
                              'UM_DGR_1_MAJOR_2_CIP_CD',
                              # 'UM_DGR_1_MINOR_1_CIP_CD',
                              # 'UM_DGR_1_MINOR_2_CIP_CD',
                              'MAX_ACT_ENGL_SCR',
                              'MAX_ACT_MATH_SCR',
                              'STDNT_BIRTH_YR',
                              'STDNT_BIRTH_MO',
                              'STDNT_ETHNC_GRP_SHORT_DES',
                              'STDNT_SEX_SHORT_DES',
                              'HS_GPA',
                              'MAX_SATI_MATH_SCR',
                              'MAX_SATI_VERB_SCR',
                              'STDNT_DMSTC_UNDREP_MNRTY_DES']

    stdnt_info = pd.read_csv("/data0/power/LARC_20210209_STDNT_INFO.csv",
                             usecols=student_fields_to_read)

    # rename columns
    stdnt_info = stdnt_info.rename(columns={'STDNT_SEX_SHORT_DES': 'sex',
                                            'STDNT_ETHNC_GRP_SHORT_DES': 'ethnicity',
                                            'HS_GPA': 'gpa_high_school',
                                            'STDNT_DMSTC_UNDREP_MNRTY_DES': 'urm_status',
                                            'UM_DGR_1_MAJOR_1_CIP_CD': 'cip6_major_1',
                                            'UM_DGR_1_MAJOR_2_CIP_CD': 'cip6_major_2',
                                            # 'UM_DGR_1_MINOR_1_CIP_CD': 'cip6_minor_1',
                                            # 'UM_DGR_1_MINOR_2_CIP_CD': 'cip6_minor_2',
                                            'MAX_ACT_ENGL_SCR': 'act_english',
                                            'MAX_ACT_MATH_SCR': 'act_math',
                                            'MAX_SATI_MATH_SCR': 'sat_math',
                                            'MAX_SATI_VERB_SCR': 'sat_verbal'})

    # change column type

    # compute age from birth year & month in Fall 2016 (assume starting in september)

    stdnt_info = stdnt_info.merge(stdnt_term_info.loc[:, ['STDNT_ID', 'year']],
                                  on='STDNT_ID')
    stdnt_info['age'] = stdnt_info['year'] - stdnt_info['STDNT_BIRTH_YR']
    stdnt_info['age'] = np.where(stdnt_info['STDNT_BIRTH_MO'] > month,
                                 stdnt_info['age'], stdnt_info['age'] + 1)
    stdnt_info = stdnt_info.drop(['STDNT_BIRTH_YR', 'STDNT_BIRTH_MO'], axis=1)
    stdnt_info = stdnt_info.replace({"Unknown": np.NaN})

    student_term_course_fields_to_read = ['STDNT_ID',
                                          'CRSE_CMPNT_CD',
                                          'CRSE_CMPNT_SHORT_DES',
                                          'CRSE_CIP_CD',
                                          'CRSE_GRD_OFFCL_CD',
                                          'GRD_PNTS_PER_UNIT_NBR',
                                          'UNITS_TAKEN_NBR',
                                          'CRSE_ID_CD',
                                          'GRD_BASIS_ENRL_CD',
                                          'TERM_SHORT_DES']
    stdnt_term_class_info = pd.read_csv(
        "/data0/power/LARC_20210209_STDNT_TERM_CLASS_INFO.csv",
        usecols=student_term_course_fields_to_read)

    # Filter term & freshman by merging with stdnt_term_info
    stdnt_term_class_info = pd.merge(stdnt_term_class_info, stdnt_term_info[
        ['STDNT_ID', 'TERM_SHORT_DES']],
                                     on=['STDNT_ID', 'TERM_SHORT_DES'])
    stdnt_term_class_info = stdnt_term_class_info.replace(
        {"In Person": "inperson",
         "Distance": "online",
         "WWW": "online",
         "Hybrid": "online"})

    stdnt_term_class_info.loc[
        ~stdnt_term_class_info["CRSE_CMPNT_CD"].isin(
            ['LEC', 'LAB', 'DIS', 'SEM']), "CRSE_CMPNT_CD"] = "others"
    stdnt_term_class_info['cip2'] = stdnt_term_class_info[
        'CRSE_CIP_CD'].str.extract('(.*(?=\.))')

    stdnt_term_class_info2 = stdnt_term_class_info.copy()

    # ## Calculate weighted gpa_zscore and mean & stddev

    # Filter to keep graded elements only
    stdnt_term_class_gpa = stdnt_term_class_info2.loc[
        stdnt_term_class_info2['GRD_BASIS_ENRL_CD'] == 'GRD',].copy()

    # ## Calculate weighted mean and weighted SD

    df_gpa = stdnt_term_class_gpa[
        ['STDNT_ID', 'TERM_SHORT_DES']].drop_duplicates().sort_values(
        'STDNT_ID')

    # Compute gpa_avg
    df_gpa = pd.merge(df_gpa,
                      stdnt_term_class_gpa.groupby(
                          ['STDNT_ID', 'TERM_SHORT_DES']).apply(
                          lambda x: weightstats(x['GRD_PNTS_PER_UNIT_NBR'],
                                                weights=x['UNITS_TAKEN_NBR'],
                                                stat='mean')).reset_index(
                          name='gpa_avg'),
                      how='left',
                      on=['STDNT_ID', 'TERM_SHORT_DES'])

    # Compute gpa_stddev
    df_gpa = pd.merge(df_gpa,
                      stdnt_term_class_gpa.groupby(
                          ['STDNT_ID', 'TERM_SHORT_DES']).apply(
                          lambda x: weightstats(x['GRD_PNTS_PER_UNIT_NBR'],
                                                weights=x['UNITS_TAKEN_NBR'],
                                                stat='std')).reset_index(
                          name='gpa_stddev'),
                      how='left',
                      on=['STDNT_ID', 'TERM_SHORT_DES'])

    # Compute zscore of grade points for each course in a term
    stdnt_term_class_gpa['zscore'] = \
        stdnt_term_class_gpa.groupby(['TERM_SHORT_DES', 'CRSE_ID_CD'])[
            'GRD_PNTS_PER_UNIT_NBR'].transform(
            lambda x: zscore(x, nan_policy='omit'))

    # Compute gpa_zscore_avg
    df_gpa = pd.merge(df_gpa,
                      stdnt_term_class_gpa.groupby(
                          ['STDNT_ID', 'TERM_SHORT_DES']).apply(
                          lambda x: weightstats(x['zscore'],
                                                weights=x['UNITS_TAKEN_NBR'],
                                                stat='mean')).reset_index(
                          name='gpa_zscore_avg'),
                      how='left',
                      on=['STDNT_ID', 'TERM_SHORT_DES'])

    # Compute gpa_zscore_stddev
    df_gpa = pd.merge(df_gpa,
                      stdnt_term_class_gpa.groupby(
                          ['STDNT_ID', 'TERM_SHORT_DES']).apply(
                          lambda x: weightstats(x['zscore'],
                                                weights=x['UNITS_TAKEN_NBR'],
                                                stat='std')).reset_index(
                          name='gpa_zscore_stddev'),
                      how='left',
                      on=['STDNT_ID', 'TERM_SHORT_DES'])

    stdnt_term_class_gpa[
        ['STDNT_ID', 'TERM_SHORT_DES']].drop_duplicates().sort_values(
        'STDNT_ID')

    # ## Create dummies for course type and course cip

    # Create a unique df with course_id and course_components
    df_course = stdnt_term_class_info2[
        ['CRSE_ID_CD', 'CRSE_CMPNT_CD']].drop_duplicates().sort_values(
        'CRSE_ID_CD').reset_index(drop=True)

    # Combine all course components into a sorted list alphabetically, groupby course_id
    df_course = df_course.groupby('CRSE_ID_CD')['CRSE_CMPNT_CD'].apply(
        lambda x: sorted(list(x))).reset_index(
        name='crse_type')

    # Convert list into string
    df_course['crse_type'] = df_course['crse_type'].apply(
        lambda x: '-'.join(map(str, x)))

    # Merge with course cip code
    df_course = df_course.merge(
        stdnt_term_class_info2[
            ['CRSE_ID_CD', 'cip2']].drop_duplicates().sort_values(
            'CRSE_ID_CD'))

    # Create a dummies 
    df_course = df_course.set_index('CRSE_ID_CD')

    df_course = pd.get_dummies(
        df_course[['crse_type', 'cip2']],
        prefix=['units_type_', 'units_cip2_'], prefix_sep='').max(
        level=0).reset_index()

    # Compute the total number of units per course
    df_course2 = \
        stdnt_term_class_info2[['CRSE_ID_CD', 'CRSE_CMPNT_CD',
                                'UNITS_TAKEN_NBR']].drop_duplicates().sort_values(
            'CRSE_ID_CD').reset_index(drop=True).groupby('CRSE_ID_CD')[
            'UNITS_TAKEN_NBR'].sum().reset_index()

    # Mutiply the course component combo by the total number of units
    df_course2 = df_course.iloc[:, 1:].multiply(df_course2['UNITS_TAKEN_NBR'],
                                                axis="index")

    # Concatinate course_id
    df_course2 = pd.concat([df_course['CRSE_ID_CD'], df_course2], axis=1)

    # Create dummies for grade F, grade starts with I (incompleted), and grade starts with W (withdraw)
    stdnt_term_class_info2['units_failed'] = np.where(
        stdnt_term_class_info2['CRSE_GRD_OFFCL_CD'] == 'F', 1, 0) * \
                                             stdnt_term_class_info2[
                                                 'UNITS_TAKEN_NBR']
    stdnt_term_class_info2['units_incompleted'] = np.where(
        stdnt_term_class_info2['CRSE_GRD_OFFCL_CD'].str.startswith('I'), 1, 0) * \
                                                  stdnt_term_class_info2[
                                                      'UNITS_TAKEN_NBR']
    stdnt_term_class_info2['units_withdrawn'] = np.where(
        stdnt_term_class_info2['CRSE_GRD_OFFCL_CD'] == 'W', 1, 0) * \
                                                stdnt_term_class_info2[
                                                    'UNITS_TAKEN_NBR']

    # Merge student features with course features, and add columns for any missing course types
    stdnt_term_class_info2 = pd.merge(stdnt_term_class_info2, df_course2,
                                      on='CRSE_ID_CD')
    for course_type in COURSE_TYPES + COURSE_COMBINED_TYPES:
        colname = "units_type_{}".format(course_type)
        if colname not in stdnt_term_class_info2.columns:
            stdnt_term_class_info2[colname] = float(0)

    # Handle courses with >2 types; for any such course we retain only
    # the first two course types (i.e.
    # 'units_type_DIS-LEC-SEM-others' -> 'units_type_DIS-LEC'
    for colname in stdnt_term_class_info2:
        col_course_types = sorted(colname.replace("units_type_", "").split("-"))
        ntypes = sum([x in col_course_types for x in COURSE_TYPES])
        if ntypes > 2:
            print("[DEBUG] detected types {}".format(col_course_types))
            coldata = stdnt_term_class_info2.pop(colname)
            target_colname = "units_type_{}-{}".format(
                col_course_types[0], col_course_types[1])
            print("[DEBUG] reassigning column {} values to column {}".format(
                colname, target_colname
            ))
            stdnt_term_class_info2[target_colname] += coldata

    # Drop features that were already dummified
    stdnt_term_class_info2.drop(
        ['GRD_BASIS_ENRL_CD', 'CRSE_GRD_OFFCL_CD', 'CRSE_ID_CD',
         'CRSE_CMPNT_CD', 'CRSE_CMPNT_SHORT_DES',
         'CRSE_CIP_CD', 'GRD_PNTS_PER_UNIT_NBR'], axis=1, inplace=True)

    # ## Sum all units measures by student per term

    stdnt_term_class_units = stdnt_term_class_info2.groupby(
        ['STDNT_ID', 'TERM_SHORT_DES']).sum().reset_index()
    stdnt_term_class_units = stdnt_term_class_units.rename(
        columns={'UNITS_TAKEN_NBR': 'units'})

    # ## Merge with gpa measures, units measures, and demographics info

    stdnt_term = pd.merge(stdnt_term_info, df_gpa,
                          on=['STDNT_ID', 'TERM_SHORT_DES'])
    stdnt_term = pd.merge(stdnt_term, stdnt_term_class_units,
                          on=['STDNT_ID', 'TERM_SHORT_DES'])
    stdnt_term = pd.merge(stdnt_term, stdnt_info, on=['STDNT_ID', 'year'])

    # Create retention in 12 months
    stdnt_term['retention'] = stdnt_term['STDNT_ID'].isin(
        list_retention['STDNT_ID'])

    # Rename columns
    stdnt_term = stdnt_term.rename(
        columns={"CUM_GPA": "gpa_cumulative", "B": "c"})

    # Import transfer csv
    stdnt_transfer = pd.read_csv(
        "/data0/power/LARC_20210209_STDNT_TERM_TRNSFR_INFO.csv")

    # Filter for incoming transferred credits only
    stdnt_transfer = stdnt_transfer.rename(
        columns={"ARTCLT_TERM_CD": "TERM_CD"})
    stdnt_transfer = stdnt_transfer.loc[
        stdnt_transfer['TRNSFR_SRC_TYP_CD'] == "E", ['STDNT_ID', 'TERM_CD',
                                                     'UNITS_TRNSFR_NBR']]

    # Sum units transferred per student
    stdnt_transfer = stdnt_transfer.groupby(
        ['STDNT_ID', 'TERM_CD']).sum().reset_index()
    stdnt_transfer = stdnt_transfer.rename(
        columns={'UNITS_TRNSFR_NBR': 'units_transferred'})

    # Merge transfer credits
    stdnt_term = stdnt_term.merge(stdnt_transfer, how='left',
                                  on=['STDNT_ID', 'TERM_CD'])
    stdnt_term['units_transferred'].fillna(0, inplace=True)

    # Drop unused columns
    stdnt_term = stdnt_term.drop(['STDNT_ID', 'TERM_CD',
                                  'TERM_SHORT_DES', 'ACAD_LVL_BOT_SHORT_DES',
                                  'CURR_GPA'], axis=1)

    stdnt_term.age = stdnt_term.age.astype(int)

    # Convert CIP6 to categorical CIP2 integer + MISSING values
    for colname in stdnt_term.columns:
        if "cip6" in colname:
            coldata = stdnt_term.pop(colname)
            coldata = coldata.apply(
                lambda x: '{:02}'.format(floor(x))
                if not np.isnan(x) else MISSING_VALUE)
            new_colname = colname.replace("cip6", "cip2")
            stdnt_term[new_colname] = coldata
    # fill course units for in any missing CIP codes with zeros
    for cip2_code in CIP2_CATEGORIES:
        cip2_units_colname = "units_cip2_{}".format(cip2_code)
        if cip2_units_colname not in stdnt_term.columns:
            stdnt_term[cip2_units_colname] = 0.

    # Create other expected columns

    # Drop unneeded columns
    stdnt_term.drop(columns=['next_year', 'term'], inplace=True)

    # Handle columns with missing values
    stdnt_term['sex'].fillna('NotIndicated', inplace=True)

    stdnt_term.to_csv(UM_PREPROCESSED_CSV_FP, index=False)
    stdnt_term.to_feather(UM_PREPROCESSED_FEATHER_FP)

    print(f"The cleaned data has been saved at {UM_PREPROCESSED_FEATHER_FP}")
    print("This is the number of students in each year")
    print(stdnt_term['year'].value_counts())
    print("This is the retention rate in each year")
    print(stdnt_term.groupby(['year', 'retention']).size())
    print("List of output columns {}".format(sorted(stdnt_term.columns)))


if __name__ == "__main__":
    output_dir = os.path.dirname(UM_PREPROCESSED_CSV_FP)
    if not os.path.exists(output_dir):
        os.makedirs(output_dir)
    main()
