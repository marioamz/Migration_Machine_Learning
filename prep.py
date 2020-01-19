# LAPOP Migration Project
# 1-18-2020

import pandas as pd


def read_subset(df, cols):
    '''
    This function reads the data printed out in read.py and
    subsets based on list of columns we want to keep

    Input:
        fname: filename of data to read
        cols: list of columns to keep
    Output:
        coldf: dataframe with columns to keep
    '''

    return df[cols]


def group_by(df, cols):
    '''
    To graph based on averages, need to groupby to get total
    counts
    '''

    df = df.groupby(cols[0:len(cols)-1])[cols[len(cols)-1]].value_counts()

    return df.to_frame().rename(columns = {cols[len(cols)-1]:'sumvar'}).reset_index()


def wts(df, cols):

    df['weighted'] = df.weight1500 * df.sumvar

    gdf = df.groupby(['wave', 'pais', cols[len(cols)-1]])['weighted'].sum()
    return gdf.to_frame().rename(columns = {cols[len(cols)-1]:'sums'}).reset_index()


def total(df, cols):

    df['total_mig'] = df.groupby(['wave', 'pais'])['weighted'].transform('sum')
    df['perc_mig'] = (wdf.weighted / wdf.total_mig) * 100

    return df
