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


def wts(df, cols, extrawt):
    '''
    This calculates the weights if needed
    '''

    if extrawt == None:
        df['weighted'] = df.weight1500 * df.sumvar

        gdf = df.groupby(['wave', 'pais', cols[len(cols)-1]])['weighted'].sum()

    else:
        df['weighted'] = df.weight1500 * df.sumvar

        gdf = df.groupby(['wave', 'pais', extrawt, cols[len(cols)-1]])['weighted'].sum()

    return gdf.to_frame().rename(columns = {cols[len(cols)-1]:'sums'}).reset_index()


def total(df, cols, weighted, country=True):
    '''
    Calculate the totals
    '''

    if country:
        df['total_mig'] = df.groupby(['wave', 'pais'])[weighted].transform('sum')
        df['perc_mig'] = (df[weighted] / df.total_mig) * 100
    else:
        df['total_mig'] = df.groupby(['wave', 'prov'])[weighted].transform('sum')
        df['perc_mig'] = (df[weighted] / df.total_mig) * 100

    return df


def country_df(df, cols, country):
    '''
    Subsetting the dataframe to take just one country out
    '''

    return df.loc[df['pais'] == country][cols]
