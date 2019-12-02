# LAPOP Migration Project
# 11-24-2019

import pandas as pd

def read(fname, countries, year):
    '''
    Turn a .dta file into a pandas dataframe, and keep
    countries and years that we care about

    Input:
        - fname: filename of .dta file
        - countries: tuple of countries I want to keep (stored as digits)
        - year: year I want to start collect data
    Output:
        - dataframe for country/year pairs
    '''

    df = pd.read_stata(fname, convert_categoricals=False)

    # Selecting countries
    cdf = df.loc[(df['pais'] > countries[0]) & (df['pais'] <= countries[1])]
    # Selecting years
    predf = cdf[cdf['year'] >= year]

    return predf

def questions(dframe):
    '''
    Find which questions are answered for all years by
    finding columns per year that have all null values,
    and removing them.

    Input:
        - dframe: dataframe
    Output:
        - df_years: dataframe with columns that span all years
    '''

    # find years in the dataframe
    years = list(dframe['year'].unique())
    # create a set of columns per year that are all nulls
    to_remove = set()

    for i in years:
        cols = dframe[dframe['year'] == i].isnull().all()
        for x, y in cols.items():
            if y:
                to_remove.add(x)

    # remove columns with all nulls for any given year from the set
    df_years = dframe.drop(list(to_remove), axis=1)

    return df_years

def missing_random(df, perc = 0.05):
    '''
    From a dataframe, remove columns with high percentages of
    null values, working under the assumption that lower perc
    of null values indicate more randomness.

    Input:
        - df: dataframe
        - perc: percent of nulls that are acceptable, 0.05

    Output:
        - null_df: dataframe keeping only columns with values
                    missing at random.
    '''

    perclist = []
    # find sum of nulls, divide by total rows to get percent
    for i, j in df.isnull().sum().items():
        if j / df.shape[0] < perc:
        # Getting rid of columns that have all empty string values, of which there are some
            if df[i].unique()[0] != '':
                perclist.append(i)

    null_df = df[perclist]

    return null_df

def impute_na(df):
    '''
    Imputes missing variables, since they're random in this survey,
    to zero

    Input:
        - df: dataframe
    Ouput:
        - impute_df: dataframe where nulls = 0
    '''

    return df.fillna(0)
