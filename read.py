# LAPOP Migration Project
# 11-24-2019

import pandas as pd


def go(fname, countries, year, col, dvar):
    '''
    Go function for the program, spits out cleaned df
    '''

    dept_dict = read_dta(fname, col)
    findf = read(fname, countries, year, dept_dict, dvar)
    #Change the names of the countries
    findf['pais'] = findf['pais'].replace({2:'Guatemala', 3:'El Salvador', 4:'Honduras'})

    findf.to_csv('Data/clean.csv', index=False)

    return findf


def read_dta(fname, col):
    '''
    Finds metadata for each column, which includes department names
    and more for each column, this is important to determine what
    we're looking at.

    Input:
        - fname: filename of the .dta file
        - column: column that we want to find information for,
                    this case it's provinces ('prov_esp')
    Output:
        - dict: a dictionary that lets me later change codes to department names
                in the dataframe
    '''

    test = pd.read_stata('Data/All_LAPOP.dta', iterator = True)

    for i, x in test.value_labels().items():
        if i == col:
            prov_dict = x

    return prov_dict


def read(fname, countries, year, dict, dvar):
    '''
    Turn a .dta file into a pandas dataframe, and keep
    countries and years that we care about. It runs a
    whole bunch of helper functions to clean it up.

    Input:
        - fname: filename of .dta file
        - countries: tuple of countries I want to keep (stored as digits)
        - year: year I want to start collect data
        - dict: dictionary with department names
    Output:
        - dataframe for country/year pairs
    '''

    df = pd.read_stata(fname, convert_categoricals=False)

    # Selecting countries
    cdf = df.loc[(df['pais'] > countries[0]) & (df['pais'] <= countries[1])]
    # Selecting years
    predf = cdf[cdf['year'] >= year]

    predf['prov'] = predf['prov'].replace(dict)
    # Helper functions
    qdf = questions(predf)
    mdf = missing_random(qdf, perc=0.05)
    idf = impute_na(mdf)
    nadf = check_otherna(idf)
    pdf = drop_na_outcome(nadf, dvar)

    return pdf


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


def check_otherna(df):
    '''
    Given the nature of the survey data, some no responses could be
    coded as really high values. This function checks for that.

    Input:
        - df: dataframe
    Output:
        - prints columns with other values that could be no responses
        which can then be checked with codebook
    '''

    dropped_df = df.loc[(df > 50).any(1)]

    return dropped_df


def drop_na_outcome(df, dvar):
    '''
    Drop my no responses in the outcome variable, as they don't help

    Input:
        - df: dataframe
        - dvar: dependent variable
    Output:
        - fdf: dataframe with na values dropped in dependent variable
    '''

    s = df[str(dvar)].value_counts()

    print('# of no respones in dependent variable')
    for i, v in s.items():
        if i < 1:
            print(v)
        else:
            print('none')

    return df[df[str(dvar)] != 0]
