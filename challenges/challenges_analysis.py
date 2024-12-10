import pandas as pd

def clean_data(category, raw_data='challenge_summary.csv'):
    '''
    Read-in and perform generic high-level cleaning.

    Inputs: 
        raw_data (str): Name of the .csv file to be cleaned.

    Outputs:
        dta (pd.df): Cleaned pandas dataframe
    '''

    dta = pd.read_csv(raw_data)
    dta = dta[(dta['start_year'] >= 2016) & (dta['start_year'] < 2025)].reset_index()

    if category == 'env':
        dta = dta[dta.env_flag == 'Environmental / Natural Resource Agencies']
    elif category == 'other':
        dta = dta[dta.env_flag == 'All Other Agencies']
    

    return dta


def agencies_by_year(datasets):
    '''
    Assign number of agencies issuing challenges in a year to the year.

    Inputs: 
        datasets (lst of pd.DF): One df with data for environmental agencies
                                 One df with data for other agencies

    Outputs:
        year_dicts (lst of dict): Challenges by year by agency        
    '''

    year_dicts = []    

    for dset in datasets:
        years = dset.start_year.unique()
        year_dict = {}
        for year in years:
            # Get the number of agencies issuing challenges for each year
            subset = dset[dset.start_year == year]
            num_agencies = len(subset.department.unique())
            year = str(year)
            year_dict[year] = num_agencies
        year_dicts.append(year_dict)
    
    return year_dicts


def normalize_by_year(datasets, year_dicts):
    '''
    Normalize yearly challenges by the number of agencies that offered at least one.

    Inputs:
        datasets (lst of pd.DF): One df with data for environmental agencies
                                 One df with data for other agencies
    
    Outputs:
        datasets (lst of pd.DF): One df with data for environmental agencies
                                 One df with data for other agencies
    '''

    i_norm = 0
    for dset in datasets:
        set_dict = year_dicts[i_norm]
        i_norm += 1
        for ind in dset.index:
            if type(ind) == tuple:
                the_year = str(ind[1])
            else:
                the_year = str(ind)
            num_agencies = set_dict[the_year]
            dset[ind] /= num_agencies

    return datasets


def challenges_by_type():
    '''
    Recategorize challenges into aggregated categories.

    Inputs:
        None

    Outputs:
        Direct writes on 2 csvs:
            enviro_agency_challenge_types_annual.csv
            other_agency_challenge_types_annual.csv
    '''

    env_dta = clean_data('env')
    other_dta = clean_data('other')

    datasets = [env_dta, other_dta]

    # Aggregate summary categories
    for series in [env_dta.primary_challenge_type, other_dta.primary_challenge_type]:
        series.replace(
            ['Software and apps','Technology demonstration and hardware', 'Analytics, visualizations, algorithms'],
            'Analytics, Tech, Software',
            inplace=True
        )

        series.replace(
            ['Ideas', 'Business plans', 'Nominations'],
            'Ideas, Plans, Nominations',
            inplace=True
        )

        series.replace(
            'Creative (multimedia & design)',
            'Creative',
            inplace=True
        )

    year_dicts = agencies_by_year(datasets)   

    # get yearly totals for each aggregated category.
    env_type_by_year = env_dta.groupby(['primary_challenge_type', 'start_year']).sum()['total_challenges']
    other_type_by_year = other_dta.groupby(['primary_challenge_type', 'start_year']).sum()['total_challenges']
    
    datasets = (env_type_by_year, other_type_by_year)

    env_type_by_year, other_type_by_year = normalize_by_year(datasets, year_dicts)

    env_type_by_year.to_csv('enviro_agency_challenge_types_annual.csv')
    other_type_by_year.to_csv('other_agency_challenge_types_annual.csv')



def challenge_by_year():
    '''
    Sum the total number of challenges for each year distinguished by type of agency.

    Inputs:
        None
    Outputs:
        Driectly write to CSV: challenges_by_year.csv
    '''
    # Prep data
    env_dta = clean_data('env')
    other_dta = clean_data('other')
    datasets = [env_dta, other_dta]
    year_dicts = agencies_by_year(datasets)   

    # Sum challenges by year
    env_ch_annual = env_dta.groupby('start_year').sum()['total_challenges']
    other_ch_annual = other_dta.groupby('start_year').sum()['total_challenges']

    # Normalize yearly datasets
    datasets = (env_ch_annual, other_ch_annual)
    env_ch_annual, other_ch_annual = normalize_by_year(datasets, year_dicts)

    env_label = pd.Series(['environmental']*15, name='tag')
    other_label = pd.Series(['other']*15, name='tag')

    # Set indices
    years = pd.Series(env_ch_annual.index)
    env_label.index = years
    other_label.index = years
    years.index = years
    
    # Combine datasets
    env_ch_trend = pd.concat([env_ch_annual, env_label, years], axis=1)
    other_ch_trend = pd.concat([other_ch_annual, other_label, years], axis=1)

    ch_dta = pd.concat([env_ch_trend, other_ch_trend], axis=0)
    ch_dta.index = range(ch_dta.shape[0])

    ch_dta.to_csv('challenges_by_year.csv')


def enviro_v_everybody():

    '''
    Run analyses on aggregated categories by bureau mission.

    inputs:
        None
    
    Outputs:
        Direct write 2 CSVs
            agency_challenges.csv
            tech_challenges.csv
    '''

    dta = clean_data(None)
    dta.primary_challenge_type.replace(
        ['Software and apps','Technology demonstration and hardware', 'Analytics, visualizations, algorithms'],
        'Analytics, Tech, Software',
        inplace=True
    )

    dta.primary_challenge_type.replace(
        ['Ideas', 'Business plans', 'Nominations'],
        'Ideas, Plans, Nominations',
        inplace=True
    )

    dta.primary_challenge_type.replace(
        'Creative (multimedia & design)',
        'Creative',
        inplace=True
    )

    # Get all challenges per agency
    agency_challenges = dta.groupby(['department', 'env_flag']).sum()['total_challenges']

    typed_challenges = dta.groupby(['department', 'env_flag', 'primary_challenge_type']).sum()['total_challenges'].unstack(fill_value=0).stack()
    typed_challenges.rename({'total_challenges':'tech_challenges'})

    agency_challenges.to_csv('agency_challenges.csv')
    typed_challenges.to_csv('tech_challenges.csv')