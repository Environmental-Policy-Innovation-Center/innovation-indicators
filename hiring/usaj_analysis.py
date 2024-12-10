import pandas as pd

AGENCIES = ['Bureau Of Reclamation', 'National Park Service',
       'National Oceanic And Atmospheric Administration',
       'U.S. Army Corps Of Engineers',
       'Natural Resources Conservation Service', 'Forest Service',
       'Bureau Of Land Management', 'U.S. Fish And Wildlife Service',
       'Geological Survey', 'Environmental Protection Agency']

def clean_data(raw_data='usaj_summary.csv'):
    '''
    Read-in and perform generic high-level cleaning.

    Inputs: 
        raw_data (str): Name of the .csv file to be cleaned.

    Outputs:
        dta (pd.df): Cleaned pandas dataframe
    '''
    dta = pd.read_csv(raw_data, index_col=0)
    return dta


def num_openings(dta):
    '''
    Sums up the number of listings per agency. Estimates the number of total openings
    from categorical designations.

    Inputs:
        dta (pd.df): USAJobs scraper data 

    Outputs:
        job_counts (dict): Agencies mapped to estimates of their total listings.
    '''

    # Set up working data structures
    opening_types = dta.groupby(['subagency', 'openings']).count()
    opening_types = opening_types['id']
    opening_types = opening_types.reset_index(level=[0,1])

    job_counts = {}
    for agency in AGENCIES:
        agency_counts = opening_types[opening_types.subagency == agency]
        base_count = agency_counts.id.sum()
        agency_counts.fillna(1, inplace=True)

        # Minimum listing estimates per non-numeric designation
        agency_counts.loc[agency_counts.openings == 'Few', 'openings'] = 2
        agency_counts.loc[agency_counts.openings == 'Many', 'openings'] = 3
        agency_counts.openings = agency_counts.openings.astype('int64')
        agency_counts.id = agency_counts.id.astype('int64')

        # Calculate and save estimated counts
        agency_counts['mult'] = agency_counts['openings'] * agency_counts['id']
        est_count = agency_counts.mult.sum()
        job_counts[agency] = {
            'row count': int(base_count),
            'estimate': int(est_count),
            'estimated undercount': int(est_count - base_count)
            }

    return job_counts


def core_stats(dta):
    '''

    '''

    # Define and prepare working variables
    job_counts = num_openings(dta)
    workforce = pd.read_csv('workforce.csv')
    stats = {}

    workforce = workforce[workforce.year == 2024][['name', 'total_workforce']]
    workforce.name = workforce.name.replace('Fish And Wildlife Service', 'U.S. Fish And Wildlife Service')
    workforce.name = workforce.name.replace('Army Corps Of Engineers', 'U.S. Army Corps Of Engineers')
    workforce.set_index(keys='name',inplace=True)

    golden_ratio = 0
    for agency in AGENCIES:
        # Define output dictionary
        stats[agency] = {
            'num_listings': job_counts[agency]['row count'],
            'num_tech': 0,
            'expected_tech': 0,
            'tech_wf_ratio':0,
            'num_inno_processes': 0,
            'num_ai': 0,
            'num_stem': 0,
            'inno_max': 0,
            'inno_tendency': 0,
            'inno_undercount': 0
        }

        # Segment to agency of interest
        agency_dta = dta[dta.subagency == agency]
        agency_innovation = agency_dta[agency_dta.innovative_score > 5]

        # Calculate and record metrics
        stats[agency]['num_tech'] = int(agency_dta['it_specialist'].sum())
        stats[agency]['tech_wf_ratio'] = stats[agency]['num_tech'] / workforce.loc[agency]['total_workforce']
        stats[agency]['num_inno_processes'] = int(agency_dta['user_research'].sum() + agency_dta['agile_startup_ux'].sum())
        stats[agency]['num_ai'] = int(agency_dta['use_ai'].sum())
        stats[agency]['num_stem'] = int(agency_dta['stem_job'].sum())
        stats[agency]['inno_max'] = int(agency_dta['innovative_score'].max())
        stats[agency]['inno_tendency'] = len(agency_innovation)

        # Clean up openings data
        agency_innovation.fillna(1, inplace=True)
        agency_innovation.loc[agency_innovation.openings == 'Few', 'openings'] = 2
        agency_innovation.loc[agency_innovation.openings == 'Many', 'openings'] = 3
        agency_innovation.openings = agency_innovation.openings.astype('int64')
        agency_innovation.id = agency_innovation.id.astype('int64')
        
        # Estimate undercounts of innovative joabs
        inno_undercounts = agency_innovation[agency_innovation.openings > 1]
        inno_undercounts.openings = inno_undercounts.openings - 1
        stats[agency]['inno_undercount'] = inno_undercounts.openings.sum()

        # Record ratio of tech talent to total openings
        golden_ratio += stats[agency]['num_tech'] / stats[agency]['num_listings']

    mean_rat = golden_ratio / 10

    # Calculate expected tech listings per agency
    for agency in AGENCIES:
        stats[agency]['expected_tech'] = stats[agency]['num_listings'] * mean_rat

    stats_df = pd.DataFrame.from_dict(stats, orient='index')
    stats_df.to_csv('usajobs_stats.csv')

    return stats_df