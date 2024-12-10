import pandas as pd

def clean_data(raw_data='github_languages_11_22_24.csv'):
    '''
    Read-in and perform generic high-level cleaning.

    Inputs: 
        raw_data (str): Name of the .csv file to be cleaned.

    Outputs:
        dta (pd.df): Cleaned pandas dataframe
    '''
    dta = pd.read_csv(raw_data, index_col=0)
    dta = dta.loc[:, (dta != 0).any(axis=0)]
    return dta


def lang_score(languages, agencies, oss_df):
    '''
    Score agencies on the languages they use based on the StackOverflow dev survey. 
    Source: https://survey.stackoverflow.co/2024/technology#most-popular-technologies-language-prof

    Inputs:
        languages (pd.df): Top languages used in OSS projects
        agencies (lst): The names of the environmental agencies of interest
        oss_df (pd.df): Overview of OSS data on GitHub

    Outputs:
        agency_lang_scores (dict): Maps agencies to their scores.

    '''
    so_rank = {
        'JavaScript':.646,
        'SQL':.541,
        'CSS':.529,
        'Python':.469,
        'TypeScript':.434,
        'Shell':.342,
        'Java':.3,
        'C#':.288,
        'C++':.203,
        'PHP':.187,
        'C':.169,
        'Go':.144,
        'PowerShell':.14,
        'Rust':.117,
        'Kotlin':.099,
        'Dart':.06,
        'Ruby':.058,
        'Lua':.053,
        'Swift':.049,
        'Visual Basic Net':.041,
        'Assembly':.04,
        'Groovy':.038,
        'Visual Basic 6.0':.031,
        'R':.031,
        'MATLAB':.03,
        'Scala':.029,
        'Objective-C':.023,
        'Perl':.023,
        'Elixir':.023,
        'Pascal':.018,
        'GDScript':.018,
        'Haskell':.016,
        'Clojure':.013,
        'NewLisp':.013,
        'MicroPython':.011,
        'Solidity':.011,
        'Erlang':.009,
        'Zig':.009,
        'F#':.009,
        'Fortran':.008,
        'Apex':.008,
        'Julia':.008,
        'Ada':.007,
        'Prolog':.006,
        'Cobol':.006,
        'OCaml':.005,
        'Crystal':.003,
        'Zephyr':.003,
        'Nim':.002
        }
    agency_lang_scores = {}
    for a in agencies:
        agency_lang_scores[a] = 0
        norm = oss_df.loc[a]['num_repos']
        languages.loc[a] = languages.loc[a].astype('float64')
        languages.loc[a] = (languages.loc[a] / norm).astype('float64')
        for lang in languages.loc[a].index:
            rank = so_rank.get(lang)
            if rank:
                agency_lang_scores[a] += float(languages.loc[a][lang] * rank)
        agency_lang_scores[a] = round(agency_lang_scores[a], 3)
    
    return agency_lang_scores

def babel():
    '''
    Run analyses on github OSS environmental data

    Inputs:
        None
    
    Outputs:
        top_grid (pd.df) Top 30 languages as percentages usage by repo
        normed_scores (dict) 0 to 1 normalized scores per agency by accessibility of their codebase
        output_dta (dict) Dictionary version of the csv that is written containing all language/OSS data per agency
    
    '''

    # Clean data and set up variables for use
    oss_lang = clean_data()
    oss_inno = pd.read_csv('github_innovation_11_26_24.csv', index_col=0)
    oss_lang.drop(['DOI-Departmental Offices','Farm Service Agency','Office of Natural Resources Revenue'], axis=0, inplace=True)
    not_lang = ['HTML', 'Makefile', 'Dockerfile', 'Jupyter Notebook',
                'Batchfile','M4','Procfile','Vue','Smarty', 'CMake', 
                'JavaScript', 'CSS','Shell', 'TeX', 'Inno Setup', 'SCSS']
    agencies = list(oss_inno.index)
    
    
    # Sort down to top XX languages used where XX = line_84_index - 10. Currently top 10.
    totals = oss_lang.sum().sort_values(ascending=False)
    top_langs = totals[:20]
    all_names = list(top_langs.index)
    lang_names = [x for x in all_names if x not in not_lang]
    top_grid = oss_lang[lang_names]

    # Score agencies based on how frequently they use popular languages
    agency_lang_scores = lang_score(top_grid, agencies, oss_inno)

    # Normalize agency language scores on 0-->1 scale
    norm_max = max(agency_lang_scores.values())
    norm_min = min(agency_lang_scores.values())
    normed_scores = {}
    for a in agencies:
        val = agency_lang_scores[a]
        normed_scores[a] = round((val - norm_min) / (norm_max - norm_min), 3)

    # Calculate and organize data structure for output.
    languages = top_grid.columns
    output_dta = {}
    for a in agencies:
        output_dta[a] = {}
        output_dta[a]['num_repos'] = round(float(oss_inno.loc[a]['num_repos']), 3)
        output_dta[a]['num_lang'] = len(top_grid.loc[a][top_grid.loc[a] > 0])
        output_dta[a]['num_contributors'] = round(float(oss_inno.loc[a]['num_contributors']),3)
        output_dta[a]['repo_per_million'] = round(float(oss_inno.loc[a]['repo_per_million']),3)
        output_dta[a]['contributor_per_million'] = round(float(oss_inno.loc[a]['num_contributors'] / oss_inno.loc[a]['total_funding']),3)
        output_dta[a]['nimbleness'] = normed_scores[a]
        
        for lang in languages:
            output_dta[a][f'{lang}_count'] = round(float(oss_lang.loc[a][lang]), 3)
            output_dta[a][f'{lang}_%'] = round(float(oss_lang.loc[a][lang] / output_dta[a]['num_repos']), 3)

    write_dta = pd.DataFrame.from_dict(output_dta, orient='index')
    write_dta.to_csv('language_numbers.csv')
    top_grid.to_csv('top_ten_lang.csv')

    return top_grid, normed_scores, output_dta

