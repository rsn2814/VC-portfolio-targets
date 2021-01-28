source('~/options_packages.R')
# Target Investors URL 
target_company_url = 'https://foundersfund.com/'

#we want to find out what companies are they invested in first

#This function takes in a url and spits out a base url 
drop_the_bass <- function(url) {
  base_url = domain(url)
  base_url = sub("www.", "", base_url)
  return (base_url)
}

target_base_url = drop_the_bass(target_company_url)

#--------------This pulls from org_investor_summary table and gets all investments--------

#pull from redshift
all_investors <- rs$query('select * from cbase.org_investor_summary')

#add a column with base urls
all_investors['base_url'] <- lapply(all_investors['investor_homepage'], drop_the_bass)
  
#THIS IS FIRST SOURCE OF INVESTMENTS
inv_target1 <- all_investors %>%
  arrange(desc(insert_date)) %>% 
  filter(base_url == target_base_url) %>%
  distinct(org_permalink, .keep_all = T) %>%
  select(org_name, org_permalink, insert_date)

#--------------This pulls from funding_round_details table and gets all investments--------

funding_round_details <- rs$query('select * from cbase.funding_round_details')
funding_round_details['base_url'] <- lapply(funding_round_details['investor_homepage'], drop_the_bass)

inv_target2 <- funding_round_details %>%
  filter(base_url == target_base_url) %>%
  arrange(desc(insert_date)) %>% 
  distinct(org_permalink, .keep_all = T) %>%
  select(-investor_name, -funding_round_permalink, -funding_round_api_path) %>%
  select(org_name, org_permalink,funding_round_announced_on, funding_round_amount_usd, funding_round_series, 
         funding_round_type, insert_date) 


#--------------This joins all portfolio companies together--------

all_investments <- full_join(inv_target1,inv_target2,"org_permalink" )

list_of_port_perma <- all_investments[['org_permalink']]

full_data <- rs$query('select * from cbase.org_full_summary')

all_investments_data <- full_data %>%
  filter(org_permalink %in% list_of_port_perma )

org_summary <- rs$query('select * from cbase.org_full_summary') %>%
  filter(org_name %in% orgs_target_investor[['org_name']] | org_permalink %in% orgs_target_investor[['org_permalink']])


#--------------INPUTS: These are filters for portfolio companies --------

#Filter based on amount raised
amount_raise_cutoff <- 15000000

#Filter based on round series
fund_round_series_filter <- c('angel', 'seed', 'series_a', 'series_b', 'series_unknown', 'undisclosed')

#filter based on round type
round_type_filter <- c('seed','early_stage_venture')

#-------------- Output: These are filters for portfolio companies --------
cbase_cats <- read_sheet(ss= 'https://docs.google.com/spreadsheets/d/1CCD-4VngSjOoOdeV3kT_L6tvs9Z1aviYybuI3Q544MY/edit?usp=sharing')

cbase_cats_list <- cbase_cats[['Category']]

all_investments_data_filter <- all_investments_data %>%
  filter(org_country_code == 'United States' | is.na(org_country_code)) %>%
  filter(org_total_funding <= amount_raise_cutoff | is.na(org_total_funding) ) %>%
  filter(org_status == 'operating' | is.na(org_status)) %>%
  filter(org_valuation <= 40000000 | is.na(org_valuation)) %>%
  filter(org_funding_stage %in% round_type_filter | is.na(org_funding_stage)) %>%
  mutate(catg_count = str_count(categories, paste(cbase_cats_list, collapse="|"))) %>%
  filter(catg_count>0)
