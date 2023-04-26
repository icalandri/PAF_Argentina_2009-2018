# Population Attributable fraction for dementia risk in Argentina
## Sex and Socioeconomic Disparities in Dementia Risk in Argentina: A Decade-Long Perspective

This repository contains the code used for the elaboration of the paper of the same name.

It contains the code used to clean, code, analyze and elaborate the tables and graphs as well as the data used.



## Flow
For the analysis please follow the following code execution order:

:closed_book: **Total PAF calculation**:
1. 01_Data_tidy_2018
2. 02_comunalities_2018
3. 03_B_factors_outside_dataset_2018
4. 04_A_PAF_calculation_2018

:orange_book: **Calculation of change in time of Total PAF**
1. 01_Data_tidy_2018
2. 02_comunalities_2018
3. 04_A_PAF_calculation_2018_ w_o_others
4. 01_Data_tidy_2009
5. 02_comunalities_2009
7. 03_A_PAF_calculation_2009
4. 01_Data_tidy_2013
5. 02_comunalities_2013
7. 03_A_PAF_calculation_2013
8. 05_year_evolution_paf

:books: **Calculation of sex differences**
1. 01_Data_tidy_2018
2. sex_pipeline
3. sex_plot_annex
4. sex_test_and_table_annex

:books: **Calculation of sex differences in time**
1. 01_Data_tidy_2018
2. 01_Data_tidy_2009
3. 01_Data_tidy_2013
4. sex_pipeline_2009
5. sex_pipeline_2013
6. sex_plot_evolution

:notebook: **Calculation of household income differences**
1. 01_Data_tidy_2018
2. quintile_pipeline
3. quintile_plot_annex
4. quintile_test_and_table_annex

:books: **Calculation of  household income differences in time**
1. 01_Data_tidy_2018
2. 01_Data_tidy_2009
3. 01_Data_tidy_2013
4. quintile_pipeline_2009
5. quintile_pipeline_2013
6. quintile_plot_evolution
