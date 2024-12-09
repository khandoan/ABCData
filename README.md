# "Data" project

This is the final project for the subject "Data Acquisition Analysis and Scientific Methods for Life Sciences" shorted as "Data" from the first course of the Master Plant Health at the Universitat Politècnica de València.

In this project the students chose a scientific article and redo all possible statistic analyses and graphics from it, trying different approaches to see if they can improve them. They will use the data included in the article and follow the Material and methods chapter as a guide.

## Group members

Group E: 

Arfa Ghias, Ana Belem Garcia, Cann Doan, Fatima Tahir.

## Chosen article

[Insect repellent and chemical agronomic treatments to reduce seed number in ‘Afourer’ mandarin: Effect on yield and fruit diameter](https://www.sciencedirect.com/science/article/pii/S0304423818308069)

The data is based on the research presented in Scientia Horticulturae (Volume 246, 27 February 2019, Pages 437-447), titled Insect repellent and chemical agronomic treatments to reduce seed number in ‘Afourer’ mandarin: Effect on yield and fruit diameter. The dataset includes measurements related to:
- Application of insect repellents and chemical treatments.
- Seed numbers in ‘Afourer’ mandarin.
- Yield and fruit diameter metrics.

## Proposal

This document serves as a preliminary research plan for reanalyzing the effects of agronomic treatments on ‘Afourer’ mandarin. The analysis will help clarify the efficacy and broader implications of the treatments studied.

Objective of Article: 

Focuses on studying methods to manage seed production in Afourer mandarins, a variety known for its desirable seedless characteristics, but it can develop seeds under certain conditions due to cross-pollination.

Objective of Our Project:

A. Replicate statistical analyses from the research article using available data to exercise R skills learned in class. 
B. Verify results presented in the article for seed-related data to confirm accuracy, consistency, and explore potential alternatives in processing and presenting data. 
C. To extract data independently from an open resource as opposed to a pre-defined datasets. 

The analyses you propose are right.

I would add some repeated measures analyses, as you have number of seeds and aphids at three different dates. 

### Main Hypotheses

If the seed data analysis is replicated, the results will align with the article's findings, while revealing opportunities for improved data management and visualisation. 

### Analyses Intended

A.  Descriptive Statistics
     1. Average and Standard Error: Summarising the central tendency and variability of numerical variables.
     2. Frequency Distribution and Density Curves: Assessing how data is spread across different values. Density curves offer a smoothed visualisation of data distribution.

B.  Inferential Statistics
    3. ANOVA (Analysis of Variance): To compare the mean values of variables across different treatments and blocks.
    4. Normality Testing (Shapiro-Wilk Test): To determine if data meets the normality assumption for parametric tests.
    5. Non-Parametric Tests (Kruskal-Wallis Rank Sum Test): To determine the average seed number per fruit achieved in each treatment.

C.  Correlation and Regression Analysis
    6. Linear Models: Analyse the effect of independent numerical variables on yield.

D.  Categorical Data Analysis
    7. Chi-Squared Test: The Chi-square test showed significant differences in the percentages of the seeded and seedless fruits among treatments

E.  Visualization Techniques
    8.  Box Plots: Compare seed count distributions across treatment groups. 
    9. Bar Charts with Error Bars: Show mean seed count per treatment with confidence intervals. 
    10. Scatterplots: Explore relationships between seed counts and continuous variables .
    11. Violin Plots: Visualise both distribution and density of seed counts for each treatment.

## Files description

## Protocol
