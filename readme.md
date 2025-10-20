# From Research to Policy: A Case Study of the Use of Psychology Research in US Policy Documents
This folder contains the data and code necessary to reproduce the analysis in the paper "From Research to Policy: A Case Study of the Use of Psychology Research in US Policy Documents" by Chen-Wei Felix Yu, Hsiao-Yu Hu, Alexander C. Furnas, Jen-Ho Chang, Claudia M. Haase, William J. Brady, and Dashun Wang. 

# Structure of the folder
The folder is organized by the findings in the paper:
1. `1-trend`: Trend of research use by policy documents across 2000-2020.
2. `2-cited_vs_noncited`: Hurdle regression to examine the features of policy document cited vs. non-cited papers.
3. `3-topic_model`: Structural topic modeling of the abstracts of the psychology papers cited by policy documents.
4. `4-poli_cate`: Policy categories of the policy documents citing psychology research.
5. `5-network`: Network analysis of the policy documents citing psychology research and the bigram frequency of their titles.

Within each folder, you will find: 
- `data`: The input data files necessary for the analysis. If such folder does not exist, it means that the analysis uses the same set of data as another analysis. See the script file for the path to the data. We are only able to provide processed data due to data sharing restrictions. Please refer to the paper for details on how the data were collected, cleaned and processed. Any identifiable information connecting to specific papers and policy documents has been removed from the data. Please note that some data files are large in size due to the nature of the data, and therefore they were stored with GIT LFS. Please ensure that you have GIT LFS installed to properly access these files.
- `src`: The code scripts to perform the analysis.
- `out`: The output files generated from the analysis, such as figures and tables. If no output files are generated, the folder may not exist, and it means that the script will directly show the results in the console. I included the original output files for transparency and reproducibility. For some computationally intensive analyses, I also incorporated the output files in the script so that you can skip running the analysis if computational resources are a concern.
- If necessary, a `readme.md` file is included to provide additional information about the analysis.

All the scripts are written with the project directory as the working directory. Please adjust the file paths in the scripts if you run them from a different working directory.

# Requirements
## R (verison = 4.4.1)
|Package   |Version  |
|:---------|:--------|
|tidyverse |2.0.0    |
|ggpubr    |0.6.0    |
|tidytext  |0.4.2    |
|huxtable  |5.6.0    |
|rstatix   |0.7.2    |
|arrow     |20.0.0.2 |
|caret     |7.0.1    |
|dotenv    |1.0.3    |
|duckdb    |1.3.2    |
|pscl      |1.5.9    |
|stm       |1.3.7    |
|rollama   |0.2.2    |
|igraph    |2.1.4    |
|tidygraph |1.3.1    |
|ggraph    |2.2.1    |


## Python (verison = 3.10.13)
| ImportName   | DistributionName   | Version               |
|:-------------|:-------------------|:----------------------|
| graph_tool   | graph_tool         | 2.98 (commit , )      |
| numpy        | numpy              | 1.26.3                |
| os           | os                 | builtin (Python 3.10) |
| pandas       | pandas             | 2.2.0                 |
| pathlib      | pathlib            | builtin (Python 3.10) |
| pickle       | pickle             | builtin (Python 3.10) |