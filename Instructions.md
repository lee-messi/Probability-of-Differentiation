# Instructions for Reproducing Analyses
**Last Updated: 30 May 2024**

Any difficulties reproducing the analysis, please contact the corresponding author, [Messi H.J. Lee](mailto:hojunlee@wustl.edu). 


## Overview of the Repository Folders (in Alphabetical Order)

* Ablation (General Prompts)
     * Folder containing code for analysis and plots (**Analysis**)
     * Folder containing python code for data collection and collected data (**Data**)
     * Folder containing plots (**Plots**)

- Ablation (Group Labels)
     - Folder containing code for analysis and plots (**Analysis**)
     - Folder containing python code for data collection and collected data (**Data**)
     - Folder containing plots (**Plots**)

* Ablation (Individual Prompt)
     * Folder containing code for analysis and plots (**Analysis**)
     * Folder containing python code for data collection and collected data (**Data**)
     * Folder containing plots (**Plots**)

- Ablation (Model)
     - Folder containing code for analysis and plots (**Analysis**)
     - Folder containing python code for data collection and collected data (**Data**)
     - Folder containing plots (**Plots**)

* Main
     * Folder containing code for analysis and plots (**Analysis**)
     * Folder containing python code for data collection and collected data (**Data**)
     * Folder containing plots (**Plots**)

- Names
     - Ratings of names from Elder & Hayes (2023) (**nameratings.csv**)
     - Randomly sampled names used for data collection (**sampled_names.csv**)
     - Code used to randomly sample names (**select_names.R**)

* Pilot
     * Folder containing code for analysis and plots (**Analysis**)
     * Folder containing python code for data collection and collected data (**Data**)
     * Folder containing plots (**Plots**)
     
- Replication
     - .RData containing dataframes for four studies using the 18 situation cues
     - Code used to prepare the .RData files (**prepare.R**)
     - Code used to calculate coverage of the ablation studies (**replication.R**)


## Data Availability Statement

All code and data have been made available in this repository. 

## Before You Begin: Install the following packages

- R Packages (R version used: 4.4.0). If R is not installed, download [a version of R](https://cran.r-project.org/).
     - tidyverse (2.0.0): https://cran.r-project.org/web/packages/tidyverse/index.html
     - effsize (0.8.1): https://cran.r-project.org/web/packages/effsize/index.html
     - meta (7.0.0): https://cran.r-project.org/web/packages/meta/index.html
     - ggsci (3.0.3): https://cran.r-project.org/web/packages/ggsci/index.html
     - ggplot2 (3.5.1): https://cran.r-project.org/web/packages/ggplot2/index.html
     - Cairo (1.6-2): https://cran.r-project.org/web/packages/Cairo/index.html

* Python Packages (Python version used: 3.11.9). If Python 3 is not installed, download [a version of Python 3](https://www.python.org/downloads/).
     * openai: https://pypi.org/project/openai/
     * tqdm: https://pypi.org/project/tqdm/
     * numpy: https://pypi.org/project/numpy/
     * pandas: https://pypi.org/project/pandas/


## Workflow for Reproducing Analyses in the Pilot Study ("Pilot Study" Folder)

### **Name Selection ("Names" Folder)**
* Download *nameratings.tab* from [Elder & Hayes (2023)](https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/HEOSVW). This file can be downloaded as a .csv file. 
* Execute *select_names.R* inside the "Names" folder. 
     - The code randomly selects 15 first names for each intersectional group and saves them as *sampled_names.csv*. 

### **Data Collection ("Data" Subfolder)**
* Execute *pilot.py* inside the "Data" folder. 
     - Check that *sampled_names.csv* is inside the "Names" folder. 
     - Using the Instructions and Writing Prompts in Table A3 of the paper, collect data for 8 areas of human activities. Edit lines 36 (system message) and 37 (user message). 
     - Modify .csv file to match the area of human activity in line 57. 
     - Each run of *pilot.py* should generate a .csv file inside the "Data" subfolder. 

### **Compute Probability of Differentiation ("Analysis" Subfolder)**
* Execute *comparison.R*. 
     - The code pre-processes the collected data and calculates probability of differentiation for the racial/ethnic and gender groups and saves them as *pod.RData*. 

### **Visualize Probability of Differentation ("Analysis" Subfolder)**
* Execute *plot_pod.R*. 
     - The code uses *pod.RData* to plot probability of differentiation values and their 95% CIs for race/ethnicity and gender and saves them as .pdf files in the "Plots" subfolder. 

### **Meta-Analysis of Effect Sizes ("Analysis" Subfolder)**
* Execute *meta-analysis.R*.
     - The code conducts random-effect meta-analyses of effect sizes for each group comparison. 


## Workflow for Reproducing Analyses in the Main Study ("Main" Folder)

### **Data Collection ("Data" Subfolder)**
* Execute *main.py* inside the "Data" folder. 
     - Check that *sampled_names.csv* is inside the "Names" folder. 
     - Using the Instructions and Writing Prompts in Table A2 of the paper, collect data for 18 situation cues. Edit lines 36 (system message) and 37 (user message). 
     - Modify .csv file to match the situation cue in line 57. 
     - Each run of *main.py* should generate a .csv file inside the "Data" subfolder. 

### **Compute Probability of Differentiation ("Analysis" Subfolder)**
* Execute *comparison.R*. 
     - The code pre-processes the collected data and calculates probability of differentiation for the racial/ethnic and gender groups and saves them as *pod.RData*. 

### **Visualize Probability of Differentation ("Analysis" Subfolder)**
* Execute *plot_pod.R*. 
     - The code uses *pod.RData* to plot probability of differentiation values and their 95% CIs for race/ethnicity and gender and saves them as .pdf files in the "Plots" subfolder. 

### **Visualize Cohen's *d*s ("Analysis" Subfolder)**
* Execute *plot_cohens_d.R*. 
     - The code uses *pod.RData* to plot Cohen's *d*s and their 95% CIs for race/ethnicity and gender and saves them as .pdf files in the "Plots" subfolder. 

### **Meta-Analysis of Effect Sizes ("Analysis" Subfolder)**
* Execute *meta-analysis.R*.
     - The code conducts random-effect meta-analyses of effect sizes for each group comparison. 


## Workflow for Reproducing Analyses in the Model Study ("Ablation (Model)" Folder)

### **Data Collection ("Data" Subfolder)**
* Execute *ablation_model.py* inside the "Data" folder. 
     - Check that *sampled_names.csv* is inside the "Names" folder. 
     - Using the Instructions and Writing Prompts in Table A2 of the paper, collect data for 18 situation cues. Edit lines 29 (system message) and 30 (user message). 
     - Modify .csv file to match the situation cue in line 51. 
     - Each run of *ablation_model.py* should generate a .csv file inside the "Data" subfolder. 

### **Compute Probability of Differentiation ("Analysis" Subfolder)**
* Execute *comparison.R*. 
     - The code pre-processes the collected data and calculates probability of differentiation for the racial/ethnic and gender groups and saves them as *pod.RData*. 

### **Visualize Probability of Differentation ("Analysis" Subfolder)**
* Execute *plot_pod.R*. 
     - The code uses *pod.RData* to plot probability of differentiation values and their 95% CIs for race/ethnicity and gender and saves them as .pdf files in the "Plots" subfolder. 

### **Optional: Visualize Cohen's *d*s ("Analysis" Subfolder)**
* Execute *plot_cohens_d.R*. 
     - The code uses *pod.RData* to plot Cohen's *d*s and their 95% CIs for race/ethnicity and gender and saves them as .pdf files in the "Plots" subfolder. 

### **Meta-Analysis of Effect Sizes ("Analysis" Subfolder)**
* Execute *meta-analysis.R*.
     - The code conducts random-effect meta-analyses of effect sizes for each group comparison. 


## Workflow for Reproducing Analyses in the Group Labels Study ("Ablation (Group Labels)" Folder)

### **Data Collection ("Data" Subfolder)**
* Execute *ablation_label.py* inside the "Data" folder. 
     - Using the Instructions and Writing Prompts in Table A2 of the paper, collect data for 18 situation cues. Edit lines 31 (system message) and 32 (user message). 
     - Modify .csv file to match the situation cue in line 52. 
     - Each run of *ablation_label.py* should generate a .csv file inside the "Data" subfolder. 

### **Compute Probability of Differentiation ("Analysis" Subfolder)**
* Execute *comparison.R*. 
     - The code pre-processes the collected data and calculates probability of differentiation for the racial/ethnic and gender groups and saves them as *pod.RData*. 

### **Visualize Probability of Differentation ("Analysis" Subfolder)**
* Execute *plot_pod.R*. 
     - The code uses *pod.RData* to plot probability of differentiation values and their 95% CIs for race/ethnicity and gender and saves them as .pdf files in the "Plots" subfolder. 

### **Optional: Visualize Cohen's *d*s ("Analysis" Subfolder)**
* Execute *plot_cohens_d.R*. 
     - The code uses *pod.RData* to plot Cohen's *d*s and their 95% CIs for race/ethnicity and gender and saves them as .pdf files in the "Plots" subfolder. 

### **Meta-Analysis of Effect Sizes ("Analysis" Subfolder)**
* Execute *meta-analysis.R*.
     - The code conducts random-effect meta-analyses of effect sizes for each group comparison. 


## Workflow for Reproducing Analyses in the General Prompts Study ("Ablation (General Prompts)" Folder)

### **Data Collection ("Data" Subfolder)**
* Execute *ablation_prompt.py* inside the "Data" folder. 
     - Check that *sampled_names.csv* is inside the "Names" folder. 
     - Using the Writing Prompts in Table A11 of the paper, collect data for 18 situation cues. Edit line 30 (user message). 
     - Modify .csv file to match the situation cue in line 51. 
     - Each run of *ablation_prompt.py* should generate a .csv file inside the "Data" subfolder. 

### **Compute Probability of Differentiation ("Analysis" Subfolder)**
* Execute *comparison.R*. 
     - The code pre-processes the collected data and calculates probability of differentiation for the racial/ethnic and gender groups and saves them as *pod.RData*. 

### **Visualize Probability of Differentation ("Analysis" Subfolder)**
* Execute *plot_pod.R*. 
     - The code uses *pod.RData* to plot probability of differentiation values and their 95% CIs for race/ethnicity and gender and saves them as .pdf files in the "Plots" subfolder. 

### **Optional: Visualize Cohen's *d*s ("Analysis" Subfolder)**
* Execute *plot_cohens_d.R*. 
     - The code uses *pod.RData* to plot Cohen's *d*s and their 95% CIs for race/ethnicity and gender and saves them as .pdf files in the "Plots" subfolder. 

### **Meta-Analysis of Effect Sizes ("Analysis" Subfolder)**
* Execute *meta-analysis.R*.
     - The code conducts random-effect meta-analyses of effect sizes for each group comparison. 


## Workflow for Reproducing Analyses in the Individual Prompt Study ("Ablation (Individual Prompt)" Folder)

### **Data Collection ("Data" Subfolder)**
* Execute *individual.py* inside the "Data" folder. 
     - Check that *sampled_names.csv* is inside the "Names" folder. 
     - Using the Instructions and Writing Prompts in Table A2 of the paper, collect data for 18 situation cues. Edit lines 29 (system message) and 30 (user message). 
     - Modify .csv file to match the situation cue in line 51. 
     - Each run of *individual.py* should generate a .csv file inside the "Data" subfolder. 

### **Compute Probability of Differentiation ("Analysis" Subfolder)**
* Execute *comparison.R*. 
     - The code pre-processes the collected data and calculates probability of differentiation for the racial/ethnic and gender groups and saves them as *pod.RData*. 

### **Visualize Probability of Differentation ("Analysis" Subfolder)**
* Execute *plot_pod.R*. 
     - The code uses *pod.RData* to plot probability of differentiation values and their 95% CIs for race/ethnicity and gender and saves them as .pdf files in the "Plots" subfolder. 

### **Optional: Visualize Cohen's *d*s ("Analysis" Subfolder)**
* Execute *plot_cohens_d.R*. 
     - The code uses *pod.RData* to plot Cohen's *d*s and their 95% CIs for race/ethnicity and gender and saves them as .pdf files in the "Plots" subfolder. 

### **Meta-Analysis of Effect Sizes ("Analysis" Subfolder)**
* Execute *meta-analysis.R*.
     - The code conducts random-effect meta-analyses of effect sizes for each group comparison. 


## Replication Analysis ("Replication" Folder)
* Execute *prepare.R*. 
     - The code loads relevant dataframes from the *pod.RData* files for the Main, Model, Group Labels, and Individual Prompt Studies, removes all unnecessary files, and saves them as a new .RData file inside the "Replication" folder. 

* Execute *replication.R*.
     - Check that *main.RData*, *model.RData*, *label.RData*, and *individual_prompt.RData* are inside the "Replication" folder. 
     - The code calculates the coverage of the ablation studies and conducts a goodness-of-fit test to assess replicability of these studies. 

