This file contains scripts and data related to summarizing the 'probable cause' column in the NTSB_flights dataset using Hugging Face Transformers and converting it into a categorical variable.

Files Included
probable_cause_summary.ipynb: Jupyter Notebook containing code for summarizing the 'probable cause' column using Hugging Face Transformers.
probable_cause_datawrangling.ipynb: Jupyter Notebook with code for data wrangling, including the transformation of the 'probable cause' column and creating a new CSV file.
new_flights_PC.csv: CSV file generated after data wrangling, containing an additional column with summarized 'probable cause' data.
README.md: This file, providing an overview and instructions for the file.
Usage

Ds to summarize the 'ProbableCause' column and adds the summary as an extra column named "Probable_Cause" to the dataset.

Requirements
Python 
Jupyter Notebook
Transformers Library from Hugging Face
Install necessary libraries using:

bash
Copy code

pip install transformers