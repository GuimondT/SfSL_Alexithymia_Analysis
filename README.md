# SfSL_Alexithymia_Analysis
This folder contains the code used for an analysis of SfSL/PISA intervention published in the journal Crisis.

The research is entitled, "Alexithymia in People with Recurrent Suicide Attempts: A niche area for targeted treatment" and was authored by Yvonne Bergmans, Tim Guimond, Clare Lambert, Shane McInerney and Kristen O'Brien. It is currently accepted for publication and this README file will be updated with issue and volume information once a publication date is determined.

The analysis is broken down into 4 steps:

1) In the first step (completed in R) the data is read in, variables are constructed from raw questionnaire response data (including min and max values on scales with missing items) in preparation for imputation. This step also includes calculation of the cronbach's alpha for the TAS score.
2) Step 2 conducts the imputation with interval imputation for scaled data, and is conducted in SAS using IVEware.
3) Step 3 reads in the results of the imputation, determines which data pairs will be retained for analysis, and creates datasets to be used for demographic tables. This step is completed in R.
4)  Step 4 conducts the analyses described in the manuscript and were conducted in SAS using the PROC MIANALYZE procedure to account for the multiply imputed data.

Two additional files are present. measuresumspan.r contains a function that calculates a total score from raw data (and a list of reverse coded items) along with the min and max possible scores when missing data is present. This was written as a separate function to facilitate its use in other settings. SfSL_Figure1.r calculates the clinically relevant change status, and prepares the plot as seen in the manuscript of the first imputed dataset (as well as a version with one plot for each imputed data set).

Questions about the code can be directed to Tim Guimond at tim(dot)guimond(at)utoronto(dot)ca.
