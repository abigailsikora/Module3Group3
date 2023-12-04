# STAT 628 - Module3 - Group3
This repository contains the work of Group 3 for Module 3 in STAT628, focusing on analyzing Yelp business data. This project specifically focuses on analyzing businesses in Nashville, TN that contained either "bar" or "nighlife" in their category list on Yelp.
## Directory Structure
```
Module3Group3/
│
├── code/ 
│   ├── Sikora.Rmd #contains code for filtering data sets, creating visuals, and analysis
|   ├── data cleaning.py
|   ├── sentiment analysis.py
│   └── shinyApp.R #code for the shiny app
│
├── data/ 
│   ├── Trips_by_davidsoncounty.csv #data set from US Department of Transportation
|   ├── census_postalcodes.csv #data set from US Census Bureau
|   ├── filtered_reviews.json #Yelp reviews
|   ├── sentiment_by_postal_code.csv
|   ├── sentiment_scores_by_business.csv
│   └── nashville_bars_nightlife.json #business data from Yelp
|
├── presentation documents/ 
│   ├── Group3 Presentation1.pdf #exploratory analysis presentation slides
│   └── 
│
├── image/  # Various images and plots used in the analysis and presentation
│   ├── bartender.png
│   ├── bouncer.png
│   ├── cocktails.png
│   ├── happyhour.png
│   ├── numberofreviewsbystars.png
│   └── postalcode_stars.png
│
└── README.md  # This file
```
## Explanation of Files and Folders
-`code/`: Contains the main code files for the project.  
`shinyApp.R` is the code used to create the shiny app for this project  
`Sikora.Rmd` is code that was used for the data merging, exploratory analysis, and analysis parts. This code was written by Abigail. See comments on the code for more detailed information.  (Added by Abigail)
`data cleaning.py` contains code used for data cleaning and merging.  (Added by Shuangyu)
`sentiment analysis.py` contians code used for sentiment analysis. (Added by Shuangyu)


-`data/`: Stores the datasets used in this project.  
-`Trips_by_davidsoncounty.csv` is the dataset we filtered from the US Department of Transportation. This only contains trips in Davidson county.  
-`census_postalcodes.csv` is the dataset we filtered from the US Census Bureau. It contains the postal codes located in Nashville.  
-`nashville_bars_nightlife.json` is the business data set from Yelp.  
-`sentiment_by_postal_code.csv` is a data set used for our shiny app.  
-`sentiment_scores_by_business.csv` is a data set used for our shiny app.


-`presentation documents/`: Stores the document files used to explain this project.   
-`Group3 Presentation1.pdf` is the slides of the first presentation which is from the exploratory data analysis.   
-`Group3 Presentation2.pptx` is the slides for the second presentation which focuses on business advice.  
-`Group3Summary.pdf` is the 4-page executive-summary of our project.


-`image/`: Contains images and plots that might be used in the analysis, presentation, or within the Shiny app. Each subfolder or image corresponds to different aspects or sections of the analysis.  
-`bartender.png` image from EDA  
-`bouncer.png` image from EDA  
-`cocktails.png` image from EDA  
-`happyhour.png` image from executive summary  
-`numberofreviewsbystars.png` image from EDA  
-`postalcode_stars.png` image from executive summary  

-`README.md:` Provides an overview and explains the repository structure (this file).

## Shiny App
You can access the Shiny App developed based on our analysis directly via the following link: https://amsikora2.shinyapps.io/stat628/  

--- 

**Note**: Ensure you have all the necessary R packages installed and loaded to run the code files and Shiny app locally.

## Contributors
-Vaisnavi Borwankar  

-Abigail Sikora  

-Shuangyu Wang
