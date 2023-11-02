# Hospital level app

This Rshiny app  offers an interactive interface to zoom in hospital level model results interms of:-

- Patient outcomes
- Resource use &
- Costs

# How to update and republish the app

NOTE: Data used in this app comes from running the FLS calculator/model.

## Step 1: Update the datasets

Update the contents of the folder `datasets`. Go to the [FLS model](https://github.com/ndorms-botnar/OxfordUni.HEOR-FLSModel) project, navigate to the following path i.e for France, FLS adherence 100% that completed running on 23/03/2023:-

source path:- `report\Summary.Reports\datasets\France\FLS_Adherence_100\2023-03-26\shiny_app_data`

Copy the contents of the `shiny_app_data` and paste them on THIS project's `datasets` folder. Once this is done, the data for the app will be the most recent for France and the scenario of interest, adherence 100%.

## Step2: Run and re-publish the app

The app is published publicly in Oxford university's `https://trainingidn.shinyapps.io` server.

- To launch the app, open either of the following `ui.R`, `server.R` or `global.R`.
- A green `Run App`will appear at the top right corner, click on it to launch the app.
- Once the app launches, click on the blue button `Republish` located at the top-right corner of launched app window.
- Clikc `publish` to send the app to the Shiny server [wait, the publishing takes a few minutes to complete]. DO NOT close the RStudio project.
