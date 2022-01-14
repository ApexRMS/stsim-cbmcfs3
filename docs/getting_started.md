---
layout: default
title: Getting started
---

# Getting started with **stsimcbmcfs3**

## Quickstart Tutorial

This quickstart tutorial will introduce you to the basics of working with stsimcbmcfs3. The steps include:
<br>
* Installing stsimcbmcfs3
* Creating a new stsimcbmcfs3 Library
* Configuring the stsimcbmcfs3 Library
* Viewing model inputs
* Running the model
* Analyzing the results

## **Step 1: Install stsimcbmcfs3**
**stsimcbmcfs3** is a Package within the <a href="https://syncrosim.com/download/" target="_blank">Syncrosim</a> simulation modeling framework; as such, running **stsimcbmcfs3** requires that the **SyncroSim** software be installed on your computer. Download and install the latest version **SyncroSim** <a href="https://syncrosim.com/download/" target="_blank">here</a>. **stsimcbmcfs3** also requires installation of the CBM-CFS3 <a href="https://www.nrcan.gc.ca/climate-change/impacts-adaptations/impacts-forests/carbon-accounting/carbon-budget-model/13107?utm_campaign=DFATD&utm_medium=twitter&utm_source=tweet44" target="_blank">Database</a> and R version <a href="https://www.r-project.org/" target="_blank">4.0.2</a> or later.
> **Note**: stsimcbmcfs3 also requires the following R packages:  rsyncrosim, tidyverse, and RODBC.

Once all required programs are installed, open **SyncroSim** and select **File -> Packages... -> Install...**, and select the **stsim**, **stsimsf**, and **stsimcbmcfs3** packages and click OK. Alternatively, download the <a href="https://github.com/ApexRMS/stsimcbmcfs3/releases" target="_blank">latest release</a> from GitHub. Open **SyncroSim** and select **File -> Packages... -> Install From File...**, then navigate to the downloaded package file with the extension *.ssimpkg*.
* **ST-Sim:** package for creating and running state-and-transition simulation models. See <a href="https://docs.stsim.net/" target="_blank">documentation</a>.
* **Stock-Flow Add-On for ST-Sim:** package for modelling stocks and flows within state-and-transition simulation models. See <a href="https://docs.stsim.net/" target="_blank">documentation</a>.

## **Step 2: Create a new stsimcbmcfs3 Library**
Having installed the **stsimcbmcfs3** Package, you are now ready to create your first SyncroSim Library. A Library is a file (with extension *.ssim*) that contains all of your model inputs and outputs. Note that the format of each Library is specific to the Package for which it was initially created. To create a new Library, choose **New Library...** from the **File** menu.
<br>
<img align="middle" style="padding: 3px" width="680" src="assets/images/screencap-1.PNG">
<br>
In this window:
<br>

## **Step 3: Configure library settings**

## **Step 4: Review the model inputs**

## **Step 5: Run the model**

## **Step 6: Analyze the results**
