### Open Access Impact Evaluation Design Tool
#### Using Simulation-Based Estimation
This sample-size and power calculator provides researchers and practitioners with a simple, web-based interface for a range of experimental study designs. The use of drop-down menus and online tutorial make this tool easy to use for those with a range of technical backgrounds. This tool allows for a range of study design complexity--from simple randomized controlled trials (RCTs) to cluster-randomized trials with binary outcomes and longitudinal follow-up--by being grounded in use of simulation methods. The open-source nature of this project allows for contributions from researchers across the natural and social sciences--economics, public health, education, etc--to meet their field's particular needs.

#### Advantages of this tool

* **Allows for multiple types of outcome data** - The simulation-based methods used by this tool can accommodate both continuous, binary, and count outcome data.
* **Allows for complex designs** - Unlike other calculation tools which use analytical approximations, the use of simulation-based calculations allows flexibility in considering more complex designs, including longitudinal follow-up, and cluster randomization.
* **Both power- and sample-size-based** - Unlike other calculators, this tool will allow users to obtain sample size requirements--both individual and cluster-based--based on a desired level of power, or obtain the level of power for a specified sampling scheme.
* **Web-based interface** - This makes the tool accessible, regardless of what computing platform is being used. It also allows developers to share updates without the need for users to download and install updates themselves.
* **Open source** - This allows many diverse contributors to enhance this single project (under the governance of the core development team). 

#### Getting started

Once you have the necessary packages installed, you can run the
application locally with:

    ~/sim-based-power-calculator$ R -e "shiny::runApp('.')"
