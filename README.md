# ADVANCE study: pain variability

Data and analysis scripts for an exploratory comparison of group-level changes in pain frequency, pain intensity, and pain sites to that of individual-level data over a 48-week period in a large cohort of people living with HIV enrolled in a randomized control trial for the management of HIV infection.

## Reference

Wadley AL, Venter WDF, Moorhouse M, Akpomiemie G, Serenata C, Hill A, Sokhela S, Mqamelo N, Kamerman PR. High individual pain variability in people living with HIV: A graphical analysis. _Eur J Pain_ **25**:160–170, 2021. DOI: [10.1002/ejp.1658](https://doi.org/10.1002/ejp.1658).

## Paper request

If you do not have access to the print version of the paper, please contact Peter Kamerman at [peter.kamerman@gmail.com](mailto:peter.kamerman@gmail.com).

## Abstract

_Background:_
People living with HIV (PLWH) frequently experience pain. Following calls to analyze individual-level data in addition to group-level data in pain studies, we compared individual and group-level changes in pain frequency, intensity and number of pain sites over 48-weeks in a large cohort of PLWH. This is the largest ever cohort study of pain in PLWH, and is the first to report pain at the level of the individual. 

_Methods:_
Participants included all participants with complete pain records from a randomized clinical trial (RCT) for the treatment of HIV (n = 787/1053). At weeks 0, 12, 24, 36 and 48 we assessed participants pain in the last week; presence of pain, and if present, the intensity and locations of the pain. We used standard averaging methods to describe data at the group-level, and unique graphical reporting methods to analyse data at the level of the individual. 

_Results:_
Group-level data demonstrated a trend for pain frequency to decline over time (19% week 0, 12% week 48). Worst pain intensity remained stable (median between 4/10 and 5/10), as did the number (median = 1)  and common sites of pain across the 48 weeks. In contrast, individual-level data demonstrated high intra-individual variability with regards to the presence of pain, and the intensity and location of the pain. 

_Conclusions:_
While our group-level data were similar to previous longitudinal studies, an apparent reduction in pain over 48 weeks, the individual-level data showed large variability within individuals in that same time frame.


## Reproducibility

For reproducibility we have built a docker image with the environment used to run the scripts:  
[kamermanpr/docker-pain-variability](https://hub.docker.com/repository/docker/kamermanpr/docker-pain-variability)

### Using Docker to run the scripts

You need to have docker installed on your computer. To do so, go to [docker.com](https://www.docker.com/community-edition#/download) and follow the instructions for downloading and installing docker for your operating system. Once Docker has been installed, follow the steps below, noting that Docker commands are entered in a terminal window (Linux and OSX/macOS) or command prompt window (Windows). 

#### Download the latest image

Enter: `docker pull kamermanpr/docker-pain-variability:v1.3`

#### Run the container

Enter: `docker run --name pain -d -p 8787:8787 -e USER=user -e PASSWORD=password kamermanpr/docker-pain-variability:v1.3`

#### Login to RStudio Server

- Open a web browser window and navigate to: `localhost:8787`

- Use the following login credentials: 
    - Username: _user_	
    - Password: _password_
    
#### Upload repository

- Go to the [ADVANCE-pain-variability](https://github.com/kamermanpr/ADVANCE-pain-variability.git) repository on GitHub and select download as a zip file.

- In the _Files_ tab on the lower right panel of RStudio, click **Upload**, located the zip file you downloaded and the click **OK**. The zip file will be uploaded and will automatically unzip, giving you access to all the content, including the analysis scripts, for the project.

- In the _Files_ tab, double-click the **ADVANCE-pain-variability.Rproj** file to ensure all the working directories are in order before running any of the scripts.

**Note:** The first time you _knit_ one of the _Rmd_ files, the generation of the PDF output will take some time as _tinyTex_ will install all the required _LaTeX_ packages for generating PDF documents. 

#### Shutting down

Once done, log out of RStudio Server and enter the following into a terminal to stop the docker container: `docker stop pain`. If you then want to remove the container, enter: `docker rm pain`. If you also want to remove the docker image you downloaded, enter: `docker rmi kamermanpr/docker-pain-variability`
