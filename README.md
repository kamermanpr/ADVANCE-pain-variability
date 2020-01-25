# ADVANCE study: pain variability

Data and analysis scripts for an exploratory comparison of group-level changes in pain frequency, pain intensity, and pain sites to that of individual-level data over a 48-week period in a large cohort of people living with HIV enrolled in a randomized control trial for the management of HIV infection.

## Reference

## Pre-print

## Abstract

## Reproducibility

For reproducibility we have built a Docker image with the environment used to run the scripts:  
[kamermanpr/docker-pain-variability](https://hub.docker.com/repository/docker/kamermanpr/docker-pain-variability)

### Using Docker to run the scripts

You need to have Docker installed on your computer. To do so, go to [docker.com](https://www.docker.com/community-edition#/download) and follow the instructions for downloading and installing Docker for your operating system. Once Docker has been installed, follow the steps below, noting that Docker commands are entered in a terminal window (Linux and OSX/macOS) or command prompt window (Windows). 

#### Download the latest image

Enter: `docker pull kamermanpr/docker-pain-variability`

#### Run the container

Enter: `docker run --name pain -d -p 8787:8787 -e USER=user -e PASSWORD=password kamermanpr/docker-pain-variability`

#### Login to RStudio Server

- Open a web browser window and navigate to: `localhost:8787`

- Use the following login credentials: 
    - Username: _user_	
    - Password: _password_
    
#### Upload/clone repository

_TO BE ADDED_

#### Shutting down

Once done, log out of RStudio Server and enter the following into a terminal to stop the Docker container: `docker stop pain`. If you then want to remove the container, enter: `docker rm pain`. If you also want to remove the Docker image you downloaded, enter: `docker rmi kamermanpr/docker-pain-variability`
