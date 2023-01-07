FROM mcr.microsoft.com/devcontainers/base:jammy

RUN sudo apt update && sudo apt -y upgrade
RUN sudo apt-get install -y gfortran

RUN sudo apt-get install -y python3-pip
RUN pip install fortls
RUN pip install findent
RUN pip install fprettify
