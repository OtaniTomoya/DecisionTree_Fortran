FROM ubuntu:20.04

USER root
RUN mkdir -p /root/workspace
WORKDIR /root/workspace
ENV DEBIAN_FRONTEND=noninteractive

# Update and install Fortran compiler and basic dependencies
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
    build-essential \
    gfortran \
    git\
    vim \
    tzdata && \
    apt-get -y clean && \
    rm -rf /var/lib/apt/lists/*

# Clean up
RUN apt-get clean && \
    rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*
