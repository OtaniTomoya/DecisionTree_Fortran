FROM ubuntu:20.04

USER root

# Create workspace directory
RUN mkdir -p /root/workspace
WORKDIR /root/workspace

# Set noninteractive mode for apt-get
ENV DEBIAN_FRONTEND=noninteractive

# Update and install Fortran compiler and basic dependencies
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
    build-essential \
    gfortran \
    git \
    vim \
    tzdata \
    wget \
    gnupg \
    ca-certificates

# Add Intel repository GPG key and list file
RUN wget --no-check-certificate https://apt.repos.intel.com/intel-gpg-keys/GPG-PUB-KEY-INTEL-SW-PRODUCTS.PUB && \
    apt-key add GPG-PUB-KEY-INTEL-SW-PRODUCTS.PUB && \
    echo "deb https://apt.repos.intel.com/oneapi all main" | tee /etc/apt/sources.list.d/oneAPI.list && \
    rm GPG-PUB-KEY-INTEL-SW-PRODUCTS.PUB

# Update and install Intel oneAPI packages
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
    intel-basekit \
    intel-hpckit && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

# Source the Intel oneAPI environment variables
RUN echo "source /opt/intel/oneapi/setvars.sh" >> ~/.bashrc
