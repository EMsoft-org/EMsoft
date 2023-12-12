FROM ubuntu:jammy

ARG TARGETARCH 
ARG DEBIAN_FRONTEND=noninteractive

RUN if [ "$TARGETARCH" = "arm64" ]; then \
 apt-get update && apt-get -y upgrade \
 && apt-get install -y \
 git \
 vim \
 wget \
 cmake \
 gcc-aarch64-linux-gnu \
 gfortran \
 ninja-build \
 build-essential \
 libopenblas-dev \
 apt-utils \
 opencl-headers \
 ocl-icd-opencl-dev \
 ocl-icd-libopencl1 \
 clinfo ; \
 elif [ "$TARGETARCH" = "amd64" ]; then \
 apt-get update && apt-get -y upgrade \
 && apt-get install -y \
 git \
 vim \
 wget \
 cmake \
 gcc \
 gfortran \
 ninja-build \
 build-essential \
 libopenblas-dev \
 apt-utils \
 opencl-headers \
 ocl-icd-opencl-dev \
 ocl-icd-libopencl1 \
 libpocl-dev \ 
 intel-opencl-icd \
 clinfo \
; fi

# intel-opencl-icd makes iGPUs work from Intel (tested on i5-8400)
# libpocl-dev is for CPU on both Intel and AMD

# set some environmental variables for the Nvidia container
ENV NVIDIA_VISIBLE_DEVICES all
ENV NVIDIA_DRIVER_CAPABILITIES compute,utility
# set up OpenCL for Nvidia
RUN mkdir -p /etc/OpenCL/vendors && \
    echo "libnvidia-opencl.so.1" > /etc/OpenCL/vendors/nvidia.icd

# clone EMsoft and set up SDK Debug/Release
RUN mkdir ~/EMs \
 && cd ~/EMs \
 && git clone https://github.com/EMsoft-org/EMsoftSuperbuild.git \
 && git clone https://github.com/EMsoft-org/EMsoftData.git \
 && git clone https://github.com/EMsoft-org/EMsoft.git \
 && mkdir EMsoftBuild && mkdir GenEMsoftData && cd EMsoftSuperbuild && mkdir Debug Release

# EMsoftSuperbuild
RUN cd ~/EMs/EMsoftSuperbuild/Debug/ \
 && cmake -DEMsoft_SDK=/opt/EMsoft_SDK -DCMAKE_BUILD_TYPE=Debug ../ -G Ninja && ninja \
 && cd ../Release \
 && cmake -DEMsoft_SDK=/opt/EMsoft_SDK -DCMAKE_BUILD_TYPE=Release ../ -G Ninja && ninja

# EMsoftBuild ... must use dynamic "shared" libraries for f90wrap later on

RUN if [ "$TARGETARCH" = "arm64" ]; then \
 cd ~/EMs/EMsoftBuild/ && mkdir Debug Release && cd Debug \
 && cmake -DCMAKE_BUILD_TYPE=Debug -DEMsoft_SDK=/opt/EMsoft_SDK -DBUILD_SHARED_LIBS=OFF \
 ../../EMsoft -G Ninja \
 && ninja \
 && cd ../Release \
 && cmake -DCMAKE_BUILD_TYPE=Release -DEMsoft_SDK=/opt/EMsoft_SDK -DBUILD_SHARED_LIBS=OFF \
  ../../EMsoft -G Ninja \
 && ninja \
 && cd ~/EMs/GenEMsoftData && mkdir EMXtal \
 && cd ../../ && mkdir .config && cd .config && mkdir EMsoft && cd EMsoft && mkdir tmp && cd ../../ ; \
 elif [ "$TARGETARCH" = "amd64" ]; then \
 cd ~/EMs/EMsoftBuild/ && mkdir Debug Release && cd Debug \
 && cmake -DCMAKE_BUILD_TYPE=Debug -DEMsoft_SDK=/opt/EMsoft_SDK -DBUILD_SHARED_LIBS=OFF \
 ../../EMsoft -G Ninja \
 && ninja \
 && cd ../Release \
 && cmake -DCMAKE_BUILD_TYPE=Release -DEMsoft_SDK=/opt/EMsoft_SDK -DBUILD_SHARED_LIBS=OFF \
  ../../EMsoft -G Ninja \
 && ninja \
 && cd ~/EMs/GenEMsoftData && mkdir EMXtal \
 && cd ../../ && mkdir .config && cd .config && mkdir EMsoft && cd EMsoft && mkdir tmp && cd ../../ \
 ; fi

# add release version to path
ENV PATH ~/EMs/EMsoftBuild/Release/Bin:$PATH
# add backup path of EMsoft
ENV EMSOFTPATHNAME ~/EMs/EMsoft

# run terminal for user at /home/
WORKDIR /home/
CMD ["/bin/bash"]
