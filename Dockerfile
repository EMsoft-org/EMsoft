FROM marcdegraef/emsoft_sdk:buildx-latest

ARG TARGETARCH 
ARG DEBIAN_FRONTEND=noninteractive

# clone EMsoft and set up SDK Debug/Release
RUN cd ~/EMs \
 && git clone https://github.com/EMsoft-org/EMsoftData.git \
 && git clone https://github.com/EMsoft-org/EMsoft.git \
 && mkdir EMsoftBuild

RUN cd ~/EMs/EMsoftBuild/ && mkdir Debug Release && cd Debug \
 && cmake -DCMAKE_BUILD_TYPE=Debug -DEMsoft_SDK=/opt/EMsoft_SDK -DBUILD_SHARED_LIBS=OFF \
 ../../EMsoft -G Ninja \
 && ninja \
 && cd ../Release \
 && cmake -DCMAKE_BUILD_TYPE=Release -DEMsoft_SDK=/opt/EMsoft_SDK -DBUILD_SHARED_LIBS=OFF \
  ../../EMsoft -G Ninja \
 && ninja
 
# add release version to path
ENV PATH ~/EMs/EMsoftBuild/Release/Bin:$PATH
# add backup path of EMsoft
ENV EMSOFTPATHNAME ~/EMs/EMsoft

# install a new user
ARG user=EMuser
ARG uid=501
ARG gid=3000

# Add new user with our credentials
ENV USERNAME ${user}
RUN useradd -m $USERNAME && \
        echo "$USERNAME:$USERNAME" | chpasswd && \
        usermod --shell /bin/bash $USERNAME && \
        usermod  --uid ${uid} $USERNAME && \
        groupmod --gid ${gid} $USERNAME

USER ${user}

# run terminal for EMuser at /home/${user}
WORKDIR /home/${user}
CMD ["/bin/bash"]
