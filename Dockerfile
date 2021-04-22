FROM jupyter/scipy-notebook:latest

# Install .NET CLI dependencies

RUN pip install mxnet

ARG NB_USER=jovyan
ARG NB_UID=1000
ENV USER ${NB_USER}
ENV NB_UID ${NB_UID}
ENV HOME /home/${NB_USER}

WORKDIR ${HOME}

USER root
RUN apt-get update
RUN apt-get install -y curl

# Install .NET CLI dependencies
RUN apt-get update \
    && DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends \
        libc6 \
        libgcc1 \
        libgssapi-krb5-2 \
        libicu66 \
        libssl1.1 \
        libstdc++6 \
        zlib1g \
    && rm -rf /var/lib/apt/lists/*

# Install .NET Core SDK
ENV DOTNET_SDK_VERSION 5.0.102

RUN dotnet_sdk_version=5.0.102 \
    && curl -SL --output dotnet.tar.gz https://dotnetcli.azureedge.net/dotnet/Sdk/$dotnet_sdk_version/dotnet-sdk-$dotnet_sdk_version-linux-x64.tar.gz \
    && dotnet_sha512='0ce2d5365ca39808fb71baec4584d4ec786491c3735543dc93244604ea97e242377d0987cd8b1e529258dee68f203b5780559201e7ea6d84487d6d8d433329b3' \
    && echo "$dotnet_sha512 dotnet.tar.gz" | sha512sum -c - \
    && mkdir -p /usr/share/dotnet \
    && tar -ozxf dotnet.tar.gz -C /usr/share/dotnet \
    && rm dotnet.tar.gz \
    && ln -s /usr/share/dotnet/dotnet /usr/bin/dotnet \
    # Trigger first run experience by running arbitrary cmd
    && dotnet help

# Enable detection of running in a container
ENV DOTNET_RUNNING_IN_CONTAINER=true \
    # Enable correct mode for dotnet watch (only mode supported in a container)
    DOTNET_USE_POLLING_FILE_WATCHER=true \
    # Skip extraction of XML docs - generally not useful within an image/container - helps performance
    NUGET_XMLDOC_MODE=skip \
    # Opt out of telemetry until after we install jupyter when building the image, this prevents caching of machine id
    DOTNET_TRY_CLI_TELEMETRY_OPTOUT=true

# Trigger first run experience by running arbitrary cmd
RUN dotnet help

COPY ./MXNetSharp/ ${HOME}/MXNetSharp/

# Copy notebooks

COPY ./notebooks/ ${HOME}/notebooks/

# Copy package sources

COPY ./NuGet.config ${HOME}/nuget.config

RUN chown -R ${NB_UID} ${HOME}
USER ${USER}

# Install lastest build from master branch of Microsoft.DotNet.Interactive from myget
RUN dotnet tool install -g Microsoft.dotnet-interactive --add-source "https://pkgs.dev.azure.com/dnceng/public/_packaging/dotnet-tools/nuget/v3/index.json"

ENV PATH="${PATH}:${HOME}/.dotnet/tools"
RUN echo "$PATH"

# Install kernel specs
RUN dotnet interactive jupyter install

# Enable telemetry once we install jupyter for the image
ENV DOTNET_TRY_CLI_TELEMETRY_OPTOUT=false

RUN dotnet build MXNetSharp -c Release -f netstandard2.0
RUN cp ./MXNetSharp/bin/Release/netstandard2.0/MXNetSharp.dll ${HOME}/MXNetSharp.dll
ENV LD_LIBRARY_PATH "/opt/conda/lib/python3.7/site-packages/mxnet:$LD_LIBRARY_PATH"
#RUN cp /opt/conda/lib/python3.7/site-packages/mxnet/libmxnet.so ${HOME}/libmxnet.so

# Set root to notebooks
WORKDIR ${HOME}/notebooks/

