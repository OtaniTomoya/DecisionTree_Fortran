# DecisionTree_Fortran

devcontainer.json, Makefileは、windows/Macで変わるので、gitに含んでいません。

./devcontainerにはDockerfileとdevcontainer.jsonが含まれています。

## Win
```./devcontainer/Dockerfile
FROM ubuntu:20.04

USER root

RUN mkdir -p /root/workspace
WORKDIR /root/workspace

ENV DEBIAN_FRONTEND=noninteractive

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

RUN wget --no-check-certificate https://apt.repos.intel.com/intel-gpg-keys/GPG-PUB-KEY-INTEL-SW-PRODUCTS.PUB && \
    apt-key add GPG-PUB-KEY-INTEL-SW-PRODUCTS.PUB && \
    echo "deb https://apt.repos.intel.com/oneapi all main" | tee /etc/apt/sources.list.d/oneAPI.list && \
    rm GPG-PUB-KEY-INTEL-SW-PRODUCTS.PUB

RUN apt-get update && \
    apt-get install -y --no-install-recommends \
    intel-basekit \
    intel-hpckit && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

RUN echo "source /opt/intel/oneapi/setvars.sh" >> ~/.bashrc
```
```.devcontainer/devcontainer.json
{
    "name": "実験環境",
    "build": { "dockerfile": "Dockerfile" },
    "runArgs": ["--init","--name","fortran_workbench"],
    "customizations": {
    "vscode": {
    "settings": {
    "diffEditor.ignoreTrimWhitespace": false,
    "explorer.openEditors.visible": 0,
    "files.insertFinalNewline": true,
    "files.trimTrailingWhitespace": true,
    "markdown-preview-enhanced.scrollSync": false
    // "notebook.lineNumbers": "on"
    },
    "extensions": [
    "oderwat.indent-rainbow",
    "fortran-lang.linter-gfortran",
    "github.copilot"
    ]
    }
    }
    }
```
```Makefile
FC = ifx
FFLAGS = -fast -diag-disable=10448 -qopenmp -qmkl=parallel -Imodules
#FC = gfortran
#FFLAGS = -O1 -Wall -g

TARGET = main
MOD_DIR = modules

MOD_SRC = $(MOD_DIR)/decision_tree_types.f90 $(MOD_DIR)/decision_tree_utils.f90 $(MOD_DIR)/decision_tree_split.f90 $(MOD_DIR)/decision_tree_metrics.f90 $(MOD_DIR)/decision_tree_build.f90 $(MOD_DIR)/decision_tree_io.f90
MAIN_SRC = main.f90

MOD_OBJ = $(MOD_SRC:.f90=.o)
MAIN_OBJ = $(MAIN_SRC:.f90=.o)

all: clean $(TARGET)

$(TARGET): $(MOD_OBJ) $(MAIN_OBJ)
	$(FC) $(FFLAGS) -o $@ $(MOD_OBJ) $(MAIN_OBJ)

$(MOD_DIR)/%.o: $(MOD_DIR)/%.f90
	$(FC) $(FFLAGS) -c $< -o $@

main.o: main.f90 $(MOD_OBJ)
	$(FC) $(FFLAGS) -c main.f90 -o main.o

clean:
	rm -f $(MOD_OBJ) $(MAIN_OBJ) $(TARGET)
	rm -f $(MOD_DIR)/*.mod

run: $(TARGET)
	./$(TARGET)

.PHONY: all clean run
```

## Mac
```.devcontainer/Dockerfile

```

```.devcontainer/devcontainer

```

```Makefile

```
