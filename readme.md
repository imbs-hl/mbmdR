# Running MB-MDR analyses on IMBS cluster

## Installation

On local machine:

```
library(devtools)
install_git(ssh://git@bioweb2.imbs.uk-sh.de:22222/gola/mbmdr.git)
```

On cluster:

```
library(devtools)
install_git(ssh://git@10.15.59.180:22222/gola/mbmdr.git)
```

## Configuration

You have to configure BatchJobs properly. See [here](https://bioweb2.imbs.uk-sh.de/gitlab/imbs/imbs-general/tree/master/templates/batchjobs) and [here](https://github.com/tudo-r/BatchJobs).
