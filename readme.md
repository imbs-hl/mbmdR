# Running MB-MDR analyses on IMBS cluster

## Installation

On local machine:

```
library(devtools)
install_git("ssh://git@bioweb2.imbs.uk-sh.de:22222/gola/mbmdr.git")
```

On cluster:

```
library(devtools)
install_git("ssh://git@10.15.59.180:22222/gola/mbmdr.git")
```

### MB-MDR binaries
You can find MB-MDR binaries [here](http://www.statgen.ulg.ac.be/software_mbmdr.html).

## Configuration

You have to configure BatchJobs properly. See [here](https://bioweb2.imbs.uk-sh.de/gitlab/imbs/imbs-general/tree/master/templates/batchjobs) and [here](https://github.com/tudo-r/BatchJobs).

If you want to use one of the internal functions, use the function `configure` to set the MB-MDR parameters.
If you want to use the function `mbmdr` to run a complete analysis, use the parameters of the function itself.
