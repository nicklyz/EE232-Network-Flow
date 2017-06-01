## Installation

Please install anaconda and run `jupyter notebook`

To create a conda environment:
```
conda create -n py27 python=2.7 anaconda
source activate py27
conda install -c marufr python-igraph=0.7.1.post6
conda install -c pkgw py2cairo=1.10.0
```

If you are using Mac, make sure to run `brew install igraph` as well to install the C core.

