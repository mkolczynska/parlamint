# ParlaMint
Playing with parliamentary corpora with data from the [Linguistically annotated multilingual comparable corpora of parliamentary debates ParlaMint.ana 2.0](https://www.clarin.si/repository/xmlui/handle/11356/1405)


## Getting the data

The linguistically annotated data can be downloaded [here](https://www.clarin.si/repository/xmlui/handle/11356/1405) (the text version is available [here](http://hdl.handle.net/11356/1388)).

As stated in the data description:

> The compressed files include the ParlaMint.ana XML TEI-encoded linguistically annotated corpus; the derived corpus in CoNLL-U with TSV speech metadata; and the vertical files (with registry file), suitable for use with CQP-based concordancers, such as CWB, noSketch Engine or KonText. Also included is the 2.0 release of the data and scripts available at the GitHub repository of the ParlaMint project.

The files are very large, e.g. the data set for Poland is 1.5 GB zipped.  

To get a list of the structure of folders and files in the downloaded .tgz file, one can run in the terminal:

`tar -t ParlaMint-PL.ana.tgz`

or

`tar -tf ParlaMint-PL.ana.tgz > parlmint_str.txt` to redirect the output to a text file.

To extract only selected files from the .tgz file, e.g. in this case only *.conllu files, run:

`tar -xzf ParlaMint-PL.ana.tgz --wildcards --no-anchored '*.conllu'`

## Using the data

CoNLL-U files can be conveniently read as data frames by the `udpipe_read_conllu` command of the [`udpipe`](https://CRAN.R-project.org/package=udpipe) package.

The metadata with information about speeches are provided in TSV files.

The `cleaning.R` script in the `scripts` folder provides sample code to read CoNLL-U and metadata files for 2019 and 2020 for various countries. Data from Croatia have a different file structure, so the code needs to be adjusted.


## Other resources

The [official ParlaMint github repository](https://github.com/clarin-eric/ParlaMint) contains various resources including scripts.

Comparative analyses of parliamentary discussions in Italy, Poland, Slovenia, and the United Kingdom were the topic of the work in the ParlaMint group at the [Helsinki Digital Humanities Hackathon #DHH21](https://www2.helsinki.fi/en/helsinki-centre-for-digital-humanities/helsinki-digital-humanities-hackathon-2021-dhh21). The resulting blog post is available [here](https://dhhackathon.wordpress.com/2021/05/28/parliamentary-debates-in-the-covid-times/).

Ruben Ros' [ParlaMintCase repository](https://github.com/rubenros1795/ParlaMintCase) contains lots of resources for Python users as well as case studies.