![rawrrHexSticker](https://github.com/fgcz/rawrr/blob/master/rawrr_logo.png)

# rawrr

The package provides access to proprietary Thermo Fisher Scientific Orbitrap instrument data as a stand-alone R package or serves as [MsRawFileReaderBackend](https://github.com/cpanse/MsBackendRawFileReader) for the Bioconductor [Spectra](https://bioconductor.org/packages/Spectra/) package.
rawrr wraps the functionality of the [RawFileReader](https://planetorbitrap.com/rawfilereader) .NET assembly. 
Test data are provided by the [tartare](https://bioconductor.org/packages/tartare/) `ExperimentData` package.
## Install

### build package
Please install the latest release from https://github.com/fgcz/rawR/releases according to the provided instructions.


### from source

* install https://planetorbitrap.com/rawfilereader DLL files, e.g., copy file to `/usr/local/lib/RawFileReader/`

* set `export MONO_PATH=/usr/local/lib/RawFileReader/`



## System requirements

* [R](https://cran.r-project.org/) (>= 4.1)

* [New RawFileReader .Net Assembly from Thermo Fisher Scientific](https://planetorbitrap.com/rawfilereader)

* U.S. language setting 
 
### MS Windows

Make sure that .NET Framework 4.5.1 or higher is installed.

### macOS|Linux

You need to have [Mono](https://www.mono-project.com) installed. Mono is an open source implementation of Microsoft's .NET Framework.

## Manuscripts

https://www.biorxiv.org/content/10.1101/2020.10.30.362533v1 (accepted at [ACS JPR](https://pubs.acs.org/journal/jprobs))

## Blog posts

http://proteomicsnews.blogspot.com/2020/11/raw-mass-spec-data-is-too-pretty-for.html

## Slides

MsRawFileReaderBackend presentaton at
[European Biocondutor Meeting, de Duve Institute, UCLouvain, Campus de Woluw ́e Brussels, Belgium, Dec 2019](http://fgcz-ms.uzh.ch/~cpanse/talks/rawR_EuroBioc2019_Brussels_88c9.pdf)
