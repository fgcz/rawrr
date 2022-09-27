[![JPR](https://img.shields.io/badge/JPR-10.1021%2Facs.jproteome.0c00866-brightgreen)](http://dx.doi.org/10.1021/acs.jproteome.0c00866)
[![codecov](https://codecov.io/gh/fgcz/rawrr/branch/master/graph/badge.svg?token=OO4Y7G4UUX)](https://codecov.io/gh/fgcz/rawrr)
[![R-CMD-check-rawrr](https://github.com/fgcz/rawrr/actions/workflows/r.yml/badge.svg)](https://github.com/fgcz/rawrr/actions/workflows/r.yml)

![rawrrHexSticker](https://github.com/fgcz/rawrr/blob/master/rawrr_logo.png)

# rawrr

The package provides access to proprietary Thermo Fisher Scientific Orbitrap instrument data as a stand-alone R package or serves as [MsRawFileReaderBackend](https://github.com/cpanse/MsBackendRawFileReader) for the Bioconductor [Spectra](https://bioconductor.org/packages/Spectra/) package.
rawrr wraps the functionality of the [RawFileReader](https://planetorbitrap.com/rawfilereader) [.NET assembly](https://www.mono-project.com/docs/advanced/assemblies-and-the-gac/). 
Test files are provided by the [tartare](https://bioconductor.org/packages/tartare/) ExperimentData package.

## Install

Please follow the [INSTALL](https://bioconductor.org/packages/release/bioc/install/rawrr/INSTALL) instructions provided through 

https://bioconductor.org/packages/rawrr/

## Manuscript

http://dx.doi.org/10.1021/acs.jproteome.0c00866

## Talks

- [rawrr - invoking managed code using ThermoFisher.CommonCore.RawFileReader;](http://fgcz-ms.uzh.ch/~cpanse/talks/20211123-rawrrRcpp_MetaRbolimics2021.html) presentaton at 
[3rd de.NBI / ELIXIR-DE metaRbolomics Hackathon 2021](https://www.denbi.de/news/1299-3rd-de-nbi-elixir-de-metarbolomics-hackathon) in Lutherstadt Wittenberg (22-24 November); [code snippets](https://github.com/cpanse/rawrrRcpp).

- [MsRawFileReaderBackend](https://bioconductor.org/packages/MsBackendRawFileReader/) presentaton at
[European Biocondutor Meeting, de Duve Institute, UCLouvain, Campus de Woluw ́e Brussels, Belgium, Dec 2019](http://fgcz-ms.uzh.ch/~cpanse/talks/rawR_EuroBioc2019_Brussels_88c9.pdf)
