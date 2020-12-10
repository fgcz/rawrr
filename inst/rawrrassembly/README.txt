This folder should contain the following files:

ThermoFisher.CommonCore.Data.dll
ThermoFisher.CommonCore.MassPrecisionEstimator.dll
ThermoFisher.CommonCore.RawFileReader.dll
rawR.exe

In general, ThermoFisher.CommonCore dlls can be obtained through:

https://planetorbitrap.com/rawfilereader

by contacting Jim Shofstahl using

jim.Shofstahl@thermofisher.com

Once the dlls have been copied to this folder, you should be able to install
rawR from source by using:

R CMD INSTALL

We **highly** recommend to install rawR by using GitHub releases published at:

https://github.com/fgcz/rawR/releases

These contain the dlls and the rawR.exe already!


as alternative you can set the MONO_PATH enviroment, e.g., 

```
export MONO_PATH="/path/to/ThermoFisher/CommonCore/DLL/files""
```
