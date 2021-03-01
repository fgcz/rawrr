This folder should contain the following files:

```
MD5 (bin/ThermoFisher.CommonCore.RawFileReader.dll) = d001a0510d1290c5cc45ea7dba0ab54e
MD5 (bin/ThermoFisher.CommonCore.Data.dll) = 5796cbbd7b4da4edcda620abfcbc3225
MD5 (bin/ThermoFisher.CommonCore.MassPrecisionEstimator.dll) = ac361a15b71910e0b90ba9655278d048
```

In general, ThermoFisher.CommonCore dlls can be obtained through:

https://planetorbitrap.com/rawfilereader

by contacting Jim Shofstahl using

jim.Shofstahl@thermofisher.com

Once the dlls have been copied to this folder, you should be able to install
rawR from source by using:

 `R CMD build rawrr && R CMD INSTALL rawrr_*.gz`

We **highly** recommend to install rawrr by using GitHub releases published at:

https://github.com/fgcz/rawrr/releases

These contain the dlls and the rawR.exe already!


as alternative you can set the MONO_PATH enviroment, e.g., 

`export MONO_PATH="/path/to/ThermoFisher/CommonCore/DLL/files"`
