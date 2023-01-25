#ifndef _RAWRRINVOKECS
#include <mono/jit/jit.h>
#endif

#include <mono/metadata/object.h>
#include <mono/metadata/environment.h>
#include <mono/metadata/assembly.h>
#include <mono/metadata/debug-helpers.h>
#include <mono/metadata/mono-config.h>

#include <string.h>
#include <stdlib.h>

#ifndef FALSE
#define FALSE 0
#endif

#include "rawrrRcpp.h"

using namespace Rcpp;

// Expose the classes
RCPP_MODULE (RawrrMod)
{
  class_<Rawrr>("Rawrr")
    .default_constructor ("Default constructor").
    method ("setAssembly", &Rawrr::setAssembly, "Set assembly path.").
    method ("setRawFile", &Rawrr::setRawFile, "Set path of Thermo Fisher raw file.").
    method ("setDomainName", &Rawrr::setDomainName, "Set the name of the domain.").
    method ("createObject", &Rawrr::createObject, "createObject.").
    method ("get_Revision", &Rawrr::get_Revision, "Returns the rawfile revision.").
    method ("openFile", &Rawrr::openFile, "Opens raw file.").
    method ("get_mZvalues", &Rawrr::get_mZvalues, "Returns mZ valyes of a given scan id.").
    method ("get_values", &Rawrr::get_values, "Returns values of a given scan id and method.").
    method ("get_trailer", &Rawrr::get_trailer, "Returns extra trailer of a given scan id.").
    method ("dector", &Rawrr::dector, "Returns extra trailer of a given scan id.").
    method ("get_info", &Rawrr::get_info, "Returns the rawfile revision.").
    method ("dector", &Rawrr::dector, "mono_jit_cleanup");
}

