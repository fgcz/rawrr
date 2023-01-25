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
#include <fstream>

#ifndef FALSE
#define FALSE 0
#endif

#include <iostream>

#include <Rcpp.h>
using namespace Rcpp;

class Rawrr
{
  std::string rawFile = "/tmp/sample.raw";
  std::string assemblyFile = "rawrrRcpp.exe";
  MonoDomain *domain;
  MonoAssembly *assembly;
  MonoImage *image;

  MonoMethod *function_set_rawFile;
  MonoMethod *function_get_Revision;
  MonoMethod *function_get_info;
  MonoMethod *function_get_mZvalues;
  MonoMethod *function_get_values;
  MonoMethod *function_get_trailer;
  MonoMethod *function_open_file;

  MonoClass *Raw;
  MonoObject *obj;

private:

public:
    Rawrr ()
  {
    std::ifstream my_file (rawFile.c_str ());
    if (my_file.good ())
      {
	// read away
      }
    else
      {
	Rcpp::Rcerr << "File " << rawFile.c_str () << " is not good." << std::endl;
      }
    mono_config_parse (NULL);
  }

  void setDomainName(std::string s = ""){
    std::string ss = "rawrrr" + s;
    Rcpp::Rcerr << "DomainName=" << ss << std::endl;
    try
    {
    	domain = mono_jit_init_version (ss.c_str(), "v4.0");
    }catch(const std::exception& e)
    {
	    Rcpp::Rcerr << "mono_jit_init_version failed." << std::endl;
    }
  }
  void setAssembly (std::string file)
  {
    Rcpp::Rcout << assemblyFile << std::endl;
    assemblyFile = file;
    Rcpp::Rcout << assemblyFile << std::endl;
  }

  void setRawFile (std::string file)
  {
    rawFile = file;
    Rcpp::Rcerr << rawFile << std::endl;

    MonoString *str;
    void *args[1];
    MonoObject *exception;
    args[0] = mono_string_new (domain, rawFile.c_str ());
    exception = NULL;
    mono_runtime_invoke (function_set_rawFile, obj, args, &exception);
    if (exception)
      {
	Rcpp::Rcout << "Exception was raised in setRawFile\n";
      }
  }

  void createObject ()
  {

    assembly = mono_domain_assembly_open (domain, assemblyFile.c_str ());

    if (!assembly)
      {
	Rcpp::Rcerr << "ASSEMBLY PROBLEM\n" << std::endl;
	return;
      }

    image = mono_assembly_get_image (assembly);

    Raw = mono_class_from_name (image, "RawrrEmbed", "RawRr");
    if (!Raw)
      {
	Rcpp::Rcerr << "Can't find RawRr in assembly " << mono_image_get_filename (image) << std::endl;
	return;
      }

    Rcpp::Rcerr <<  "mono_object_new ...";
    obj = mono_object_new (domain, Raw);
    Rcpp::Rcerr <<  " [DONE]" << std::endl;

    Rcpp::Rcerr <<  "mono_runtime_object_init ...";
    mono_runtime_object_init (obj);
    Rcpp::Rcerr <<  " [DONE]" << std::endl;

    /// browse class methods
    MonoClass *klass;
    MonoMethod *m = NULL;
    void *iter;

    klass = mono_object_get_class (obj);
    domain = mono_object_get_domain (obj);

    function_get_Revision = NULL;
    function_get_info = NULL;
    function_get_mZvalues = NULL;
    function_get_values = NULL;
    function_get_trailer = NULL;
    function_set_rawFile = NULL;
    function_open_file = NULL;
    iter = NULL;
    while ((m = mono_class_get_methods (klass, &iter)))
      {
	//printf ("cpp: method = %s\n", mono_method_get_name (m));
	if (strcmp (mono_method_get_name (m), "get_Revision") == 0)
	  {
	    function_get_Revision = m;
	  }
	else if (strcmp (mono_method_get_name (m), "get_info") == 0)
	  {
	    function_get_info = m;
	  }
	else if (strcmp (mono_method_get_name (m), "openFile") == 0)
	  {
	    function_open_file = m;
	  }
	else if (strcmp (mono_method_get_name (m), "values") == 0)
	  {
	    function_get_values = m;
	  }
	else if (strcmp (mono_method_get_name (m), "trailer") == 0)
	  {
	    function_get_trailer = m;
	  }
	else if (strcmp (mono_method_get_name (m), "setRawFile") == 0)
	  {
	    function_set_rawFile = m;
	  }
	else
	  {
	  }
      }				//while
  }

  CharacterVector get_trailer (int scanIdx)
  {
    void *args[2];
    int val;
    MonoObject *exception;
    val = scanIdx;
    args[0] = &val;
    exception = NULL;
    MonoArray *resultArray =
      (MonoArray *) mono_runtime_invoke (function_get_trailer, obj, args,
					 &exception);
    if (exception)
      {
        Rcpp::Rcerr << "exception raised" << std::endl;
	return (NULL);
      }
    CharacterVector rv (mono_array_length (resultArray));
    for (unsigned int i = 0; i < mono_array_length (resultArray); i++)
      {
	MonoString *s = mono_array_get (resultArray, MonoString *, i);
	char *s2 = mono_string_to_utf8 (s);
	rv[i] = s2;
      }
    return (rv);
  }

  NumericVector get_values (int scanIdx, std::string method)
  {
    int val;
    MonoObject *exception;
    MonoString *str;

    val = scanIdx;

    void *args[2];
    args[0] = &val;
    args[1] = mono_string_new (domain,method.c_str());

    exception = NULL;

    //Rcpp::Rcout << method << "\t" << function_get_values << std::endl;

    MonoArray *resultArray = (MonoArray *) mono_runtime_invoke (function_get_values, obj, args, &exception);

    if (exception)
      {
    	Rcpp::Rcerr << "An exception was raised" << std::endl;
	get_info();
	return (NULL);
      }

    NumericVector rv (mono_array_length (resultArray));
    for (unsigned int i = 0; i < mono_array_length (resultArray); i++)
      {
	MonoString *s = mono_array_get (resultArray, MonoString *, i);
	char *s2 = mono_string_to_utf8 (s);
	rv[i] = atof (s2);
      }
    return (rv);
  }

  NumericVector get_mZvalues (int scanIdx)
  {
    void *args[1];
    int val;
    MonoObject *exception;
    val = scanIdx;
    args[0] = &val;
    exception = NULL;
    MonoArray *resultArray =
      (MonoArray *) mono_runtime_invoke (function_get_mZvalues, obj, args,
					 &exception);
    if (exception)
      {
	return (-1);
      }
    NumericVector rv (mono_array_length (resultArray));
    for (unsigned int i = 0; i < mono_array_length (resultArray); i++)
      {
	MonoString *s = mono_array_get (resultArray, MonoString *, i);
	char *s2 = mono_string_to_utf8 (s);
	rv[i] = atof (s2);
      }
    return (rv);
  }

  void openFile()
  {
    MonoObject *result, *exception;
    int val;

    exception = NULL;

    result = mono_runtime_invoke (function_open_file, obj, NULL, &exception);

    if (exception)
      {
	Rcpp::Rcerr << "An exception was thrown while open raw file." << std::endl;
      }
  }

  int get_Revision ()
  {
    MonoObject *result, *exception;
    int val;

    exception = NULL;
    result = mono_runtime_invoke (function_get_Revision, obj, NULL, &exception);
    if (exception)
      {
	Rcpp::Rcerr << "An exception was thrown get_Revision()." << std::endl;
	return (-1);
      }

    val = *(int *) mono_object_unbox (result);

    //int retval = mono_environment_exitcode_get ();

    return (val);
  }

  int get_info ()
  {
    MonoObject *exception;

    exception = NULL;

    MonoArray *result =
      (MonoArray *) mono_runtime_invoke (function_get_info, obj, NULL,
					 &exception);

    if (exception)
      {
	Rcpp::Rcerr << "An exception was thrown get_info()." << std::endl;
	return (-1);
      }

    for (unsigned int i = 0; i < mono_array_length (result); i++)
      {
	MonoString *s = mono_array_get (result, MonoString *, i);
	char *s2 = mono_string_to_utf8 (s);
	Rcpp::Rcout << "INFO[" << i << "]: " << s2 << std::endl;
      }

    return (0);
  }

  void dector(){
    mono_jit_cleanup (domain);
    Rcpp::Rcout << "dector" << std::endl;
  }

};				//class
