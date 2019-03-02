# EMsoft File Path Conventions

Starting with version 4.3, EMsoft can be configured in three different ways with respect to where output files will be stored: using a configuration file, using environment variables, or using absolute paths.  Which mode the user selects depends mostly on personal preference; in some cases, when using scripts to execute sequences of EMsoft programs, it may be easier to use environment variables instead of the configuration file.

## Using a configuration file
This is described in detail in the wiki help page on [Package Configuration](https://github.com/EMsoft-org/EMsoft/wiki/Package-Configuration).  Basically, the confguration file is a JSON file that contains definitions for a number of file paths, namely:

- EMsoftpathname
- EMXtalFolderpathname
- EMdatapathname
- EMtmppathname
- EMsoftLibraryLocation

## Using environment variables
The user can choose not to create a configuration file (or disable an existing configuration file by temporarily renaming it) and instead use environment variables, for instance in a .cshrc file on UNIX systems or in a shell script.  The following variables can be defined (all in upper case):

- EMSOFTPATHNAME
- EMXTALFOLDERPATHNAME
- EMDATAPATHNAME
- EMTMPNAME
- EMSOFTLIBRARYLOCATION
- USERNAME
- USERLOCATION
- USEREMAIL

In a .cshrc file, the entries would be like this:

```fortran
setenv EMDATAPATHNAME /Users/username/Files/EMdata 
```
In this mode, EMsoft programs will prepend the correct path to all filenames entered via .nml name list files, in exactly the same way as is done when using a configuration JSON file.


## Using absolute paths
If the user only sets the EMSOFTPATHNAME environment variable, and leaves all other path variables undefined, then the EMsoft programs will assume that absolute paths need to be used.  Every file produced by an EMsoft program, including the crystal structure files and any temporary files, will be stored in the folder from which the program is executed, unless the user specifies an absolute path. 

