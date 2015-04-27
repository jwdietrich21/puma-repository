HL7 Engine Notes

The HL7 Engine is a Pascal unit that provides functions for parsing and compiling HL7 messages as well as for reading and writing messages as files or streams.

In order to use the HL7 Engine in your own enterprises you simply have to add the file "hl7.pas" to your project. You may also want to add additional units like "msh.pas" etc. to your project to get higher-level support of certain HL7 functionality. All high-level units depend from "hl7.pas", which has therefore to be added in any case.

In the directory "Example_Programs" you find sample programs that illustrate how to use the HL7 engine.

The directory "Samples" contains sample HL7 messages for testing purposes.

Files in the directory "Test Cases for FPCUnit" are for unit testing with FPCUnit. They are not necessary for normal use, except for the case that you want to improve and test the Unit Converter.


About the PUMA repository

The PUMA repository is a collection of units for development of medical applications in Pascal. See http://puma-repository.sf.net for details.


License

Files of the PUMA repository are licensed with a BSD license. This facilitates their usage in both free open source software and commercial applications.

J. W. Dietrich
