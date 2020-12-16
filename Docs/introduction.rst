Introduction
============

X²XMLDataBinding is an alternative to the Delphi built-in XML data binding generator with a few benefits.

The tool has been compiled and tested with Delphi 10.2, your mileage may vary with other versions. It was originally compiled with Delphi XE2 and probably still works on many versions.

The files it generates are most likely compatible with Delphi XE2 and up as well, perhaps even lower versions, let me know if you have experience with them!


Key features
------------

* Proper namespace support
* Output to a single file or multiple files, allowing types to be reused when included from several root schemas
* Has<Name> properties for optional elements and attributes
* <Name>IsNil properties for xsi:nil elements
* <Name>Def methods to safely read optional values with a default
* Read and write enumeration properties as a typed enum or raw text
* Proper boolean support
* Much improved support for date/time values
* Support for Base64 encoded values
* Basic validation for outputting XML documents which applies element order for sequences and checks for the presence of required elements
* Influence the generator by using a :doc:`hints`

Note: some features might already be present in the built-in XML data binding generator, but have been buggy in older Delphi versions and some still are.

This tool does not implement the full XSD specification, as some parts are obviously a design by committee. However, feel free to enhance the parsing if you have a particular schema which can not be processed!