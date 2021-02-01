# X²XMLDataBinding
*an alternative to the Delphi built-in XML data binding generator with a few benefits.*

## Key features
 - Proper namespace support
 - Output to a single file or multiple files, allowing types to be reused when included from several root schemas
 - Has<Name> properties for optional elements and attributes
 - <Name>IsNil properties for xsi:nil elements
 - <Name>Def methods to safely read optional values with a default
 - Read and write enumeration properties as a typed enum or raw text
 - Proper boolean support
 - Much improved support for date/time values
 - Support for Base64 encoded values
 - Basic validation for outputting XML documents which applies element order for sequences and checks for the presence of required elements
 - Influence the generator by using a Hints file

 ## Dependencies
 - JEDI Component Library (jcl /jvcl)
 - x2utils (https://github.com/MvRens/x2utils.git) (git submodule)
 - x2log (https://github.com/MvRens/x2log.git) (git submodule)


Documentation is available on [ReadTheDocs](https://x2xmldatabinding.readthedocs.io/).