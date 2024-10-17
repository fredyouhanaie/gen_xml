
Fred Youhanaie <fyrlang@anydata.co.uk>

Copyright 2024 Fred Youhanaie

# Introduction

`gen_xml` is an Erlang bahaviour module for processing eXtensible
Markup Language (XML) files.

## Module Overview

To start the process of parsing the XML file, an application should
call the `read/3` function. While `read/3` is scanning the XML file it
will call the appropriate handler functions in the supplied callback
module.

### The Callback Module

The callback module should export three functions for processing the
individual XML elements:

* `handle_begin(Tag, Attr, State)`, is called whenever a new tag is
  encountered. It takes three arguments: `Tag` is an `atom`
  corresponding to the element tag, `Attrs` is the list of attributes
  from the `startElement` event, see `xmerl_sax_parser` for further
  details. If required, the attributes can be converted to a map of
  attribute name/value pairs using the `gen_xml:attr_map/1` function.
  
  The third parameter, `State`, is explained below.

* `handle_end(Tag, State)` corresponds to the `endElement` event,
  which signals the `</Tag>` element. It can be used to finish off any
  outstanding work for this element.

* `handle_text(Text, State)` corresponds to the `characters` event,
  which is the text within an element.

Associated with each callback module is a state variable maintained by
the `gen_xml:read/3` function, and is initialized to the value that is
supplied to it during the call. The current contents of the state
variable is passed to the handler functions, which should return it,
optionally updated, for the future calls to the handlers.

The structure of the state variable is entirely dependent on the
callback module, for example the `genxml_counter` module will maintain
a map where the keys are the XML element tags and the values are the
cumulative counts of the corresponding tags.

Once `read/3` has concluded processing the file, the final contents of
the state variable is returned to the caller.


A number of sample callback modules have been provided in the
`Examples/` directory of the git repo.

---
