# `gen_xml`

A behaviour module for processing an XML document.

This is the generic version of a couple of near-identical modules I
have created in other projects.

The module will scan a supplied XML document using
`xmerl_sax_parser:file/2`. While scanning the XML file the callback
functions are called when encountering the start/end element tags. See
the overview docs for details.

To use the module in a project add `gen_xml` to `rebar3.config`, e.g.

> `{deps, [ gen_xml ]}.`

## Build and test

The [rebar3](https://rebar3.org/) tool is used for all the development
processes.

    $ rebar3 dialyzer
    $ rebar3 eunit
    $ rebar3 shell
    ...
 
## The `null` callback module

The `genxml_null` module is used for testing and benchmarking.

It can also be used as a template for new callback modules.

The callback module can be run manually against a file `File` with:

    $ rebar3 shell
    > genxml_null:start(File).

---
