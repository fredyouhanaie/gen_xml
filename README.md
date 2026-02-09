# `gen_xml`

[![Erlang CI](https://github.com/fredyouhanaie/gen_xml/actions/workflows/erlang.yml/badge.svg)](https://github.com/fredyouhanaie/gen_xml/actions/workflows/erlang.yml)

A behaviour module for processing an XML document.

This is the generic version of a couple of near-identical modules I
have created in other projects.

The module will scan a supplied XML document using
`xmerl_sax_parser:file/2`. While scanning the XML file the callback
functions are called when encountering the start/end element tags. See
the overview docs for details.

The module is particularly useful for scanning/processing very large
documents. The simpler alternative scanner, `xmerl_scan:file/1`, does
not scale well for large files, for example a document with 1,000,000
elements.

The behaviour has the added advantage of enabling asynchronous
processing of the XML elements while the scanner continues with
scanning the rest of the document.

To use the module in a project add `gen_xml` to `rebar3.config`, e.g.

> `{deps, [ gen_xml ]}.`

See the `Examples` directory for some example callback modules.

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

## The `counts` callback module

The `genxml_counts` module is used for testing and benchmarking.

It can also be used as a template for new callback modules.

The module will return the count of the element tags found in the XML
document.

The callback module can be run manually against a file `File` with:

    $ cd genxml
    $ rebar3 shell
    > genxml_counts:start(File).

---

## The `paths` callback module

This module reads an XML document and generates a list of paths that
represents the XML document structure.

The module includes two helper functions: `collect/1` and `print/1`,
the former returns the paths as a list, while the latter prints them
directly to the terminal.

```shell
$ rebar3 shell

1> genxml_paths:collect("Examples/sample-xml-files-sample-4.xml").
{ok,["root/book/year","root/book/author","root/book/title",
     "root/book","root/person/email","root/person/age",
     "root/person/name","root/person","root/person/email",
     "root/person/age","root/person/name","root/person","root"]}

2> genxml_paths:print("Examples/sample-xml-files-sample-4.xml").
root
root/person
root/person/name
root/person/age
root/person/email
root/person
root/person/name
root/person/age
root/person/email
root/book
root/book/title
root/book/author
root/book/year
{ok,[]}
```

---

## The CLI

The command line interface enables running the various handlers from
the terminal.

The module is `gen_xml_cli.erl`, however, the escript generated with
rebar3 is renamed to `gen_xml`.

```shell
$ rebar3 escritize
$ ./_build/default/bin/gen_xml 
error: gen_xml: subcommand expected
Usage:
  gen_xml {counts|null|paths} [-v] [--verbose] <file>

Subcommands:
  counts        run the counts callback module
  null          run the null callback module
  paths         run the paths callback module

Arguments:
  file          file

Optional arguments:
  -v, --verbose be verbose, can use multiple times for warning to debug
```

The CLI subcommands and options are defined in the include file.

---
