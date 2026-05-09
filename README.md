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

The behaviour has the added advantage of enabling processing of the
XML elements asynchronously while the scanner continues with scanning
the rest of the document.

To use the module in a project add `gen_xml` to `rebar3.config`, e.g.

> `{deps, [ gen_xml ]}.`

See the `Examples` directory for some example callback modules.

## Build and test

The [rebar3](https://rebar3.org/) tool is used for all the development
processes.

```shell
$ rebar3 dialyzer
$ rebar3 eunit
$ rebar3 shell
...
```

---

## The `null` callback module

The `genxml_null` module is used for testing and benchmarking.

It can also be used as a template for new callback modules.

The callback module can be run manually against a file `File` with:

```shell
$ rebar3 shell

1> genxml_null:start("Examples/sample-xml-files-sample-5.xml").
{ok,null}

2> timer:tc(genxml_null, start, ["Examples/sample-xml-files-sample-4.xml"]).
{158,{ok,null}}

3> timer:tc(genxml_null, start, ["Examples/sample-xml-files-sample-6.xml"]).
{2841,{ok,null}}
```

---

## The `counts` callback module

The `genxml_counts` module is used for testing and benchmarking.

It can also be used as a template for new callback modules.

The module will return the count of the element tags found in the XML
document.

The callback module can be run manually against a file `File` with:

```shell
$ rebar3 shell

1> genxml_counts:start("Examples/sample-xml-files-sample-4.xml").
{ok,#{name => 2,root => 1,title => 1,author => 1,person => 2,
      age => 2,email => 2,book => 1,year => 1}}

2> genxml_counts:start("Examples/sample-xml-files-sample-6.xml").
{ok,#{name => 48,root => 1,title => 24,author => 24,person => 48,
      age => 48,email => 48,book => 24,year => 24}}
```

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
$ rebar3 escriptize
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

You can create a symlink for convenience:

```shell
$ ln -sv _build/default/bin/gen_xml
```

The output of the CLI is formatted for readability as well as for
further processing:

```shell
$ ./gen_xml counts Examples/sample-xml-files-sample-5.xml
      16,name
       1,root
       8,title
       8,author
      16,person
      16,age
      16,email
       8,book
       8,year

$ ./gen_xml paths Examples/sample-xml-files-sample-4.xml
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

$ ./gen_xml paths Examples/sample-xml-files-sample-4.xml | sort -u
root
root/book
root/book/author
root/book/title
root/book/year
root/person
root/person/age
root/person/email
root/person/name
```

---
