# `gen_xml` examples

`genxml` is an escript that enables running the known callback modules
from the command line.

The script supports all the callback modules from the main source
tree as well as the `ets` and `attrs` modules:

```shell
$ ./_build/default/bin/genxml
error: genxml: subcommand expected
Usage:
  genxml <command> [-v] [--verbose] <file>

Subcommands:
  attrs         run the attrs callback module
  counts        run the counts callback module
  ets           run the ets callback module
  null          run the null callback module
  paths         run the paths callback module

Arguments:
  file          file

Optional arguments:
  -v, --verbose be verbose, can use multiple times for warning to debug
```

---

## The `ets` callback module

This module reads an XML document and saves its contents into a newly
created ETS table.

The module can be run on the command line via the `genxml` CLI. The
output of the CLI is the contents of the ETS table as an Erlang list
of tuples, as produced with `ets:tab2list/1`.

The callback module can be run from the CLI as follows:

```erlang
$ ./_build/default/bin/genxml ets ../sample-xml-files-sample-4.xml
[{0,'$root',#{},0,[]},
 {1282,root,#{},0,[]},
 {1314,person,#{},1282,[]},
 {1346,name,#{},1314,[]},
 {1378,'$text',#{},1346,"John Doe"},
 {1410,age,#{},1314,[]},
 {1442,'$text',#{},1410,"30"},
 {1474,email,#{},1314,[]},
 {1506,'$text',#{},1474,"john.doe@example.com"},
 {1538,person,#{},1282,[]},
 {1570,name,#{},1538,[]},
 {1602,'$text',#{},1570,"Jane Smith"},
 {1634,age,#{},1538,[]},
 {1666,'$text',#{},1634,"25"},
 {1698,email,#{},1538,[]},
 {1730,'$text',#{},1698,"jane.smith@example.com"},
 {1762,book,#{},1282,[]},
 {1794,title,#{},1762,[]},
 {1826,'$text',#{},1794,"The Adventure Begins"},
 {1858,author,#{},1762,[]},
 {1890,'$text',#{},1858,"Robert Johnson"},
 {1922,year,#{},1762,[]},
 {1954,'$text',#{},1922,"2022"}]
```

The structure of the tuple is as follows:

1. `id`: unique integer for the record. The very first dummy record
   has `id` 0 and `$root` as tag.
1. `tag`: the tag of the XML element, or the atoms `$root` or
   `$text`. The latter represents the contents of an XML element.
1. `attr`: the map of attributes of the element.
1. `parent`: the immediate container of this element, 0 represents the
   dummy `$root` element.
1. `text`: the contents of an element, `parent` identifies the
   containing element.

---

## The `attrs` callback module

This is another example module that scans an XML document and collects
the atributes contained in the element start tags. The module returns
a list of tuples (pairs) where the first element is the element tag
and the second is a, possiby empty, list of attribute names.

The local `genxml` script will take a single XML file and produce a
table of the tag/attribute lines.

---

## The test files

The three sample files were downloaded from the
[Toolsfairy](https://toolsfairy.com/tools/code-test/sample-xml-files)
web site on 2024-10-13 (Copyright © Toolsfairy 2024)

    sample-xml-files-sample-4.xml
    sample-xml-files-sample-5.xml
    sample-xml-files-sample-6.xml

---
