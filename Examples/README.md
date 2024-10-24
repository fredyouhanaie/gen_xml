# `gen_xml` examples

`genxml` is an escript that enables running the known callback modules
from the command line.

The script supports the `null` module from the main source tree and
the three example modules in this subtree.

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

## The `ets` callback module

This module reads an XML document and saves its contents into a newly
created ETS table.

The module can be run on the command line via the `genxml` CLI. The
output of the CLI is the contents of the ETS table as an Erlang list
of tuples, as produced with `ets:tab2list/1`.

The callback module can run from CLI as follows:

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

## The `paths` callback module

This module reads an XML document and prints the document structure in
the form of a set of paths. For example:

```
$ ./_build/default/bin/genxml paths ../sample-xml-files-sample-4.xml

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
```

---

## The test files

The three sample files were downloaded from the
[Toolsfairy](https://toolsfairy.com/tools/code-test/sample-xml-files)
web site on 2024-10-13 (Copyright © Toolsfairy 2024)

    sample-xml-files-sample-4.xml
    sample-xml-files-sample-5.xml
    sample-xml-files-sample-6.xml

---
