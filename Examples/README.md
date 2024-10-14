# `gen_xml` examples

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

## The test files

The three sample files were downloaded from the
[Toolsfairy](https://toolsfairy.com/tools/code-test/sample-xml-files)
web site on 2024-10-13 (Copyright © Toolsfairy 2024)

    sample-xml-files-sample-4.xml
    sample-xml-files-sample-5.xml
    sample-xml-files-sample-6.xml

---
