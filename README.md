# Diameter-Xml-Dict

This is a simple utility for quickly generating [XML
Dictionary](https://tools.ietf.org/html/draft-frascone-xml-dictionary-00
"Diameter XML Dictionary") or
[ASN.1](https://en.wikipedia.org/wiki/Abstract_Syntax_Notation_One)
for [Diameter Protocol](https://en.wikipedia.org/wiki/Diameter_(protocol))
dictionaries from 3GPP or IETF documents.

## Install

You need [Stack](https://docs.haskellstack.org/en/stable/README/) to
be installed locally.

```
stack install
```

## Usage

Generating XML Example:

```
diameter-xml-dict  --application-id 16777236 --application-name 3GPP-29-214 --vendor-id 10415 --vendor-name 3GPP \
    --csv test-data/3GPP_TS_29_214.csv --bnf test-data/3GPP_TS_29_214.bnf xml > 3GPP_TS_29_214.xml
```

Generating ASN.1 Example:

```
diameter-xml-dict  --application-id 16777236 --application-name ThreeGPP-29-214 --vendor-id 10415 --vendor-name 3GPP \
    --csv test-data/3GPP_TS_29_214.csv --bnf test-data/3GPP_TS_29_214.bnf asn1 > 3GPP_TS_29_214.asn1
```

To generate dictionary you need two files:

- AVP descriptions in CSV format
- descriptions of commands, grouped AVPs, enumerated AVPs in BNF-like notation

Look in [test-data](./test-data) directory for examples.

### CSV file

CSV file format follows common for 3GPP and IETF tabular format for AVPs

```
"Attribute Name","Code","Section","Data","MUST","MAY","SHLD NOT","MUST NOT","Encr","Applicability"
```

You can easily extract this table from PDF document with such free
tool as [Tabula](http://tabula.technology/).

### BNF file

BNF file format follows common for Diameter RFCs, 3GPP and IETF formats for commands descriptions
as described in [Diameter Base Protocol](https://tools.ietf.org/html/rfc6733).

So you mostly need to copy/paste them from document.

1. Commands

```
COMMAND-NAME ::= <Diameter-Header: COMMAND-CODE, R-BIT, P-BIT >
...
```

2. Grouped AVPs

```
AVP-NAME ::= <AVP-Header: AVP-CODE >
...
```

3. Enumerated AVPs

```
AVP-NAME ::= <AVP-Enumerated: AVP-CODE >
<MEMBER-NAME [:MEMBER-CODE]>
...
```

4. Imports (for ASN.1)

```
DICTIONARY-NAME ::= <AVP-Import>
<AVP-NAME>
...
```

