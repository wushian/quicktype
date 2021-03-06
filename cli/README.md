## Synopsis

```shell
$ quicktype [--lang ts|go|cs|elm|schema|swift] FILE|URL ... 
```

## Description

Given JSON sample data, quicktype outputs code for working with that data in  
TypeScript, Go, C#, Elm, and more.                                            

## Options

```
-o, --out FILE                         The output file. Determines --lang and --top-level. 
-t, --top-level NAME                   The name for the top level type.                    
-l, --lang ts|go|cs|elm|schema|swift   The target language.                                
-s, --src-lang json|schema             The source language (default is json).              
--src FILE|URL                         The file or url to type.
--src-urls FILE                        Tracery grammar describing URLs to crawl.                      
-h, --help                             Get some help.                                      
```

## Examples

### Generate C# to parse a Bitcoin API

```shell                              
$ quicktype -o LatestBlock.cs https://blockchain.info/latestblock 
```

### Generate Go code from a JSON file

```shell
$ quicktype -l go user.json                                       
```

### Generate JSON Schema, then TypeScript

```shell                         
$ quicktype -o schema.json https://blockchain.info/latestblock    
$ quicktype -o bitcoin.ts --src-lang schema schema.json           
```

Learn more at [quicktype.io](https://quicktype.io).