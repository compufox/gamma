# gamma - static site generator
### _ava fox_

## ***this is very alpha and doesn't do much error handling/checking***
## ***viewer discretion is advised***

gamma is a command line tool that converts markdown and templates into static sites 

## Building 

1- install a lisp (we assume you're using SBCL with quicklisp set up already)

2- `mkdir ~/common-lisp && git clone https://github.com/compufox/gamma ~/common-lisp/gamma`

3- `sbcl --eval "(ql:quickload :gamma)" --eval "(asdf:make :gamma)"` 

4- the binary should be at `~/common-lisp/gamma/bin`

## Usage

```
$ ./gamma -h
generates static HTML from markdown files

Usage: gamma [-h|--help] [-c|--config FILE] [--version] [-o|--out DIR]
             [-r|--root DIR] [-u|--check-underscore]

Available options:
  -h, --help             prints this help text
  -c, --config FILE      config file FILE to load (defaults to site.conf)
  --version              prints the application version
  -o, --out DIR          directory DIR to output html into (defaults to _site)
  -r, --root DIR         root directory DIR for the site
  -u, --check-underscore if present, check any directories with a leading underscore
```

## License

NPLv1+

