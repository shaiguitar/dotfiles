
au BufRead,BufNewFile *.scala set filetype=scala
" Vim indent file
" Language   : Scala (http://scala-lang.org/)
" Maintainer : Stefan Matthias Aust
" Last Change: 2006 Apr 13

if exists("b:did_indent")
  finish
endif
let b:did_indent = 1

setlocal indentexpr=GetScalaIndent()

setlocal indentkeys=0{,0},0),!^F,<>>,o,O

setlocal autoindent shiftwidth=2 tabstop=2 softtabstop=2 expandtab

if exists("*GetScalaIndent")
  finish
endif

function! CountParens(line)
  let line = substitute(a:line, '"\(.\|\\"\)*"', '', 'g')
  let open = substitute(line, '[^(]', '', 'g')
  let close = substitute(line, '[^)]', '', 'g')
  return strlen(open) - strlen(close)
endfunction

function! GetScalaIndent()
  " Find a non-blank line above the current line.
  let lnum = prevnonblank(v:lnum - 1)

  " Hit the start of the file, use zero indent.
  if lnum == 0
    return 0
  endif

  let ind = indent(lnum)
  let prevline = getline(lnum)

  "Indent html literals
  if prevline !~ '/>\s*$' && prevline =~ '^\s*<[a-zA-Z][^>]*>\s*$'
    return ind + &shiftwidth
  endif

  " Add a 'shiftwidth' after lines that start a block
  " If if, for or while end with ), this is a one-line block
  " If val, var, def end with =, this is a one-line block
  if prevline =~ '^\s*\<\(\(else\s\+\)\?if\|for\|while\)\>.*[)]\s*$'
        \ || prevline =~ '^\s*\<\(\(va[lr]\|def\)\>.*[=]\s*$'
        \ || prevline =~ '^\s*\<else\>\s*$'
        \ || prevline =~ '{\s*$'
    let ind = ind + &shiftwidth
  endif

  " If parenthesis are unbalanced, indent or dedent
  let c = CountParens(prevline)
  echo c
  if c > 0
    let ind = ind + &shiftwidth
  elseif c < 0
    let ind = ind - &shiftwidth
  endif
  
  " Dedent after if, for, while and val, var, def without block
  let pprevline = getline(prevnonblank(lnum - 1))
  if pprevline =~ '^\s*\<\(\(else\s\+\)\?if\|for\|while\)\>.*[)]\s*$'
        \ || pprevline =~ '^\s*\<\(\va[lr]\|def\)\>.*[=]\s*$'
        \ || pprevline =~ '^\s*\<else\>\s*$'
    let ind = ind - &shiftwidth
  endif

  " Align 'for' clauses nicely
  if prevline =~ '^\s*\<for\> (.*;\s*$'
    let ind = ind - &shiftwidth + 5
  endif

  " Subtract a 'shiftwidth' on '}' or html
  let thisline = getline(v:lnum)
  if thisline =~ '^\s*[})]' 
        \ || thisline =~ '^\s*</[a-zA-Z][^>]*>'
    let ind = ind - &shiftwidth
  endif

  " Indent multi-lines comments
  if prevline =~ '^\s*\/\*\($\|[^*]\(\(\*\/\)\@!.\)*$\)'
    let ind = ind + 1
  endif

  " Indent multi-lines ScalaDoc
  if prevline =~ '^\s*\/\*\*\($\|[^*]\(\(\*\/\)\@!.\)*$\)'
    let ind = ind + 2
  endif

  " Dedent after multi-lines comments & ScalaDoc
  if prevline =~ '^\s*\(\(\/\*\)\@!.\)*\*\/.*$'
    " Dedent 1
    let ind = ind - 1
    " Align to any multiple of 'shiftwidth'
    let ind = ind - (ind % &shiftwidth)
  endif

  return ind
endfunction
" Vim plugin that generates new Scala source file when you type
"    vim nonexistent.scala.
" Scripts tries to detect package name from the directory path, e. g.
" .../src/main/scala/com/mycompany/myapp/app.scala gets header
" package com.mycompany.myapp
"
" Author     :   Stepan Koltsov <yozh@mx1.ru>

function! MakeScalaFile()
    if exists("b:template_used") && b:template_used
        return
    endif
    
    let b:template_used = 1
    
    let filename = expand("<afile>:p")
    let x = substitute(filename, "\.scala$", "", "")
    
    let p = substitute(x, "/[^/]*$", "", "")
    let p = substitute(p, "/", ".", "g")
    let p = substitute(p, ".*\.src$", "@", "") " unnamed package
    let p = substitute(p, ".*\.src\.", "!", "")
    let p = substitute(p, "^!main\.scala\.", "!", "") "
    let p = substitute(p, "^!.*\.ru\.", "!ru.", "")
    let p = substitute(p, "^!.*\.org\.", "!org.", "")
    let p = substitute(p, "^!.*\.com\.", "!com.", "")
    
    " ! marks that we found package name.
    if match(p, "^!") == 0
        let p = substitute(p, "^!", "", "")
    else
        " Don't know package name.
        let p = "@"
    endif
    
    let class = substitute(x, ".*/", "", "")
    
    if p != "@"
        call append("0", "package " . p)
    endif
    
    "norm G
    "call append(".", "class " . class . " {")
    
    "norm G
    "call append(".", "} /// end of " . class)
    
    call append(".", "// vim: set ts=4 sw=4 et:")
    call append(".", "")
    
endfunction

au BufNewFile *.scala call MakeScalaFile()

" vim: set ts=4 sw=4 et:
" Vim syntax file
" Language   : Scala (http://scala-lang.org/)
" Maintainers: Stefan Matthias Aust, Julien Wetterwald
" Last Change: 2007 June 13

if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

syn case match
syn sync minlines=50

" most Scala keywords
syn keyword scalaKeyword abstract case catch do else extends final finally for forSome if implicit lazy match new null override private protected requires return sealed super this throw try type while with yield
syn match scalaKeyword "=>"
syn match scalaKeyword "<-"
syn match scalaKeyword "\<_\>"

syn match scalaOperator ":\{2,\}" "this is not a type

" package and import statements
syn keyword scalaPackage package nextgroup=scalaFqn skipwhite
syn keyword scalaImport import nextgroup=scalaFqn skipwhite
syn match scalaFqn "\<[._$a-zA-Z0-9,]*" contained nextgroup=scalaFqnSet
syn region scalaFqnSet start="{" end="}" contained

" boolean literals
syn keyword scalaBoolean true false

" definitions
syn keyword scalaDef def nextgroup=scalaDefName skipwhite
syn keyword scalaVal val nextgroup=scalaValName skipwhite
syn keyword scalaVar var nextgroup=scalaVarName skipwhite
syn keyword scalaClass class nextgroup=scalaClassName skipwhite
syn keyword scalaObject object nextgroup=scalaClassName skipwhite
syn keyword scalaTrait trait nextgroup=scalaClassName skipwhite
syn match scalaDefName "[^ =:;([]\+" contained nextgroup=scalaDefSpecializer skipwhite
syn match scalaValName "[^ =:;([]\+" contained
syn match scalaVarName "[^ =:;([]\+" contained 
syn match scalaClassName "[^ =:;(\[]\+" contained nextgroup=scalaClassSpecializer skipwhite
syn region scalaDefSpecializer start="\[" end="\]" contained contains=scalaDefSpecializer
syn region scalaClassSpecializer start="\[" end="\]" contained contains=scalaClassSpecializer

" type constructor (actually anything with an uppercase letter)
syn match scalaConstructor "\<[A-Z][_$a-zA-Z0-9]*\>" nextgroup=scalaConstructorSpecializer
syn region scalaConstructorSpecializer start="\[" end="\]" contained contains=scalaConstructorSpecializer

" method call
syn match scalaRoot "\<[a-zA-Z][_$a-zA-Z0-9]*\."me=e-1
syn match scalaMethodCall "\.[a-z][_$a-zA-Z0-9]*"ms=s+1

" type declarations in val/var/def
syn match scalaType ":\s*\(=>\s*\)\?[._$a-zA-Z0-9]\+\(\[[^]]*\]\+\)\?\(\s*\(<:\|>:\|#\|=>\)\s*[._$a-zA-Z0-9]\+\(\[[^]]*\]\+\)*\)*"ms=s+1

" comments
syn match scalaTodo "[tT][oO][dD][oO]" contained
syn match scalaLineComment "//.*" contains=scalaTodo
syn region scalaComment start="/\*" end="\*/" contains=scalaTodo
syn case ignore
syn include @scalaHtml syntax/html.vim
unlet b:current_syntax
syn case match
syn region scalaDocComment start="/\*\*" end="\*/" contains=scalaDocTags,scalaTodo,@scalaHtml keepend
syn region scalaDocTags start="{@\(link\|linkplain\|inherit[Dd]oc\|doc[rR]oot\|value\)" end="}" contained
syn match scalaDocTags "@[a-z]\+" contained

syn match scalaEmptyString "\"\""

" multi-line string literals
syn region scalaMultiLineString start="\"\"\"" end="\"\"\"" contains=scalaUnicode
syn match scalaUnicode "\\u[0-9a-fA-F]\{4}" contained

" string literals with escapes
syn region scalaString start="\"[^"]" skip="\\\"" end="\"" contains=scalaStringEscape " TODO end \n or not?
syn match scalaStringEscape "\\u[0-9a-fA-F]\{4}" contained
syn match scalaStringEscape "\\[nrfvb\\\"]" contained

" symbol and character literals
syn match scalaSymbol "'[_a-zA-Z0-9][_a-zA-Z0-9]*\>"
syn match scalaChar "'[^'\\]'\|'\\.'\|'\\u[0-9a-fA-F]\{4}'"

" number literals
syn match scalaNumber "\<\(0[0-7]*\|0[xX]\x\+\|\d\+\)[lL]\=\>"
syn match scalaNumber "\(\<\d\+\.\d*\|\.\d\+\)\([eE][-+]\=\d\+\)\=[fFdD]\="
syn match scalaNumber "\<\d\+[eE][-+]\=\d\+[fFdD]\=\>"
syn match scalaNumber "\<\d\+\([eE][-+]\=\d\+\)\=[fFdD]\>"

" xml literals
syn match scalaXmlTag "<[a-zA-Z]\_[^>]*/>" contains=scalaXmlQuote,scalaXmlEscape,scalaXmlString
syn region scalaXmlString start="\"" end="\"" contained
syn match scalaXmlStart "<[a-zA-Z]\_[^>]*>" contained contains=scalaXmlQuote,scalaXmlEscape,scalaXmlString
syn region scalaXml start="<\([a-zA-Z]\_[^>]*\_[^/]\|[a-zA-Z]\)>" matchgroup=scalaXmlStart end="</\_[^>]\+>" contains=scalaXmlEscape,scalaXmlQuote,scalaXml,scalaXmlStart,scalaXmlComment
syn region scalaXmlEscape matchgroup=scalaXmlEscapeSpecial start="{" matchgroup=scalaXmlEscapeSpecial end="}" contained contains=TOP
syn match scalaXmlQuote "&[^;]\+;" contained
syn match scalaXmlComment "<!--\_[^>]*-->" contained

syn sync fromstart

" map Scala groups to standard groups
hi link scalaKeyword Keyword
hi link scalaPackage Include
hi link scalaImport Include
hi link scalaBoolean Boolean
hi link scalaOperator Normal
hi link scalaNumber Number
hi link scalaEmptyString String
hi link scalaString String
hi link scalaChar String
hi link scalaMultiLineString String
hi link scalaStringEscape Special
hi link scalaSymbol Special
hi link scalaUnicode Special
hi link scalaComment Comment
hi link scalaLineComment Comment
hi link scalaDocComment Comment
hi link scalaDocTags Special
hi link scalaTodo Todo
hi link scalaType Type
hi link scalaTypeSpecializer scalaType
hi link scalaXml String
hi link scalaXmlTag Include
hi link scalaXmlString String
hi link scalaXmlStart Include
hi link scalaXmlEscape Normal
hi link scalaXmlEscapeSpecial Special
hi link scalaXmlQuote Special
hi link scalaXmlComment Comment
hi link scalaDef Keyword
hi link scalaVar Keyword
hi link scalaVal Keyword
hi link scalaClass Keyword
hi link scalaObject Keyword
hi link scalaTrait Keyword
hi link scalaDefName Function
hi link scalaDefSpecializer Function
hi link scalaClassName Special
hi link scalaClassSpecializer Special
hi link scalaConstructor Special
hi link scalaConstructorSpecializer scalaConstructor

let b:current_syntax = "scala"

" you might like to put these lines in your .vimrc
"
" customize colors a little bit (should be a different file)
" hi scalaNew gui=underline
" hi scalaMethodCall gui=italic
" hi scalaValName gui=underline
" hi scalaVarName gui=underline
