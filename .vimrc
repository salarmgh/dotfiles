set nocompatible              " be iMproved, required
filetype off                  " required

set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
Plugin 'VundleVim/Vundle.vim'
Plugin 'morhetz/gruvbox'
Plugin 'fatih/vim-go'
Plugin 'tpope/vim-fugitive'
Plugin 'w0rp/ale'
Plugin 'ctrlpvim/ctrlp.vim'
Plugin 'Raimondi/delimitMate'
Plugin 'airblade/vim-gitgutter'
Plugin 'pangloss/vim-javascript'
Plugin 'mxw/vim-jsx'
Plugin 'tpope/vim-surround'
Plugin 'leafgarland/typescript-vim'
Plugin 'MaxMEllon/vim-jsx-pretty'
Plugin 'prettier/vim-prettier'
Plugin 'AndrewRadev/splitjoin.vim'
Plugin 'SirVer/ultisnips'
Plugin 'fatih/molokai'
Plugin 'preservim/nerdcommenter'
call vundle#end()            " required
set t_Co=256
syntax on
colorscheme gruvbox
filetype plugin indent on
"set background=dark
set number
set laststatus=2
set splitright
set statusline=%f%3(%m%)%r%h%w[%{&ff}]%{fugitive#statusline()}%=[%4lL,%2vC\ %P]
set nocompatible
set confirm
set hidden
set secure
set autoread
set nomodeline
set backspace=indent,eol,start
set smarttab
set virtualedit+=block
set hlsearch
set incsearch
set ignorecase
set smartcase
set tabstop=4
set shiftwidth=4
set expandtab
set autoindent
set formatoptions+=j

nnoremap <Space> <nop>
let mapleader=' '


augroup indentation
    autocmd!
    autocmd Filetype lua        setlocal tabstop=2 shiftwidth=2
    autocmd Filetype ruby       setlocal tabstop=2 shiftwidth=2
    autocmd Filetype html*      setlocal tabstop=2 shiftwidth=2
    autocmd Filetype vue        setlocal tabstop=2 shiftwidth=2
    autocmd Filetype yaml       setlocal tabstop=2 shiftwidth=2
    autocmd Filetype javascript setlocal tabstop=2 shiftwidth=2

    autocmd Filetype c   setlocal tabstop=4 shiftwidth=4 noexpandtab
    autocmd Filetype cpp setlocal tabstop=4 shiftwidth=4 noexpandtab
    autocmd Filetype go  setlocal tabstop=4 shiftwidth=4 noexpandtab
augroup END

augroup spelling
    autocmd!
    autocmd Filetype text      setlocal spell   textwidth=79
    autocmd Filetype markdown  setlocal spell   textwidth=79
    autocmd FileType gitcommit setlocal spell
    autocmd FileType help      setlocal nospell
augroup END

augroup filetypes
    autocmd!
    autocmd BufNewFile,BufReadPost *.md        setlocal filetype=markdown
    autocmd BufNewFile,BufReadPost Vagrantfile setlocal filetype=ruby
    autocmd BufNewFile,BufReadPost *.sls       setlocal filetype=yaml
augroup END

" Ctrlp
let g:ctrlp_cmd = 'CtrlP'
let g:ctrlp_map = '<leader>f'
let g:ctrlp_open_new_file = 'r'
let g:ctrlp_open_multiple_files = 'i'
let g:ctrlp_match_window = 'bottom,order:btt,min:1,max:10,results:100'
nnoremap <leader>b :CtrlPBuffer<CR>
let g:ctrlp_user_command = {
\ 'types': {
  \ 1: ['.git', 'cd %s && git ls-files -co --exclude-standard'],
  \ },
\ 'fallback': 'find %s -type f',
\ }
let g:ctrlp_use_caching = 0
if executable('ag')
  let g:ctrlp_user_command['fallback'] = 'ag %s -l --nocolor -g ""'
endif

" Delimate
let delimitMate_expand_cr = 1
autocmd FileType python let b:delimitMate_nesting_quotes = ['"']
autocmd FileType elixir let b:delimitMate_nesting_quotes = ['"']
autocmd FileType scheme let b:delimitMate_quotes = "\""

" prettier
let g:prettier#autoformat = 0
autocmd BufWritePre *.js,*.jsx,*.mjs,*.ts,*.tsx,*.css,*.less,*.scss,*.json,*.md,*.yaml,*.html PrettierAsync
" GitGutter
let g:gitgutter_terminal_reports_focus = 0

runtime! ftplugin/man.vim
runtime! macros/matchit.vim

nnoremap / mm/
nnoremap ? mm?

set autowrite

map <C-n> :cnext<CR>
map <C-m> :cprevious<CR>
nnoremap <leader>a :cclose<CR>

autocmd FileType go nmap <leader>r  <Plug>(go-run)
autocmd FileType go nmap <leader>t  <Plug>(go-test)

" run :GoBuild or :GoTestCompile based on the go file
function! s:build_go_files()
  let l:file = expand('%')
  if l:file =~# '^\f\+_test\.go$'
    call go#test#Test(0, 1)
  elseif l:file =~# '^\f\+\.go$'
    call go#cmd#Build(0)
  endif
endfunction

autocmd FileType go nmap <leader>, :<C-u>call <SID>build_go_files()<CR>
autocmd FileType go nmap <Leader>c <Plug>(go-coverage-toggle)
autocmd FileType go nmap <leader>d :GoDoc<Return>
"autocmd FileType go nmap <leader>\ :GoDef<Return>

let g:go_fmt_command = "goimports"


" Trigger configuration. Do not use <tab> if you use https://github.com/Valloric/YouCompleteMe.
let g:UltiSnipsExpandTrigger="<tab>"
let g:UltiSnipsJumpForwardTrigger="<c-b>"
let g:UltiSnipsJumpBackwardTrigger="<c-z>"

" If you want :UltiSnipsEdit to split your window.
let g:UltiSnipsEditSplit="vertical"


let g:go_highlight_types = 1
let g:go_highlight_fields = 1
let g:go_highlight_functions = 1
let g:go_highlight_function_calls = 1
let g:go_highlight_operators = 1
let g:go_highlight_extra_types = 1
autocmd BufNewFile,BufRead *.go setlocal noexpandtab tabstop=4 shiftwidth=4 
let g:go_metalinter_enabled = ['vet', 'golint', 'errcheck']
let g:go_metalinter_autosave = 1
let g:go_metalinter_autosave_enabled = ['vet', 'golint', 'errcheck']
let g:go_metalinter_deadline = "5s"

" Add spaces after comment delimiters by default
let g:NERDSpaceDelims = 1

" Use compact syntax for prettified multi-line comments
let g:NERDCompactSexyComs = 1

" Align line-wise comment delimiters flush left instead of following code indentation
let g:NERDDefaultAlign = 'left'

" Set a language to use its alternate delimiters by default
let g:NERDAltDelims_java = 1

" Add your own custom formats or override the defaults
let g:NERDCustomDelimiters = { 'c': { 'left': '/**','right': '*/' } }

" Allow commenting and inverting empty lines (useful when commenting a region)
let g:NERDCommentEmptyLines = 1

" Enable trimming of trailing whitespace when uncommenting
let g:NERDTrimTrailingWhitespace = 1

" Enable NERDCommenterToggle to check all selected lines is commented or not
let g:NERDToggleCheckAllLines = 1

:command WQ wq
:command Wq wq
:command W w
:command Q q

autocmd FileType go nnoremap <leader>r :GoRun<CR>
