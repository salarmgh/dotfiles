set nocompatible              " be iMproved, required
filetype off                  " required

set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
Plugin 'VundleVim/Vundle.vim'
Plugin 'joshdick/onedark.vim'
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
call vundle#end()            " required
set t_Co=256
syntax on
colorscheme onedark
filetype plugin indent on
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
