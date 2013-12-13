" Vundle     "
" ========== "
set nocompatible
filetype off

set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

Bundle 'gmarik/vundle'

Bundle 'Raimondi/delimitMate'
Bundle 'Valloric/YouCompleteMe'
Bundle 'scrooloose/nerdcommenter'
Bundle 'bling/vim-airline'
Bundle 'flazz/vim-colorschemes'
Bundle 'derekwyatt/vim-scala'
Bundle 'lambdatoast/elm.vim'

Bundle 'Shougo/vimproc.vim'
Bundle 'Shougo/unite.vim'

" Config     "
" ========== "

"" Eclim + YCM
let g:EclimCompletionMethod = 'omnifunc'

set mouse=a

syntax on
filetype plugin indent on

"" Tabs
set smarttab
set expandtab
set tabstop=2
set shiftwidth=2
set softtabstop=2

"" Relative Line numbers
set rnu

"" Show status bar
set laststatus=2

set visualbell ""No error sound

"" Show full menu
set wildmenu
set wildmode=full

"" Move as you type, wrapped-search
set incsearch
set wrapscan

"" Case insensitive search for lowercase, case sensitive for upper
"" http://stackoverflow.com/a/2288438
set ignorecase
set smartcase

set history=9999

"" Backup settings
set backup		            " keep a backup file
set backupdir=~/.vim/backup	" backup files centralized

"" Show cursor
set cursorline

"" Set ruler
set cc=80
set ruler

"" Set a title on xterm systems
set title

"" Allow backspacing over everything in insert mode
set backspace=indent,eol,start

"" Unite
let g:unite_source_history_yank_enable = 1
call unite#filters#matcher_default#use(['matcher_fuzzy'])
nnoremap <C-p> :Unite file_rec/async -start-insert -no-split<cr>
nnoremap <C-f> :Unite line -start-insert -no-split<cr>
nnoremap <C-o> :Unite file -start-insert -no-split<cr>
nnoremap <C-e> :Unite buffer -quick-match -no-split<cr>

"" Move around with Ctrl
map <C-k> <C-w><Up>
map <C-j> <C-w><Down>
map <C-l> <C-w><Right>
map <C-h> <C-w><Left>

map <C-Tab> :bnext<cr>
map <C-S-Tab> :bprevious<cr>

map <C-s> :w<cr>

"" NERDComment
noremap <silent> <C-u> :call NERDComment(1, 'toggle')<CR>

"" :W asks for sudo pw
command! W w !sudo tee % > /dev/null

"" Strip GVIM
set guioptions=ac
set guifont=Source\ Code\ Pro\ ExtraLight\ 10
colors github
