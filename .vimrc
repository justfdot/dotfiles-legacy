syntax on
set number
set relativenumber
set cursorline
set showcmd
set noshowmode
set mouse=a
set clipboard+=unnamedplus

set nocompatible
filetype off
set hlsearch
" set splitbelow
set splitright
set noequalalways   "Do not change windows size after close one
set autowrite

set linebreak       "Wrap lines by words
set expandtab       "Use softtabstop spaces instead of tab characters for indentation
set shiftwidth=2    "Indent by 2 spaces when using >>, <<, == etc.
set softtabstop=2   "Indent by 2 spaces when pressing <TAB>
"set autoindent      "Keep indentation from previous line
"set smartindent     "Automatically inserts indentation in some cases
"set cindent         "Like smartindent, but stricter and more customisable

if has ("autocmd")
  " File type detection. Indent based on filetype. Recommended.
  filetype plugin indent on
endif

set path+=**
command! MakeTags !ctags -R .

" игнорировать регистр при поиске
set ic
" по звездочке не прыгать на следующее найденное, а просто подсветить
nnoremap * *N
" в визуальном режиме по команде * подсвечивать выделение
vnoremap * y :execute ":let @/=@\""<CR> :execute "set hlsearch"<CR>
" включить/отключить подсветку вхождений в регистре /
noremap <F9> :set hlsearch! hlsearch?<CR>
" jump to normal mode
imap df <Esc>
" add empty line from normal mode
nmap <CR> o<Esc>
" highlight last inserted text
nnoremap gV `[v`]
" swap colon and semicolon
nnoremap ; :
nnoremap : ;


set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
  " let Vundle manage Vundle, required
  Plugin 'gmarik/Vundle.vim'

  " Plugin 'morhetz/gruvbox'
  Plugin 'chriskempson/base16-vim'

  Plugin 'bling/vim-airline'
  Plugin 'vim-airline/vim-airline-themes'
  " Plugin 'easymotion/vim-easymotion'
  " Plugin 'tpope/vim-sensible'
  Plugin 'tpope/vim-commentary'
  Plugin 'tpope/vim-surround'
  Plugin 'jeetsukumaran/vim-buffergator'
  Plugin 'scrooloose/nerdtree'
  Plugin 'junegunn/vim-pseudocl'
  Plugin 'junegunn/vim-oblique'

  Plugin 'scrooloose/syntastic'
  " Plugin 'Valloric/YouCompleteMe'
  Plugin 'fboender/bexec'

  " Plugin 'vim-ruby/vim-ruby'
  " Plugin 'tpope/vim-rails'
  " Plugin 'tpope/vim-bundler'
  Plugin 'kchmck/vim-coffee-script'
call vundle#end()            " required

let mapleader=','

set termguicolors
let g:gruvbox_italic=1
" colorscheme gruvbox
colorscheme base16-tomorrow-night
set background=dark    " Setting dark mode

if filereadable(expand("~/.vimrc_background"))
  let base16colorspace=256
  source ~/.vimrc_background
endif


" Buffergator plugin settings
let g:buffergator_viewport_split_policy = "B"
let g:buffergator_sort_regime = "mru"


" Buffer Executor plugin settings
let bexec_splitsize = 5
nmap <C-b>b :Bexec()<CR>
vmap <C-b>b :BexecVisual()<CR>


" EasyMotion plugin settings
" nmap f <Plug>(easymotion-s)
" Turn on case insensitive feature
" let g:EasyMotion_smartcase = 1


" Airline plugin settings
set laststatus=2
let g:airline_left_sep = ''
let g:airline_right_sep = ''
let g:airline#extensions#tabline#enabled = 1
" don't count trailing whitespace since it lags in huge files
let g:airline#extensions#whitespace#enabled = 0
" disable to improve fugitive performance
let g:airline#extensions#branch#enabled = 0
" put a buffer list at the top
let g:airline#extensions#tabline#fnamemod = ':t'
let g:airline#extensions#tabline#left_sep = ''
let g:airline_symbols_ascii = 1


" YouCompleteMe plugin settings
let g:ycm_autoclose_preview_window_after_completion = 1
let g:ycm_key_list_select_completion = ['<TAB>', '<Enter>']

nnoremap <M-l> :bnext<CR>
nnoremap <M-h> :bprevious<CR>


" NERDTree plugin settings
nmap <leader>/ :NERDTreeFind<CR>
nmap <leader>n :NERDTreeToggle<CR>


" Syntastic plugin settings
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*
let g:syntastic_always_populate_loc_list = 0
let g:syntastic_auto_loc_list = 0
let g:syntastic_aggregate_errors = 1
let g:syntastic_check_on_wq = 0
let g:syntastic_python_checkers = ['flake8']

let g:syntastic_warning_symbol = '✗➜'
let g:syntastic_error_symbol = '✗➜'
let g:syntastic_style_error_symbol = '✓➜'
let g:syntastic_style_warning_symbol = '✓➜'


" Map ctrl-movement keys to window switching
map <C-j> <C-w><Down>
map <C-k> <C-w><Up>
map <C-l> <C-w><Right>
map <C-h> <C-w><Left>

" ================ Persistent Undo ==================
" Keep undo history across sessions, by storing in file.
" Only works all the time.
if has('persistent_undo')
  silent !mkdir ~/.vim/backups > /dev/null 2>&1
  set undodir=~/.vim/backups
  set undofile
endif
