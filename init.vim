"================================================================
"  Author: SHEN Fei (f@mathehacker.com)
"================================================================

"===============================
" 插件设置
" First install https://github.com/junegunn/vim-plug

"===============================

call plug#begin('~/.local/share/nvim/plugged')

Plug 'tomasr/molokai'
" 文件树形结构
Plug 'scrooloose/nerdtree', { 'on':  'NERDTreeToggle' }
Plug 'majutsushi/tagbar'
" 文件查找
Plug 'kien/ctrlp.vim'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'Lokaltog/vim-easymotion'
Plug 'scrooloose/nerdcommenter'
Plug 'Raimondi/delimitMate'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-repeat'
Plug 'https://github.com/jalvesaq/R-Vim-runtime.git'
Plug 'jalvesaq/Nvim-R'
Plug 'gerw/vim-latex-suite'
" 代码检查
Plug 'vim-syntastic/syntastic'
Plug 'hdima/python-syntax'
Plug 'JuliaEditorSupport/julia-vim'
Plug 'godlygeek/tabular'
Plug 'plasticboy/vim-markdown'
Plug 'tpope/vim-fugitive'

call plug#end()
"===============================


"===============================
" => General
"===============================
" 显示行号
set number

syntax on
set mouse=a
set history=1000

set noswapfile

"===============================

"set autoread  " 文件修改后自动载入
"autocmd! bufwritepost .vimrc source %  " vimrc文件修改之后自动加载

set ffs=unix,dos,mac

scriptencoding utf-8
set encoding=utf-8
set fileencodings=utf-8,gbk,gb2312,big5,latin-1
set termencoding=utf-8

"===============================

let mapleader = ","
let g:mapleader = ","

"set cursorline  " 突出显示当前行
set guicursor=

" Set 7 lines to the cursor - when moving vertically
set scrolloff=7

set wildmenu

set ruler  " 显示当前行列号

set showcmd

set hidden

set backspace=indent,eol,start
set whichwrap+=<,>,h,l

" 搜索设置
set ignorecase  " 搜索时忽略大小写
set incsearch   " 键入时即时搜索
set smartcase   " 有大写字母时大小写敏感
set hlsearch    " 高亮搜索结果

set magic

" 去掉提示音
set noerrorbells
set novisualbell
set t_vb=

set showmatch
set mat=2

set nofoldenable " disable folding

" 缩进
set expandtab  " 将 tab 自动转成空格; 使用 Ctrl + V + Tab 输入真的 tab"
set smarttab

set shiftwidth=4
au FileType cpp,c,hpp,h,R,Rmd,r,rmd setl shiftwidth=2
au FileType html,htm,css setl shiftwidth=2
au FileType yaml,yml setl shiftwidth=2
set autoindent  " 换行时自动缩进
set smartindent
set cindent

set wrap

au BufRead,BufNewFile *.md setf markdown
au BufRead,BufNewFile *.Rmd setf rmd
au BufRead,BufNewFile *.dockerfile setf dockerfile
"augroup markdown
    "au!
    "au BufNewFile,BufRead *.md,*.markdown setlocal filetype=ghmarkdown
"augroup END
autocmd filetype crontab setlocal nobackup nowritebackup

"===============================
" 一些自定义的 map
"===============================
map <space> /
map Y y$

nnoremap <leader>s :w<CR>
inoremap <leader>s <esc>:w<CR>
cnoremap Q<CR> q<CR>

nnoremap <leader>nh :noh<CR>
nnoremap <leader>1 1gt<CR>
nnoremap <leader>2 2gt<CR>
nnoremap <leader>3 3gt<CR>
nnoremap <leader>4 4gt<CR>
nnoremap <leader>5 5gt<CR>

nnoremap <leader>mr :vertical resize +15<CR>

set termguicolors
set laststatus=2
let g:molokai_original = 1
let g:rehash256 = 1
colorscheme molokai
"hi Normal guifg=#e4e4e4
hi Visual cterm=reverse ctermfg=232 ctermbg=220
hi Visual guifg=#080808 guibg=#ffd700
hi Comment guifg=#949494 gui=italic
hi LineNr ctermfg=250 ctermbg=236
"hi PreProc term=bold ctermfg=227
"hi Function ctermfg=86
hi Delimiter ctermfg=254
hi Delimiter guifg=#e4e4e4

"===============================
" begin of plugin config
"===============================

map <leader>tt :NERDTreeToggle<CR>
let NERDTreeIgnore=['\.pyc', '\.git', '\.svn', '\.o']

let g:NERDDefaultAlign = 'left'

nnoremap <silent> <F9> :TagbarToggle<CR>


let g:ctrlp_map = '<leader>p'
let g:ctrlp_cmd = 'CtrlP'
map <leader>f: CtrlPMRU<CR>
let g:ctrlp_working_path_mode = 'ra'
let g:ctrlp_custom_ignore = {
    \ 'dir': '\v[\/]\.(git|hg|svn|rvm)$',
    \ 'file': '\v\.(exe|so|dll|pyc|zip|tar|tar.gz)$',
    \ }
let g:ctrlp_max_height = 15
let g:ctrlp_follow_symlinks = 1


let g:airline_theme = 'bubblegum'
let g:airline_powerline_fonts = 1
"let g:airline#extensions#tabline#enabled = 1
"let g:airline_section_c = '%{getcwd()}/%t'
let g:airline_section_c = airline#section#create(['%<', '%{getcwd()}', '/', 'file', ' ', 'readonly'])
let g:airline_section_z = '%l/%L:%c'
" 设置宽度大于多少时才显示某section
let g:airline#extensions#default#section_truncate_width = {
    \ 'a': 50,
    \ 'b': 60,
    \ 'gutter': 80,
    \ 'x': 80,
    \ 'y': 70,
    \ 'warning': 80,
    \ }


let vimrplugin_assign = 0


let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0
let g:syntastic_python_checkers = ['flake8']

let g:syntastic_cpp_compiler_options = ' -std=c++11'
let g:syntastic_cpp_check_header = 1
let g:syntastic_cpp_remove_include_errors = 1
"let g:syntastic_cpp_auto_refresh_includes = 1


set statusline+=%{fugitive#statusline()}

"===============================
" end of plugin config
"===============================
