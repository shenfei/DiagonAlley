"================================================================
"  Author: SHEN Fei (shenfei361@gmail.com)
"================================================================

"===============================
" => General
"===============================
set nocompatible

" 显示行号
set number

syntax on
set mouse=a
set history=500

" 设置备份
set backupext=.bak
set backupdir=~/.vim_bak/

filetype plugin indent on

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

" 缩进
set expandtab  " 将 tab 自动转成空格; 使用 Ctrl + V + Tab 输入真的 tab"
set smarttab

set shiftwidth=4
au FileType cpp,c,hpp,h setl shiftwidth=2
set autoindent  " 换行时自动缩进
set smartindent
set cindent

set wrap

"===============================
" 一些自定义的 map
"===============================
map <space> /
map Y y$

nnoremap <leader>s :w<CR>
inoremap <leader>s <esc>:w<CR>

nnoremap <leader>nh :noh<CR>

"===============================
" 插件设置

"===============================
" => Vundle
"===============================
filetype off

set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

Bundle 'gmarik/vundle'

"===============================

"Bundle 'altercation/vim-colors-solarized'
syntax enable
set background=dark
set laststatus=2
set t_Co=256
"colorscheme zellner
"colorscheme solarized
"colorscheme pablo

Bundle 'tomasr/molokai'
let g:molokai_original = 1
colorscheme molokai
hi Visual term=reverse ctermfg=232 ctermbg=226
hi Comment term=bold ctermfg=243
hi PreProc term=bold ctermfg=227
hi Function ctermfg=86

" 保持背景的透明效果
hi Normal ctermbg=NONE
"===============================

" 文件树形结构
Bundle 'scrooloose/nerdtree'
map <leader>tt :NERDTreeToggle<CR>
let NERDTreeIgnore=['\.pyc', '\.git', '\.svn', '\.o']

Bundle 'majutsushi/tagbar'
nnoremap <silent> <F9> :TagbarToggle<CR>

" 文件查找
Bundle 'kien/ctrlp.vim'
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

Bundle 'bling/vim-airline'
let g:airline_theme = 'bubblegum'
let g:airline_section_c = '%{getcwd()}/%t'
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

"===============================

Bundle 'Lokaltog/vim-easymotion'

Bundle 'scrooloose/nerdcommenter'

Bundle 'Raimondi/delimitMate'

Bundle 'tpope/vim-surround'
Bundle 'tpope/vim-repeat'

Bundle 'Vim-R-plugin'

Bundle 'matchit.zip'

"===============================
" 代码检查
"===============================
Bundle 'scrooloose/syntastic'
let g:syntastic_python_checkers = ['pyflakes']

Bundle 'kevinw/pyflakes-vim'
let g:pyflakes_use_quickfix = 0

Bundle 'hdima/python-syntax'

Bundle 'plasticboy/vim-markdown'

"===============================

Bundle 'tpope/vim-fugitive'
set statusline+=%{fugitive#statusline()}

"===============================

Bundle 'git://git.code.sf.net/p/vim-latex/vim-latex'
let g:Tex_ViewRule_pdf = 'evince'
"let g:tex_flavor = 'xelatex'

"===============================

" (TODO)
"Bundle 'Valloric/YouCompleteMe'

" end of plugin config
"===============================

filetype plugin indent on
