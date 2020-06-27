"================================================================
"  Author: F. Shen (shen@fshen.org)
"================================================================

"===============================
" 1) mkdir ~/.vim/bundle
" 2) git clone https://github.com/gmarik/Vundle.vim.git ~/.vim/bundle/Vundle.vim
" 3) in vim, :PluginInstall
"===============================

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
set noswapfile

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

set nofoldenable " disable folding

" 缩进
set expandtab  " 将 tab 自动转成空格; 使用 Ctrl + V + Tab 输入真的 tab"
set smarttab

set shiftwidth=4
au FileType cpp,c,hpp,h,R,Rmd,r,rmd setl shiftwidth=2
au FileType html,htm,css setl shiftwidth=2
set autoindent  " 换行时自动缩进
set smartindent
set cindent

set wrap

au BufRead,BufNewFile *.md setf markdown
au BufRead,BufNewFile *.Rmd setf rmd
au BufRead,BufNewFile *.jl setf julia
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

"===============================
" 插件设置

"===============================
" => Vundle
"===============================
filetype off

set rtp+=~/.vim/bundle/Vundle.vim
"call vundle#begin()
call vundle#rc()

Plugin 'gmarik/Vundle.vim'

"===============================

Plugin 'altercation/vim-colors-solarized'
syntax enable
set background=dark
set laststatus=2
set t_Co=256
"colorscheme zellner
"colorscheme solarized
"colorscheme pablo

Plugin 'tomasr/molokai'
let g:molokai_original = 1
let g:rehash256 = 1
colorscheme molokai
hi Normal ctermfg=254
hi Visual term=reverse ctermfg=232 ctermbg=220
hi Comment term=bold ctermfg=247
hi LineNr ctermfg=250 ctermbg=236
"hi PreProc term=bold ctermfg=227
"hi Function ctermfg=86
hi Delimiter ctermfg=254

Plugin 'chriskempson/vim-tomorrow-theme'
"colorscheme Tomorrow-Night-Bright
"colorscheme Tomorrow-Night
"colorscheme Tomorrow-Night-Eighties

" 保持背景的透明效果
hi Normal ctermbg=NONE
"===============================

" 文件树形结构
Plugin 'scrooloose/nerdtree'
map <leader>tt :NERDTreeToggle<CR>
let NERDTreeIgnore=['\.pyc', '\.git', '\.svn', '\.o']

Plugin 'majutsushi/tagbar'
nnoremap <silent> <F9> :TagbarToggle<CR>

" 文件查找
Plugin 'kien/ctrlp.vim'
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

Plugin 'vim-airline/vim-airline'
Plugin 'vim-airline/vim-airline-themes'
let g:airline_theme = 'bubblegum'
let g:airline_powerline_fonts = 1
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

"===============================

Plugin 'Lokaltog/vim-easymotion'

Plugin 'scrooloose/nerdcommenter'

Plugin 'Raimondi/delimitMate'

Plugin 'tpope/vim-surround'
Plugin 'tpope/vim-repeat'

Plugin 'https://github.com/jalvesaq/R-Vim-runtime.git'

Plugin 'Vim-R-plugin'
let vimrplugin_assign = 0

Plugin 'gerw/vim-latex-suite'

Plugin 'matchit.zip'

"===============================
" 代码检查
"===============================
Plugin 'scrooloose/syntastic'
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0
let g:syntastic_python_checkers = ['pyflakes']

let g:syntastic_cpp_compiler_options = ' -std=c++11'
let g:syntastic_cpp_check_header = 1
let g:syntastic_cpp_remove_include_errors = 1
"let g:syntastic_cpp_auto_refresh_includes = 1

Plugin 'hdima/python-syntax'

Plugin 'JuliaEditorSupport/julia-vim'

Plugin 'godlygeek/tabular'
Plugin 'plasticboy/vim-markdown'
"Plugin 'jtratner/vim-flavored-markdown'
"hi def link markdownItalic String

"Plugin 'vim-pandoc/vim-pandoc'
"Plugin 'vim-pandoc/vim-pandoc-syntax'
"let g:pandoc#syntax#conceal#use = 0

"===============================

Plugin 'tpope/vim-fugitive'
set statusline+=%{fugitive#statusline()}

"===============================


" (TODO)
"Plugin 'Valloric/YouCompleteMe'

" end of plugin config
"===============================

"call vundle#end()
filetype plugin indent on

