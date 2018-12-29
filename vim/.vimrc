set nocompatible               " be iMproved

" Specify a directory for plugins
" - For Neovim: ~/.local/share/nvim/plugged
" - Avoid using standard Vim directory names like 'plugin'
call plug#begin('~/.vim/plugged')
" Bash
 Plug 'vim-scripts/bash-support.vim'


" Bracket autocomplete
 Plug 'jiangmiao/auto-pairs'
 Plug 'rakr/vim-one'

 Plug 'itchyny/lightline.vim'
 Plug 'artur-shaik/vim-javacomplete2'
 Plug 'rhysd/committia.vim'
 Plug 'lervag/vimtex'
 Plug 'JuliaEditorSupport/julia-vim'
 Plug 'scrooloose/nerdtree'
 "Plug 'nvie/vim-flake8'
 Plug 'Xuyuanp/nerdtree-git-plugin'

 Plug 'Valloric/YouCompleteMe', { 'do': './install.py' }

 "autosave
 Plug 'vim-scripts/vim-auto-save'

 Plug 'JuliaEditorSupport/julia-vim'

 Plug 'davidhalter/jedi-vim'

" Initialize plugin system
call plug#end()

filetype plugin indent on    " required

"Credit joshdick
"Use 24-bit (true-color) mode in Vim/Neovim when outside tmux.
"If you're using tmux version 2.2 or later, you can remove the outermost $TMUX check and use tmux's 24-bit color support
"(see < http://sunaku.github.io/tmux-24bit-color.html#usage > for more information.)
if (empty($TMUX))
  if (has("nvim"))
  "For Neovim 0.1.3 and 0.1.4 < https://github.com/neovim/neovim/pull/2198 >
  let $NVIM_TUI_ENABLE_TRUE_COLOR=1
  endif
  "For Neovim > 0.1.5 and Vim > patch 7.4.1799 < https://github.com/vim/vim/commit/61be73bb0f965a895bfb064ea3e55476ac175162 >
  "Based on Vim patch 7.4.1770 (`guicolors` option) < https://github.com/vim/vim/commit/8a633e3427b47286869aa4b96f2bfc1fe65b25cd >
  " < https://github.com/neovim/neovim/wiki/Following-HEAD#20160511 >
  if (has("termguicolors"))
    set termguicolors
  endif
endif

colorscheme one
let g:one_allow_italics=1 " I love italic for comments
set background=dark
let g:deoplete#enable_at_startup=1
set path=.,,**
syntax on
set expandtab
set tabstop=4
set softtabstop=2
set shiftwidth=2
set number
set showcmd
set cursorline
set wildmenu
set showmatch
set incsearch
set colorcolumn=88
autocmd FileType * setlocal formatoptions-=c formatoptions-=r formatoptions-=o

filetype plugin indent on  
set omnifunc=syntaxcomplete#Complete

autocmd FileType java setlocal omnifunc=javacomplete#Complete
let g:deoplete#enable_at_startup = 1
let g:deoplete#omni_patterns = {}
let g:deoplete#omni_patterns.java = '[^. *\t]\.\w*'
let g:deoplete#auto_completion_start_length = 2
let g:deoplete#sources = {}
let g:deoplete#sources._ = []
let g:deoplete#file#enable_buffer_path = 1
"set mouse=a

" Command to move among tabs in Konsole-style
map <A-Right> gt
map <A-Left> gT

"split navigations
nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>
nnoremap <C-H> <C-W><C-H>

" Lightline
let g:lightline = {
      \ 'colorscheme': 'one',
      \ }

" syntastic
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0

" nerdtree
map <C-n> :NERDTreeToggle<CR>
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif

let python_highlight_all=1

" Autosave
let g:auto_save = 1  " enable AutoSave on Vim startup
"let g:auto_save_no_updatetime = 1  " do not change the 'updatetime' option
"let g:auto_save_in_insert_mode = 0  " do not save while in insert mode

" Make jedi compatible with YCM
let g:jedi#completions_enabled = 0
