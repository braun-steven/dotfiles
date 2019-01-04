set hidden
set nocompatible               " be iMproved
" Remove '-- INSERT --' line since it is shown in lighline anyway
set noshowmode

" Reload .vimrc on save
autocmd! bufwritepost .vimrc source %


" Disable arrows
let g:elite_mode=1

" Specify a directory for plugins
" - For Neovim: ~/.local/share/nvim/plugged
" - Avoid using standard Vim directory names like 'plugin'
call plug#begin('~/.vim/plugged')
 Plug 'lifepillar/vim-solarized8'

 " Comment/uncomment with gc
 Plug 'tpope/vim-commentary'
 "Plug 'ervandew/supertab'

" Linting
 Plug 'desmap/ale-sensible'
 Plug 'w0rp/ale'

 " Python formatting with :Black
 Plug 'ambv/black'

 " Python virtual envs
 Plug 'plytophogy/vim-virtualenv'

" FZF
 Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
 Plug 'junegunn/fzf.vim'

" Sensible vim config
 Plug 'tpope/vim-sensible'

" Bash
 Plug 'vim-scripts/bash-support.vim'

 " Git info on the sidebar
 Plug 'airblade/vim-gitgutter'

 " Tagbar with <F8>
 Plug 'majutsushi/tagbar'

 " CTag automation
 Plug 'ludovicchabant/vim-gutentags'

" Bracket autocomplete
 Plug 'jiangmiao/auto-pairs'

 Plug 'joshdick/onedark.vim'

 " Syntax highlighting
 Plug 'sheerun/vim-polyglot'

 " Statusline
 Plug 'itchyny/lightline.vim'

 " Git commit extension
 Plug 'rhysd/committia.vim'
 Plug 'JuliaEditorSupport/julia-vim'

 " File tree with <C-n>
 Plug 'scrooloose/nerdtree'
 Plug 'Xuyuanp/nerdtree-git-plugin'

 " Autocomplete framework
 Plug 'Valloric/YouCompleteMe', { 'do': './install.py' }

 Plug 'davidhalter/jedi-vim'

 " Utility Snippets
 " Track the engine.
 Plug 'SirVer/ultisnips'

 " " Snippets are separated from the engine
 Plug 'honza/vim-snippets'
 
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

" Set leader key to <space> 
:let mapleader = ' '

let g:onedark_termcolors=256
let g:onedark_terminal_italics=1
colorscheme solarized8
" colorscheme onedark 
set background=dark
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
set colorcolumn=120
autocmd FileType * setlocal formatoptions-=c formatoptions-=r formatoptions-=o

filetype plugin indent on  
set omnifunc=syntaxcomplete#Complete

autocmd FileType java setlocal omnifunc=javacomplete#Complete
"set mouse=a

"split navigations
nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>
nnoremap <C-H> <C-W><C-H>

" Lightline
let g:lightline = {
      \ 'active': {
      \   'left': [ [ 'mode', 'paste' ], [ 'readonly', 'relativepath', 'modified' ] ]
      \ },
      \ 'colorscheme': 'solarized',
      \ 'component_function': {
      \   'mode': 'LightlineMode'
      \ }
      \ }

" function! LightLineFilename()
"   return fnamemodify(expand("%"), ":~:.")
" endfunction

function! LightlineMode()
  return expand('%:t') ==# '__Tagbar__' ? 'Tagbar':
        \ expand('%:t') ==# 'ControlP' ? 'CtrlP' :
        \ &filetype ==# 'unite' ? 'Unite' :
        \ &filetype ==# 'vimfiler' ? 'VimFiler' :
        \ &filetype ==# 'vimshell' ? 'VimShell' :
        \ lightline#mode()
endfunction

" nerdtree
map <C-n> :NERDTreeToggle<CR>
map <C-m> :TagbarToggle<CR>
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif

let python_highlight_all=1

" Make jedi compatible with YCM
let g:jedi#completions_enabled = 0

" Vim tagbar toggle
nmap <F8> :TagbarToggle<CR>
let g:tagbar_type_julia = {
    \ 'ctagstype' : 'julia',
    \ 'kinds'     : [
        \ 't:struct', 'f:function', 'm:macro', 'c:const']
    \ }

" FZF
nmap ; :Buffers<CR>
nmap <Leader>t :Tags<CR>
nmap <Leader>f :Files<CR>


" Disable ycm extra conf question
let g:ycm_confirm_extra_conf = 0

" Disable arrow movement, resize splits instead.
if get(g:, 'elite_mode')
	nnoremap <Up>    :resize +2<CR>
	nnoremap <Down>  :resize -2<CR>
	nnoremap <Left>  :vertical resize +2<CR>
	nnoremap <Right> :vertical resize -2<CR>
endif

" Clear search with <C-L> 
nnoremap <esc> :noh<return><esc>

" Util snips config
" Trigger configuration. Do not use <tab> if you use https://github.com/Valloric/YouCompleteMe.
let g:UltiSnipsExpandTrigger="<c-x><c-c>"
let g:UltiSnipsJumpForwardTrigger="<c-b>"
let g:UltiSnipsJumpBackwardTrigger="<c-z>"

" Ale fixers
let g:ale_fixers = ['prettier', 'standard'] 
